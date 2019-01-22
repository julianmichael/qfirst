from typing import Dict, List, Optional

from overrides import overrides

import torch
from torch.nn import Sequential
from torch.nn.modules import Linear, Dropout, Sequential, ReLU
import torch.nn.functional as F

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, TimeDistributed, TextFieldEmbedder, Pruner
from allennlp.modules.span_extractors import EndpointSpanExtractor
from allennlp.modules.token_embedders import Embedding
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import batched_index_select, get_text_field_mask, sequence_cross_entropy_with_logits

from qfirst.modules.question_generator import QuestionGenerator
from qfirst.data.util import get_slot_label_namespace
from qfirst.util.question_conversion import get_question_tensors_for_clause_tensors_batched
from qfirst.metrics import E2EMetric, E2EPretrainingMetric

import math

from nrl.common.span import Span
from nrl.modules.span_rep_assembly import SpanRepAssembly

# should receive verb instances from the qasrl dataset reader
@Model.register("e2e_qasrl")
class E2EParser(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 stacked_encoder: Seq2SeqEncoder,
                 encoder_output_projected_dim: int = -1,
                 predicate_feature_dim: int = 128,
                 embedding_dropout: float = 0.0,
                 clause_hidden_dim: int = 128,
                 num_pruned_clauses: int = 4,
                 qarg_hidden_dim: int = 128,
                 span_hidden_dim: int = 128,
                 final_beam_size: int = 128,
                 final_embedding_dim: int = 128,
                 tan_hidden_dim: int = 128,
                 metric_thresholds: List[float] = [0.05, 0.15, 0.25, 0.4, 0.5, 0.6, 0.75, 0.85, 0.95],
                 is_pretraining: bool = False,
                 is_logging: bool = False,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(E2EParser, self).__init__(vocab, regularizer)
        self.text_field_embedder = text_field_embedder

        self.span_hidden_dim = span_hidden_dim
        self.clause_hidden_dim = clause_hidden_dim
        self.qarg_hidden_dim = qarg_hidden_dim
        self.tan_hidden_dim = tan_hidden_dim
        self.final_embedding_dim = final_embedding_dim

        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)
        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.num_pruned_clauses = num_pruned_clauses
        self.final_beam_size = final_beam_size

        self.clause_embedding = Embedding(vocab.get_vocab_size("abst-clause-labels"), final_embedding_dim)
        self.qarg_embedding = Embedding(vocab.get_vocab_size("qarg-labels"), final_embedding_dim)

        self.stacked_encoder = stacked_encoder
        if encoder_output_projected_dim > 0:
            self.encoder_output_projection = TimeDistributed(Linear(stacked_encoder.get_output_dim(), encoder_output_projected_dim))
            self.encoder_output_projected_dim = encoder_output_projected_dim
        else:
            self.encoder_output_projection = None
            self.encoder_output_projected_dim = stacked_encoder.get_output_dim()

        self.clause_scorer = Sequential(
            Linear(self.encoder_output_projected_dim, self.clause_hidden_dim), ReLU(),
            Linear(self.clause_hidden_dim, self.vocab.get_vocab_size("abst-clause-labels"))
        )
        self.qarg_scorer = Sequential(
            Linear(self.final_embedding_dim, self.qarg_hidden_dim), ReLU(),
            Linear(self.qarg_hidden_dim, self.qarg_hidden_dim), ReLU(),
            Linear(self.qarg_hidden_dim, self.vocab.get_vocab_size("qarg-labels"))
        )

        self.span_hidden = SpanRepAssembly(self.encoder_output_projected_dim, self.encoder_output_projected_dim, self.span_hidden_dim)
        self.span_scorer = TimeDistributed(Linear(self.span_hidden_dim, 1))
        self.span_pruner = Pruner(self.span_scorer)

        self.pred_final_hidden = Sequential(
            Linear(self.encoder_output_projected_dim, self.final_embedding_dim), ReLU()
        )
        self.span_final_hidden = Sequential(
            Linear(self.span_hidden_dim, self.final_embedding_dim), ReLU()
        )

        self.score_pruner = Pruner(lambda x: x)

        self.animacy_scorer = Linear(self.span_hidden_dim, 1)
        self.tan_scorer = Linear(self.encoder_output_projected_dim, self.vocab.get_vocab_size("tan-string-labels"))

        self.is_pretraining = is_pretraining
        self.is_logging = is_logging

        self.span_extractor = EndpointSpanExtractor(
            input_dim = self.span_hidden_dim,
            combination = "x,y"
        )

        if self.is_pretraining:
            self.metric = E2EPretrainingMetric(metric_thresholds)
        else:
            self.metric = E2EMetric(metric_thresholds)

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                clause_strings: torch.LongTensor = None,
                clause_set: torch.LongTensor = None,
                tan_strings: torch.LongTensor = None,
                qargs: torch.LongTensor = None,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                tan_set = None,
                animacy_spans = None,
                animacy_labels = None,
                metadata = None,
                qarg_pretrain_clauses = None,
                qarg_pretrain_spans = None,
                qarg_pretrain_labels = None,
                **kwargs):

        # Shape: batch_size, num_tokens, encoder_output_projected_dim
        encoded_text, text_mask = self._get_encoded_text(text, predicate_indicator)
        batch_size, num_tokens, _ = encoded_text.size()

        # The predicate representation is what feeds into the question-related classifiers.
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = batched_index_select(encoded_text, predicate_index).squeeze(1)

        # We make predictions of which clauses and answer spans are most likely to be used for this verb, and prune each into its own beam.
        clause_scores = self.clause_scorer(pred_rep)

        # Shapes: batch_size, num_pruned_clauses; batch_size, num_pruned_clauses, 1
        _, _, top_clause_indices, top_clause_scores = self.score_pruner(
            clause_scores.unsqueeze(-1),
            torch.ones([batch_size, self.vocab.get_vocab_size("abst-clause-labels")], dtype = torch.float64, device = pred_rep.device),
            self.num_pruned_clauses
        )

        # From the same encoded text, we also assemble a representation for every answer span.
        # Shapes: batch_size, num_spans, span_hidden_dim; batch_size, num_spans
        span_hidden, span_mask = self.span_hidden(encoded_text, encoded_text, text_mask, text_mask)

        # Shape: batch_size, 2 * num_tokens; batch_size, 2 * num_tokens; batch_size, 2 * num_tokens, 1
        top_span_hidden, top_span_mask, top_span_indices, top_span_scores = self.span_pruner(
            span_hidden, span_mask.float(), 2 * num_tokens
        )

        # We also predict the TAN properties of the clause...
        tan_scores, tan_probs, tan_loss = self._predict_tan(pred_rep, tan_set)

        # and the animacy properties of all of the spans.
        animacy_scores, animacy_probs, animacy_loss = self._predict_animacy(
            encoded_text, text_mask, animacy_spans, animacy_labels
        )

        import random
        logging_batch_index = int(random.random() * batch_size)

        if self.is_logging:
            tokens = metadata[logging_batch_index]["sentence_tokens"]
            print("----------")
            print(" ".join(tokens))
            print(tokens[metadata[logging_batch_index]["verb_index"]])
            clause_beam = top_clause_indices[logging_batch_index]
            clause_beam_scores = top_clause_scores[logging_batch_index]
            for i in sorted(range(min(self.num_pruned_clauses, 10)), key = lambda i: -clause_beam_scores[i]):
                print("%10.5f  %s" % (clause_beam_scores[i], self.vocab.get_token_from_index(clause_beam[i].item(), namespace = "abst-clause-labels")))
            print()
            prelim_scored_spans = self._to_scored_spans(span_mask, top_span_indices, top_span_mask)[logging_batch_index]
            for i in sorted(range(int(top_span_mask[logging_batch_index].sum().item())), key = lambda i: -top_span_scores[logging_batch_index, i])[0:10]:
                s = prelim_scored_spans[i]
                print("%10.5f  %s" % (top_span_scores[logging_batch_index, i], " ".join(tokens[s.start() : s.end() + 1]) + " " + str(s)))

        # If we're pretraining, we stop and calculate losses for the clause and span scores just against the gold sets.
        if self.is_pretraining:
            if clause_strings is None or qarg_pretrain_clauses is None:
                raise ConfigurationError("Must give gold labels during pretraining")
            clause_probs = torch.sigmoid(clause_scores)
            clause_loss = F.binary_cross_entropy_with_logits(
                clause_scores, clause_set, size_average = False
            ) / (batch_size * self.vocab.get_vocab_size("abst-clause-labels"))
            span_probs = torch.sigmoid(top_span_scores).squeeze(-1) * top_span_mask.float()
            gold_span_labels = self._get_prediction_map(
                answer_spans.view(batch_size, -1, 2),
                num_tokens,
                num_answers.view(batch_size, -1, 1),
            ).unsqueeze(-1)
            span_prediction_mask = batched_index_select(
                gold_span_labels,
                top_span_indices
            ).squeeze(-1)
            total_num_spans = top_span_mask.sum().item()
            span_loss = F.binary_cross_entropy_with_logits(
                top_span_scores.squeeze(-1), span_prediction_mask,
                weight = top_span_mask.float(), size_average = False
            ) / total_num_spans

            # qarg classifier pretraining
            _, num_pretrain_instances, _ = qarg_pretrain_spans.size()
            qarg_pretrain_mask = (qarg_pretrain_spans[:, :, 0] >= 0).squeeze(-1).long()

            # get clauses of input instances
            # max to prevent the padded labels from messing up the embedding module
            input_clauses = self.clause_embedding(qarg_pretrain_clauses.max(torch.zeros_like(qarg_pretrain_clauses)))

            # get spans of input instances
            # Shape: batch_size, num_spans, 2 * encoder_output_projected_dim
            span_pre_embeddings = self.span_extractor(encoded_text, qarg_pretrain_spans, text_mask, qarg_pretrain_mask)
            spanemb_A, spanemb_B = torch.chunk(span_pre_embeddings, 2, dim = -1)
            input_spans_hidden = self.span_hidden.hiddenA(spanemb_A) + \
                                 self.span_hidden.hiddenB(spanemb_B)

            # construct input embeddings
            expanded_pred_embedding = self.pred_final_hidden(pred_rep) \
                                        .view(   batch_size,                    1, self.final_embedding_dim) \
                                        .expand( batch_size, num_pretrain_instances, self.final_embedding_dim)
            qarg_inputs = expanded_pred_embedding + \
                          input_clauses + \
                          self.span_final_hidden(input_spans_hidden)

            # score and compute qarg loss
            qarg_pretrain_scores = self.qarg_scorer(qarg_inputs)
            final_qarg_pretrain_mask = qarg_pretrain_mask \
                .unsqueeze(-1) \
                .expand(batch_size, num_pretrain_instances, self.vocab.get_vocab_size("qarg-labels")) \
                .float()
            # multinomial loss:
            # binary loss:

            total_num_qarg_instances = final_qarg_pretrain_mask.sum().item()
            qarg_loss = F.binary_cross_entropy_with_logits(
                qarg_pretrain_scores, qarg_pretrain_labels, weight = final_qarg_pretrain_mask, size_average = False
            ) / total_num_qarg_instances
            pretrain_qarg_probs = torch.sigmoid(qarg_pretrain_scores).squeeze(-1) * final_qarg_pretrain_mask

            self.metric(
                clause_probs, clause_set,
                span_probs, span_prediction_mask,
                pretrain_qarg_probs, qarg_pretrain_labels,
                tan_probs, tan_set,
                animacy_probs, animacy_labels,
                metadata)
            return { "loss": (3 * (clause_loss + span_loss)) + qarg_loss + tan_loss + animacy_loss }

        # We take the cartesian product of clause/span beams --- but to save memory, we just do it with their scores, using broadcasting.
        top_clauses_reshaped = top_clause_scores.view(batch_size,  -1,  1) # -1 = num_pruned_clauses
        top_spans_reshaped   =   top_span_scores.view(batch_size,   1, -1) # -1 = 2 * num_tokens
        # We construct a square of scores, then flatten it for the next step.
        # Shape: batch_size, num_pruned_clauses, 2 * num_tokens
        summed_scores = (top_clauses_reshaped + top_spans_reshaped) \
            .view(  batch_size,                          -1, 1)
        summed_mask = top_span_mask \
            .view(  batch_size,                       1, 2 * num_tokens) \
            .expand(batch_size, self.num_pruned_clauses, 2 * num_tokens) \
            .contiguous() \
            .view(  batch_size,                          -1)

        # We prune the list of summed scores before constructing the joint embeddings for qarg scoring.
        # Shape: batch_size, final_beam_size; batch_size, final_beam_size; batch_size, final_beam_size, 1
        _, top_summed_mask, top_summed_indices, top_summed_scores = self.score_pruner(
            summed_scores, summed_mask.float(), self.final_beam_size
        )

        # We reconstruct the final jointly scorable items using the indices from both pruning steps.
        # indices in flattened cube = (clause_index * 2 * num_tokens) + span_index
        final_clause_index_indices = top_summed_indices.div(2 * num_tokens)
        final_span_index_indices = top_summed_indices.remainder(2 * num_tokens)

        # the indices of the summed scores are into the cube whose axes are the component beams with indices into the original vocabs,
        # so we recover those here
        final_clauses = batched_index_select(top_clause_indices.unsqueeze(-1), final_clause_index_indices).squeeze(-1)
        final_span_indices    = batched_index_select(top_span_indices.unsqueeze(-1), final_span_index_indices).squeeze(-1)
        final_span_embeddings = batched_index_select(top_span_hidden,                final_span_index_indices)
        # final_span_mask = batched_index_select(top_span_mask.unsqueeze(-1), final_span_index_indices).squeeze(-1)
        intermediate_beam_mask = top_summed_mask
        beam_sizes = intermediate_beam_mask.long().sum(1)

        final_clause_scores = batched_index_select(top_clause_scores, final_clause_index_indices).squeeze(-1)
        final_span_scores = batched_index_select(top_span_scores, final_span_index_indices).squeeze(-1)

        # We construct their embeddings under a concat + linear transformation,
        expanded_pred_embedding = self.pred_final_hidden(pred_rep) \
                                      .view(   batch_size,                    1, self.final_embedding_dim) \
                                      .expand( batch_size, self.final_beam_size, self.final_embedding_dim)
        joint_embeddings = expanded_pred_embedding + \
                           self.clause_embedding(final_clauses) + \
                           self.span_final_hidden(final_span_embeddings)

        # then pass them to the qarg scorer, adding the original scores to get the final logits.
        # Shape: batch_size, final_beam_size, vocab_size("qarg-labels")
        qarg_scores = self.qarg_scorer(joint_embeddings)
        final_scores = top_summed_scores \
            .expand(batch_size, self.final_beam_size, self.vocab.get_vocab_size("qarg-labels")) + \
            qarg_scores
        final_scores = final_scores.squeeze(-1)
        final_beam_mask = intermediate_beam_mask \
            .unsqueeze(-1) \
            .expand(batch_size, self.final_beam_size, self.vocab.get_vocab_size("qarg-labels"))

        # Now we produce the output data structures.
        scored_spans = self._to_scored_spans(span_mask, final_span_indices, intermediate_beam_mask)
        def get_beam_item_in_batch(batch_index, beam_index):
            clause_score = final_clause_scores[batch_index, beam_index].item()
            span_score = final_span_scores[batch_index, beam_index].item()
            item_qarg_scores = qarg_scores[batch_index, beam_index]
            qarg_items = [
                (self.vocab.get_token_from_index(i, namespace = "qarg-labels"), item_qarg_scores[i].item(), item_qarg_scores[i].item() + clause_score + span_score)
                for i in range(self.vocab.get_vocab_size("qarg-labels"))
            ]
            return {
                "clause": self.vocab.get_token_from_index(final_clauses[batch_index, beam_index].item(), "abst-clause-labels"),
                "clause_score": clause_score,
                "span":  scored_spans[batch_index][beam_index],
                "span_score": span_score,
                "qargs": qarg_items
            }
        batch_of_beams = [
            [get_beam_item_in_batch(batch_index, beam_index) for beam_index in range(beam_sizes[batch_index].item())]
            for batch_index in range(batch_size)
        ]

        if self.is_logging:
            beam = sorted(
                batch_of_beams[logging_batch_index],
                key = lambda x: -(x["clause_score"] + x["span_score"] + max([s for _, _, s in x["qargs"]]))
            )
            for i in range(0, min(10, len(beam))):
                x = beam[i]
                print("-----")
                print("%10.5f  %s" % (x["clause_score"], x["clause"]))
                print("%10.5f  %s" % (x["span_score"],   " ".join(tokens[x["span"].start() : x["span"].end() + 1]) + " (%s, %s)" % (x["span"].start(), x["span"].end())))
                print(" ".join(["%10.5f  %-10s" % (full_score, qarg) for qarg, _, full_score in sorted(x["qargs"], key = lambda x: -x[2])][0:4]))

        # Finally, if gold data is present, we compute the loss of our beam against the gold, and pass the results to the metric.
        loss_dict = {}
        if clause_strings is not None:
            if self.is_logging:
                logging_gold_set = metadata[logging_batch_index]["gold_set"]
                print("Gold:")
                for clause, qarg, (start, end) in logging_gold_set:
                    print("%40s %-15s %s" % (clause, qarg, " ".join(tokens[start : end + 1]) + (" (%s, %s)" % (start, end))))
            # Identify the beam items that were present in gold to get a prediction mask.
            prediction_mask = torch.zeros_like(final_scores)
            for batch_index in range(batch_size):
                gold_set = metadata[batch_index]["gold_set"]
                predicted_beam = batch_of_beams[batch_index]
                for beam_index in range(beam_sizes[batch_index].item()):
                    i = predicted_beam[beam_index]
                    for qarg_index in range(self.vocab.get_vocab_size("qarg-labels")):
                        qarg = self.vocab.get_token_from_index(qarg_index, namespace = "qarg-labels")
                        predicted_tuple = (i["clause"], qarg, (i["span"].start(), i["span"].end()))
                        if predicted_tuple in gold_set:
                            prediction_mask[batch_index, beam_index, qarg_index] = 1
            # Binary classification loss
            total_num_beam_items = final_beam_mask.sum().item()
            beam_loss = F.binary_cross_entropy_with_logits(
                final_scores, prediction_mask, weight = final_beam_mask, size_average = False
            ) / total_num_beam_items
            final_probs = torch.sigmoid(final_scores).squeeze(-1) * final_beam_mask.float()
            self.metric(final_probs.detach().long(), prediction_mask.detach().long(), metadata)
            loss_dict["loss"] = beam_loss + tan_loss + animacy_loss

        return {
            "full_beam": batch_of_beams,
            **loss_dict
        }

    def get_metrics(self, reset: bool = False):
        if self.metric is not None:
            return self.metric.get_metric(reset=reset)
        else:
            return {}

    def _get_encoded_text(self,
                      text: Dict[str, torch.LongTensor],
                      predicate_indicator: torch.LongTensor):
        # Shape: batch_size, num_tokens, embedding_dim
        embedded_text_input = self.embedding_dropout(self.text_field_embedder(text))

        # Shape: batch_size, num_tokens ?
        text_mask = get_text_field_mask(text)
        # Shape: batch_size, num_tokens, predicate_feature_dim ?
        embedded_predicate_indicator = self.predicate_feature_embedding(predicate_indicator.long())

        # Shape: batch_size, num_tokens, embedding_dim + predicate_feature_dim
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)

        batch_size, num_tokens, embedding_dim_with_predicate_feature = embedded_text_with_predicate_indicator.size()

        if self.stacked_encoder.get_input_dim() != embedding_dim_with_predicate_feature:
            # TODO fix error message
            raise ConfigurationError("The SRL model uses an indicator feature, which makes "
                                     "the embedding dimension one larger than the value "
                                     "specified. Therefore, the 'input_dim' of the stacked_encoder "
                                     "must be equal to total_embedding_dim + 1.")

        # Shape: batch_size, num_tokens, encoder_output_dim
        encoded_text = self.stacked_encoder(embedded_text_with_predicate_indicator, text_mask)
        if self.encoder_output_projection is not None:
            projected_encoded_text = self.encoder_output_projection(encoded_text)
        else:
            projected_encoded_text = encoded_text

        return projected_encoded_text, text_mask

    def _to_scored_spans(self, span_mask, top_span_indices, top_span_mask):
        span_mask = span_mask.data.cpu()
        top_span_indices = top_span_indices.data.cpu()
        top_span_mask = top_span_mask.data.cpu()
        batch_size, num_spans = span_mask.size()
        top_spans = []
        for b in range(batch_size):
            batch_spans = []
            for start, end, i in self._start_end_range(num_spans):
                batch_spans.append(Span(start, end))
            batch_top_spans = []
            for i in range(top_span_indices.size(1)):
                if top_span_mask[b, i].item() == 1:
                    batch_top_spans.append(batch_spans[top_span_indices[b, i]])
            top_spans.append(batch_top_spans)
        return top_spans

    def _start_end_range(self, num_spans):
        n = int(.5 * (math.sqrt(8 * num_spans + 1) -1))

        result = []
        i = 0
        for start in range(n):
            for end in range(start, n):
                result.append((start, end, i))
                i += 1

        return result

    def _get_prediction_map(self, spans, seq_length, num_answerers):
        batchsize, num_spans, _ = spans.size()
        span_mask = (spans[:, :, 0] >= 0).view(batchsize, num_spans).long()
        num_labels = int((seq_length * (seq_length+1))/2)
        labels = spans.data.new().resize_(batchsize, num_labels).zero_().float()
        spans = spans.data
        arg_indexes = (2 * spans[:,:,0] * seq_length - spans[:,:,0].float().pow(2).long() + spans[:,:,0]) / 2 + (spans[:,:,1] - spans[:,:,0])
        arg_indexes = arg_indexes * span_mask.data

        for b in range(batchsize):
            for s in range(num_spans):
                if span_mask.data[b, s] > 0:
                        labels[b, arg_indexes[b, s]] = 1

        return torch.autograd.Variable(labels.float())

    def _predict_tan(self, pred_rep, tan_set):
        batch_size = pred_rep.size(0)
        tan_scores = self.tan_scorer(pred_rep)
        tan_loss = F.binary_cross_entropy_with_logits(
            tan_scores, tan_set, size_average = False
        ) / (batch_size * self.vocab.get_vocab_size("tan-string-labels"))
        tan_probs = torch.sigmoid(tan_scores).squeeze(-1)
        return tan_scores, tan_probs, tan_loss

    def _predict_animacy(self, encoded_text, text_mask, animacy_spans, animacy_labels):
        _, num_animacy_instances, _ = animacy_spans.size()
        animacy_mask = (animacy_spans[:, :, 0] >= 0).float()
        # Shape: batch_size, num_spans, 2 * encoder_output_projected_dim
        animacy_span_pre_embeddings = self.span_extractor(encoded_text, animacy_spans, text_mask, animacy_mask.long())
        spanemb_A, spanemb_B = torch.chunk(animacy_span_pre_embeddings, 2, dim = -1)
        animacy_spans_hidden = self.span_hidden.hiddenA(spanemb_A) + \
                               self.span_hidden.hiddenB(spanemb_B)
        animacy_scores = self.animacy_scorer(animacy_spans_hidden).squeeze(-1)
        actual_num_animacy_instances = animacy_mask.sum().item()
        animacy_loss = F.binary_cross_entropy_with_logits(
            animacy_scores, animacy_labels.float(), weight = animacy_mask, size_average = False
        ) / actual_num_animacy_instances
        animacy_probs = torch.sigmoid(animacy_scores).squeeze(-1) * animacy_mask
        return animacy_scores, animacy_probs, animacy_loss
