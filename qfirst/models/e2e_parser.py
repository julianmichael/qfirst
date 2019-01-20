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
from allennlp.modules.token_embedders import Embedding
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import batched_index_select, get_text_field_mask, sequence_cross_entropy_with_logits

from qfirst.modules.question_generator import QuestionGenerator
from qfirst.data.util import get_slot_label_namespace
from qfirst.util.question_conversion import get_question_tensors_for_clause_tensors_batched
from qfirst.metrics import E2EMetric

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
                 num_pruned_qargs: int = 5,
                 span_hidden_dim: int = 128,
                 final_beam_size: int = 128,
                 final_embedding_dim: int = 128,
                 tan_hidden_dim: int = 128,
                 metric_thresholds: List[float] = [0.25, 0.5, 0.75],
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(E2EParser, self).__init__(vocab, regularizer)
        self.text_field_embedder = text_field_embedder

        self.span_hidden_dim = span_hidden_dim
        self.clause_hidden_dim = clause_hidden_dim
        self.qarg_hidden_dim = qarg_hidden_dim
        self.final_embedding_dim = final_embedding_dim

        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)
        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.num_pruned_clauses = num_pruned_clauses
        self.num_pruned_qargs = num_pruned_qargs
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
            Linear(self.encoder_output_projected_dim, self.clause_hidden_dim), ReLU(),
            Linear(self.clause_hidden_dim, self.vocab.get_vocab_size("qarg-labels"))
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

        self.joint_scorer = Linear(self.final_embedding_dim, 1)

        self.animacy_scorer = Linear(self.span_hidden_dim, 1)
        self.tan_scorer = Linear(self.encoder_output_projected_dim, self.vocab.get_vocab_size("tan-string-labels"))

        self.metric = E2EMetric(metric_thresholds)

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                clause_strings: torch.LongTensor = None,
                tan_strings: torch.LongTensor = None,
                qargs: torch.LongTensor = None,
                animacy_flags: torch.LongTensor = None,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None,
                **kwargs):

        # Shape: batch_size, num_tokens, encoder_output_projected_dim
        encoded_text, text_mask = self._get_encoded_text(text, predicate_indicator)
        batch_size, num_tokens, _ = encoded_text.size()

        # The predicate representation is what feeds into the question-related classifiers.
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = batched_index_select(encoded_text, predicate_index).squeeze(1)

        # From the same encoded text, we also assemble a representation for every answer span.
        # Shapes: batch_size, num_spans, span_hidden_dim; batch_size, num_spans
        span_hidden, span_mask = self.span_hidden(encoded_text, encoded_text, text_mask, text_mask)

        # We make predictions of which clauses, qargs, and answer spans are most likely to be used for this verb, and prune each into its own beam.
        clause_scores = self.clause_scorer(pred_rep)
        # Shapes: batch_size, num_pruned_clauses; batch_size, num_pruned_clauses, 1
        _, _, top_clause_indices, top_clause_scores = self.score_pruner(
            clause_scores.unsqueeze(-1),
            torch.ones([batch_size, self.vocab.get_vocab_size("abst-clause-labels")], dtype = torch.float64),
            self.num_pruned_clauses
        )

        qarg_scores = self.qarg_scorer(pred_rep)
        # Shape: batch_size, num_pruned_qargs; batch_size, num_pruned_qargs, 1
        _, _, top_qarg_indices, top_qarg_scores = self.score_pruner(
            qarg_scores.unsqueeze(-1),
            torch.ones([batch_size, self.vocab.get_vocab_size("qarg-labels")], dtype = torch.float64),
            self.num_pruned_qargs
        )

        # Shape: batch_size, 2 * num_tokens; batch_size, 2 * num_tokens; batch_size, 2 * num_tokens, 1
        top_span_hidden, top_span_mask, top_span_indices, top_span_scores = self.span_pruner(
            span_hidden, span_mask.float(), 2 * num_tokens
        )

        # Then we take the cartesian product of these beams --- but to save memory, we just do it with their scores, using broadcasting.
        top_clauses_reshaped = top_clause_scores.view(batch_size, -1,  1,  1) # -1 = num_pruned_clauses
        top_qargs_reshaped   =   top_qarg_scores.view(batch_size,  1, -1,  1) # -1 = num_pruned_qargs
        top_spans_reshaped   =   top_span_scores.view(batch_size,  1,  1, -1) # -1 = 2 * num_tokens
        # We construct a cube of scores, then flatten it for the next step.
        # Shape: batch_size, num_pruned_clauses, num_pruned_qargs, 2 * num_tokens
        summed_scores = (top_clauses_reshaped + top_qargs_reshaped + top_spans_reshaped) \
            .view(  batch_size,                                                             -1, 1)
        summed_mask = top_span_mask \
            .view(  batch_size,                       1,                     1, 2 * num_tokens) \
            .expand(batch_size, self.num_pruned_clauses, self.num_pruned_qargs, 2 * num_tokens) \
            .contiguous() \
            .view(  batch_size,                                                             -1)

        # We prune the list of summed scores before constructing the joint embeddings for final scoring of the best ones.
        # Shape: batch_size, final_beam_size; batch_size, final_beam_size; batch_size, final_beam_size, 1
        _, top_summed_mask, top_summed_indices, top_summed_scores = self.score_pruner(
            summed_scores, summed_mask.float(), self.final_beam_size
        )

        # We reconstruct the final jointly scorable items using the indices from both pruning steps.
        # indices in flattened cube = (clause_index * num_pruned_qargs * 2 * num_tokens) + (qarg_index * 2 * num_tokens) + span_index
        final_span_index_indices = top_summed_indices.remainder(2 * num_tokens)
        final_qarg_index_indices = top_summed_indices.div(2 * num_tokens).remainder(self.num_pruned_qargs)
        final_clause_index_indices = top_summed_indices.div(2 * num_tokens * self.num_pruned_qargs)

        # the indices of the summed scores are into the cube whose axes are the component beams with indices into the original vocabs,
        # so we recover those here
        final_clauses = batched_index_select(top_clause_indices.unsqueeze(-1), final_clause_index_indices).squeeze(-1)
        final_qargs   = batched_index_select(top_qarg_indices.unsqueeze(-1),   final_qarg_index_indices).squeeze(-1)
        final_span_indices    = batched_index_select(top_span_indices.unsqueeze(-1), final_span_index_indices).squeeze(-1)
        final_span_embeddings = batched_index_select(top_span_hidden,                final_span_index_indices)
        final_span_mask = batched_index_select(top_span_mask.unsqueeze(-1), final_span_index_indices).squeeze(-1)

        # We finally construct their embeddings under a concat + linear transformation,
        expanded_pred_embedding = self.pred_final_hidden(pred_rep) \
                                      .view(   batch_size,                    1, self.final_embedding_dim) \
                                      .expand( batch_size, self.final_beam_size, self.final_embedding_dim)
        joint_embeddings = expanded_pred_embedding + \
                           self.clause_embedding(final_clauses) + \
                           self.qarg_embedding(final_qargs) + \
                           self.span_final_hidden(final_span_embeddings)
        # pass them to the joint scorer,
        joint_scores = self.joint_scorer(F.relu(joint_embeddings))
        # and compute the final scores.
        final_scores = top_summed_scores + joint_scores

        # Totally separately, we make the TAN prediction for the verb and and animacy predictions for each answer span.
        # TODO: we could also do this on the joint embedding to get QA-pair-specific TAN predictions.
        # TODO: right now I'm not bothering to do anything with these. I must incorporate them into output/loss at some point.
        # Shape: batch_size, vocab.get_vocab_size("tan-string-labels")
        tan_logits = self.tan_scorer(pred_rep)
        # Shape: batch_size, final_beam_size, 1 ?
        animacy_logits = self.animacy_scorer(final_span_embeddings)

        # Now we produce the output data structures.
        scored_spans = self._to_scored_spans(span_mask, final_span_indices, final_span_mask)
        def get_beam_item_in_batch(batch_index, beam_index):
            return {
                "clause": self.vocab.get_token_from_index(final_clauses[batch_index, beam_index].item(), "abst-clause-labels"),
                "qarg":  self.vocab.get_token_from_index( final_qargs[batch_index, beam_index].item(), "qarg-labels"),
                "span":   scored_spans[batch_index][beam_index],
                "score":  final_scores[batch_index, beam_index].item()
            }
        batch_of_beams = [
            [get_beam_item_in_batch(batch_index, beam_index) for beam_index in range(self.final_beam_size)]
            for batch_index in range(batch_size)
        ]

        # Finally, if gold data is present, we compute the loss of our beam against the gold, and pass the results to the metric.
        loss_dict = {}
        if clause_strings is not None:
            # Identify the beam items that were present in gold to get a prediction mask.
            prediction_mask = torch.zeros_like(final_scores)
            for batch_index in range(batch_size):
                gold_set = metadata[batch_index]["gold_set"]
                predicted_beam = batch_of_beams[batch_index]
                for beam_index in range(self.final_beam_size):
                    i = predicted_beam[beam_index]
                    predicted_tuple = (i["clause"], i["qarg"], (i["span"].start(), i["span"].end()))
                    if predicted_tuple in gold_set:
                        prediction_mask[batch_index, beam_index] = 1
            # Binary classification loss
            loss_dict["loss"] = F.binary_cross_entropy_with_logits(final_scores, prediction_mask, size_average = True)
            self.metric(final_scores.detach().long(), prediction_mask.detach().long(), metadata)

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
        # print("embedded text: " + str(embedded_text_input.size()))

        # Shape: batch_size, num_tokens ?
        text_mask = get_text_field_mask(text)
        # Shape: batch_size, num_tokens, predicate_feature_dim ?
        embedded_predicate_indicator = self.predicate_feature_embedding(predicate_indicator.long())
        # print("embedded predicate indicator: " + str(embedded_predicate_indicator.size()))

        # Shape: batch_size, num_tokens, embedding_dim + predicate_feature_dim
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        # print("embedded text with predicate indicator: " + str(embedded_text_with_predicate_indicator.size()))

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
