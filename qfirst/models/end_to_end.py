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

from qfirst.data.util import get_slot_label_namespace
from qfirst.util.question_conversion import get_question_tensors_for_clause_tensors_batched
from qfirst.metrics import EndToEndMetric, E2EPretrainingMetric

import math

from qfirst.common.span import Span
from qfirst.models.clause_and_span_to_answer_slot import ClauseAndSpanToAnswerSlotModel
from qfirst.models.multiclass import MulticlassModel
from qfirst.models.span import SpanModel
from qfirst.modules.span_rep_assembly import SpanRepAssembly

# should receive verb instances from the qasrl dataset reader
@Model.register("qasrl_end_to_end")
class EndToEndModel(Model):
    def __init__(self, vocab: Vocabulary,
                 clause_model: MulticlassModel,
                 span_model: SpanModel,
                 cs_to_slot_model: ClauseAndSpanToAnswerSlotModel,
                 clause_beam_size: int = 8,
                 final_beam_size: int = 128,
                 metric: EndToEndMetric = EndToEndMetric(),
                 is_logging: bool = False,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(EndToEndModel, self).__init__(vocab, regularizer)
        self._clause_model = clause_model
        self._span_model = span_model
        self._cs_to_slot_model = cs_to_slot_model
        self._clause_beam_size = clause_beam_size
        self._final_beam_size = final_beam_size
        self._metric = metric
        self._is_logging = is_logging

        self._pruner = Pruner(lambda x: x)

        # TODO consider this?
        # span_beam_size_override: int = -1,
        # self._span_beam_size_override = span_beam_size_override

        # TODO maybe include sum of scores objective as a hyperparam
        # use_sum_of_scores: bool = False,

        initializer(self)

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                metadata = None,
                **kwargs):
        device = predicate_index.device
        clause_outputs = self._clause_model(text, predicate_indicator, predicate_index)
        span_outputs = self._span_model.decode(self._span_model(text, predicate_indicator, predicate_index))

        batch_size, num_spans = span_outputs["top_span_indices"].size()

        _, _, top_clause_indices, top_clause_scores = self._pruner(
            clause_outputs["logits"].unsqueeze(-1),
            torch.ones([batch_size, self.vocab.get_vocab_size("abst-clause-labels")], dtype = torch.float64, device = device),
            self._clause_beam_size)

        top_span_scores = span_outputs["top_span_logits"]
        top_span_mask = span_outputs["top_span_mask"]
        def tensorize_span_list(span_list):
            spans = [torch.LongTensor([s.start(), s.end()], device = device) for s, _ in span_list]
            padding = [torch.LongTensor([-1, -1], device = device)] * (num_spans - len(span_list))
            return torch.stack(spans + padding)
        top_span_indices = torch.stack([tensorize_span_list(span_list) for span_list in span_outputs["spans"]])

        import random
        logging_batch_index = int(random.random() * batch_size)
        if self._is_logging:
            tokens = metadata[logging_batch_index]["sentence_tokens"]
            print("----------")
            print(" ".join(tokens))
            print(tokens[metadata[logging_batch_index]["verb_index"]])
            clause_beam = top_clause_indices[logging_batch_index]
            clause_beam_scores = top_clause_scores[logging_batch_index]
            for i in sorted(range(min(self._clause_beam_size, 10)), key = lambda i: -clause_beam_scores[i]):
                print("%10.5f  %s" % (clause_beam_scores[i], self.vocab.get_token_from_index(clause_beam[i].item(), namespace = "abst-clause-labels")))
            print()
            for i in sorted(range(int(top_span_mask[logging_batch_index].sum().item())), key = lambda i: -top_span_scores[logging_batch_index, i])[0:10]:
                s, _ = span_outputs["spans"][logging_batch_index][i]
                print("%10.5f  %s" % (top_span_scores[logging_batch_index, i], " ".join(tokens[s.start() : s.end() + 1]) + " " + str(s)))

        # if not self.use_sum_of_scores:
        top_clause_scores = F.logsigmoid(top_clause_scores)
        top_span_scores = F.logsigmoid(top_span_scores)

        # We take the cartesian product of clause/span beams --- but to save memory, we just do it with their scores, using broadcasting.
        top_clauses_reshaped = top_clause_scores.view(batch_size,  -1,  1) # -1 = self._clause_beam_size
        top_spans_reshaped   =   top_span_scores.view(batch_size,   1, -1) # -1 = 2 * num_tokens
        # We construct a square of scores, then flatten it for the next step.
        # Shape: batch_size, self._clause_beam_size, 2 * num_tokens
        summed_scores = (top_clauses_reshaped + top_spans_reshaped) \
            .view(  batch_size,                     -1,         1)
        summed_mask = top_span_mask \
            .view(  batch_size,                      1, num_spans) \
            .expand(batch_size, self._clause_beam_size, num_spans) \
            .contiguous() \
            .view(  batch_size,                                -1)

        # We prune the list of summed scores before constructing the joint embeddings for qarg scoring.
        # Shape: batch_size, final_beam_size; batch_size, final_beam_size; batch_size, final_beam_size, 1
        _, top_summed_mask, top_summed_indices, top_summed_scores = self._pruner(
            summed_scores, summed_mask.float(), self._final_beam_size
        )

        # We reconstruct the final jointly scorable items using the indices from both pruning steps.
        # indices in flattened cube = (clause_index * 2 * num_tokens) + span_index
        final_clause_index_indices = top_summed_indices.div(num_spans)
        final_span_index_indices = top_summed_indices.remainder(num_spans)

        # the indices of the summed scores are into the cube whose axes are the component beams with indices into the original vocabs,
        # so we recover those here
        final_clause_indices = batched_index_select(top_clause_indices.unsqueeze(-1), final_clause_index_indices).squeeze(-1)
        final_clause_scores = batched_index_select(top_clause_scores, final_clause_index_indices).squeeze(-1)

        final_span_indices = batched_index_select(top_span_indices, final_span_index_indices)
        final_span_mask = batched_index_select(top_span_mask.unsqueeze(-1), final_span_index_indices).squeeze(-1)
        final_span_scores = batched_index_select(top_span_scores, final_span_index_indices).squeeze(-1)
        # final_scored_spans = self._span_model._span_selector._to_scored_spans(span_outputs["span_mask"], final_span_indices, final_span_mask, final_span_scores)

        # print("top_span_mask: " + str(top_span_mask.size()))
        # print("top_span_scores: " + str(top_span_scores.size()))

        intermediate_beam_mask = top_summed_mask
        beam_sizes = intermediate_beam_mask.long().sum(1)

        # Shape: batch_size, final_beam_size, vocab_size("qarg-labels")
        qarg_output = self._cs_to_slot_model(
            text, predicate_indicator, predicate_index,
            final_clause_indices, final_span_indices)
        # cond
        qarg_scores = F.logsigmoid(qarg_output["logits"])

        final_scores = top_summed_scores \
            .expand(batch_size, self._final_beam_size, self.vocab.get_vocab_size("qarg-labels")) + \
            qarg_scores
        final_scores = final_scores.squeeze(-1)
        final_beam_mask = intermediate_beam_mask \
            .unsqueeze(-1) \
            .expand(batch_size, self._final_beam_size, self.vocab.get_vocab_size("qarg-labels"))

        # Now we produce the output data structures.
        scored_spans = [
            (Span(final_span_indices[b, i, 0].item(), final_span_indices[b, i, 1].item()), final_span_scores[b, i].item())
            for b in range(batch_size)
            for i in range(beam_sizes[b])
        ]

        def get_beam_item_in_batch(batch_index, beam_index):
            clause_score = final_clause_scores[batch_index, beam_index]
            span_score = final_span_scores[batch_index, beam_index]
            item_qarg_scores = qarg_scores[batch_index, beam_index]
            qarg_items = [
                (self.vocab.get_token_from_index(i, namespace = "qarg-labels"), item_qarg_scores[i].item())
                for i in range(self.vocab.get_vocab_size("qarg-labels"))
            ]
            qarg, qarg_score = max(qarg_items, key = lambda t: t[1])
            total_score = clause_score + span_score + qarg_score
            # TODO ensure consistency with final score
            total_prob = total_score.exp()
            return {
                "clause": self.vocab.get_token_from_index(final_clause_indices[batch_index, beam_index].item(), "abst-clause-labels"),
                "clause_score": clause_score.item(),
                "span":  scored_spans[batch_index][beam_index],
                "span_score": span_score.item(),
                "qarg": qarg,
                "qarg_score": qarg_score,
                "total_prob": total_prob.item(),
                "all_qargs": qarg_items
            }
        batch_of_beams = [
            [get_beam_item_in_batch(batch_index, beam_index) for beam_index in range(beam_sizes[batch_index].item())]
            for batch_index in range(batch_size)
        ]


        if self._is_logging:
            beam = sorted(
                batch_of_beams[logging_batch_index],
                key = lambda x: -(max([s for _, _, s in x["qargs"]]))
            )
            for i in range(0, min(10, len(beam))):
                x = beam[i]
                print("-----")
                print("%10.5f  %s" % (x["clause_score"], x["clause"]))
                print("%10.5f  %s" % (x["span_score"],   " ".join(tokens[x["span"].start() : x["span"].end() + 1]) + " (%s, %s)" % (x["span"].start(), x["span"].end())))
                print(" ".join(["%10.5f  %-10s" % (full_score, qarg) for qarg, _, full_score in sorted(x["qargs"], key = lambda x: -x[2])][0:4]))

        # Finally, if gold data is present, we compute the loss of our beam against the gold, and pass the results to the metric.
        loss_dict = {}
        if metadata is not None and metadata[0]["gold_set"] is not None:
            if self._is_logging:
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
            if self.use_product_of_probs:
                final_probs = final_scores.exp().squeeze(-1) * final_beam_mask.float()
                beam_loss = F.binary_cross_entropy(
                    final_probs, prediction_mask, weight = final_beam_mask, reduction = "sum"
                ) / total_num_beam_items
            else:
                beam_loss = F.binary_cross_entropy_with_logits(
                    final_scores, prediction_mask, weight = final_beam_mask, reduction = "sum"
                ) / total_num_beam_items
                final_probs = torch.sigmoid(final_scores).squeeze(-1) * final_beam_mask.float()
            self.metric(prediction_mask.sum().item(), batch_of_beams, metadata)
            loss_dict["loss"] = (5 * beam_loss) + tan_loss + animacy_loss

        return {
            "beam": batch_of_beams,
            "animacies": batch_of_animacies,
            "tans": batch_of_tans,
            **loss_dict
        }

    def get_metrics(self, reset: bool = False):
        if self.metric is not None:
            return self.metric.get_metric(reset=reset)
        else:
            return {}

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
