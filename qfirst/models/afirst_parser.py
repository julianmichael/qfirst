from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary
from allennlp.data.dataset import Batch

import math

from qfirst.common.span import Span
# from qfirst.models.question_predictor import QuestionPredictor
# from qfirst.models.span_detector import SpanDetector

from qfirst.metrics.beam_metric import BeamMetric

from itertools import groupby

# "Reproduction" of the nrl-qasrl model
# should receive instances from the qfirst dataset reader though
# @Model.register("afirst_parser")
# class AfirstParser(Model):
#     def __init__(self, vocab: Vocabulary,
#                  span_detector: SpanDetector,
#                  question_predictor: QuestionPredictor,
#                  span_minimum_prob: float,
#                  metric: BeamMetric):
#         super(AfirstParser, self).__init__(vocab)
#         self.span_detector = span_detector
#         self.question_predictor = question_predictor
#         self.span_minimum_prob = span_minimum_prob
#         self.metric = metric

#     @overrides
#     def forward(self,
#                 text: Dict[str, torch.LongTensor],
#                 predicate_indicator: torch.LongTensor,
#                 labeled_spans: torch.LongTensor = None,
#                 metadata = None,
#                 annotations = None,
#                 **kwargs):
#         if self.training:
#             raise ConfigurationError("AfirstParser cannot be trained directly. It must be assembled from its trained components.")

#         detected_span_outputs = self.span_detector.forward(text, predicate_indicator)
#         detected_spans = self.span_detector.decode(detected_span_outputs)["spans"]
#         detected_spans = [[(s, p.item()) for s, p in batch_spans if p >= self.span_minimum_prob] for batch_spans in detected_spans]

#         batch_size = predicate_indicator.size(0)
#         max_num_spans = max([len(instance_spans) for instance_spans in detected_spans])

#         pred_labeled_spans = torch.ones(batch_size, max_num_spans, 2, dtype = torch.long, device = predicate_indicator.device)
#         pred_labeled_spans *= -1.0
#         pred_span_lists = []
#         for i, batch_spans in enumerate(detected_spans):
#             spans = [(s, p) for s, p in batch_spans if p >= 0.2]
#             pred_span_lists.append(spans)
#             for j, (span, _) in enumerate(spans):
#                 pred_labeled_spans[i, j, 0] = span.start()
#                 pred_labeled_spans[i, j, 1] = span.end()

#         qgen_outputs = self.question_predictor.forward(text, predicate_indicator, pred_labeled_spans)
#         qgen_questions = self.question_predictor.decode(qgen_outputs)["questions"]

#         # assemble prediction beams
#         full_predictions = []
#         for spans, questions in zip(pred_span_lists, qgen_questions):
#             pred_qa_groups = [(tuples[0][1], [sp for sp, _ in tuples])
#                                 for q, _tuples_gen in groupby(zip(spans, questions), lambda t: " ".join(t[1]))
#                                 for tuples in (list(_tuples_gen),)]
#             pred_qa_pairs = []
#             for q, spans in pred_qa_groups:
#                 question = list(q)
#                 question_slots = {
#                     slot_name: slot_value
#                     for slot_name, slot_value in zip(self.question_predictor.question_generator.get_slot_labels(), question)
#                 }
#                 pred_qa_pairs.append({"question": question, "question_slots": question_slots, "answer_spans": spans })
#             full_predictions.append(pred_qa_pairs)

#         if annotations is not None: # we have gold datums
#             batched_gold_qa_pairs = []
#             for bi, instance_annotations in enumerate(annotations):
#                 gold_qa_pairs = []
#                 for qi, question_label in enumerate(instance_annotations["question_labels"]):
#                     def get_spans(spansJson):
#                         return [Span(s[0], s[1]-1) for s in spansJson]
#                     all_gold_answer_spans = [s for ans in question_label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"])]
#                     num_answers = len(question_label["answerJudgments"])
#                     num_invalids = len([ans for ans in question_label["answerJudgments"] if not ans["isValid"]])
#                     distinct_gold_answer_spans = list(set(all_gold_answer_spans))
#                     gold_question_dict = {
#                         "question_slots": question_label["questionSlots"],
#                         "question": [question_label["questionSlots"][n] for n in self.question_predictor.question_generator.get_slot_labels()],
#                         "answer_spans": distinct_gold_answer_spans,
#                         "num_answers": num_answers,
#                         "num_gold_invalids": num_invalids
#                     }
#                     gold_qa_pairs.append(gold_question_dict)
#                 batched_gold_qa_pairs.append(gold_qa_pairs)

#             if self.metric is not None:
#                 for i in range(batch_size):
#                     self.metric(batched_gold_qa_pairs[i], full_predictions[i], metadata[i])

#         return {
#             "full_predictions": full_predictions
#         }

#     def get_metrics(self, reset: bool = False):
#         if self.metric is not None:
#             return self.metric.get_metric(reset=reset)
#         else:
#             return {}

#     @classmethod
#     def from_params(cls, vocab: Vocabulary, params: Params) -> 'AfirstParser':
#         span_detector = Model.from_params(vocab, params.pop("span_detector"))
#         question_predictor = Model.from_params(vocab, params.pop("question_predictor"))
#         span_minimum_prob = params.pop("span_minimum_prob", 0.01)
#         metric_config = params.pop("metric", None)
#         metric = None
#         if metric_config is not None:
#             metric = BeamMetric.from_params(metric_config)
#         return AfirstParser(vocab,
#                             span_detector = span_detector, question_predictor = question_predictor,
#                             span_minimum_prob = span_minimum_prob,
#                             metric = metric)
