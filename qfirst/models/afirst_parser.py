from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary
from allennlp.data.dataset import Batch

# from qfirst.util import QuestionGenerator
# from qfirst.models.question_answerer import QuestionAnswerer
# from qfirst.data.util import get_slot_label_namespace

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.afirst_beam_metric.afirst_beam_metric import AfirstBeamMetric

import math

from nrl.common.span import Span
from nrl.models.question_predictor import QuestionPredictor
from nrl.models.span_detector import SpanDetector

from itertools import groupby

# "Reproduction" of the nrl-qasrl model
# should receive instances from the qfirst dataset reader though
@Model.register("afirst_parser")
class AfirstParser(Model):
    def __init__(self, vocab: Vocabulary,
                 span_detector: SpanDetector,
                 question_predictor: QuestionPredictor,
                 metric: AfirstBeamMetric):
        super(AfirstParser, self).__init__(vocab)
        self.span_detector = span_detector
        self.question_predictor = question_predictor
        self.metric = metric

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                labeled_spans: torch.LongTensor = None,
                metadata = None,
                **kwargs):
        if self.training:
            raise ConfigurationError("AfirstParser cannot be trained directly. It must be assembled from its trained components.")

        detected_span_outputs = self.span_detector.forward(text, predicate_indicator)
        detected_spans = self.span_detector.decode(detected_span_outputs)["spans"]
        detected_spans = [[(s, p.item()) for s, p in batch_spans] for batch_spans in detected_spans]

        batch_size = predicate_indicator.size(0)
        max_num_spans = max([len(instance_spans) for instance_spans in detected_spans])

        pred_labeled_spans = torch.ones(batch_size, max_num_spans, 2, dtype = torch.long, device = predicate_indicator.device)
        pred_labeled_spans *= -1.0
        pred_span_lists = []
        for i, batch_spans in enumerate(detected_spans):
            spans = [(s, p) for s, p in batch_spans if p >= 0.2]
            pred_span_lists.append(spans)
            for j, (span, _) in enumerate(spans):
                pred_labeled_spans[i, j, 0] = span.start()
                pred_labeled_spans[i, j, 1] = span.end()

        qgen_outputs = self.question_predictor.forward(text, predicate_indicator, pred_labeled_spans)
        qgen_questions = self.question_predictor.decode(qgen_outputs)["questions"]

        # assemble prediction beams
        full_predictions = []
        for spans, questions in zip(pred_span_lists, qgen_questions):
            pred_qa_groups = [(tuples[0][1], [sp for sp, _ in tuples])
                                for q, _tuples_gen in groupby(zip(spans, questions), lambda t: " ".join(t[1]))
                                for tuples in (list(_tuples_gen),)]
            pred_qa_pairs = []
            for q, spans in pred_qa_groups:
                pred_qa_pairs.append({"question": list(q), "spans": spans })
            full_predictions.append(pred_qa_pairs)

        if labeled_spans is not None: # we have gold datums
            span_mask = (labeled_spans[:, :, 0] >= 0).long()

            # assemble gold qa sets
            span_slot_labels = {}
            for n in self.question_predictor.question_generator.get_slot_labels():
                if 'span_slot_%s'%n in kwargs and kwargs['span_slot_%s'%n] is not None:
                    span_slot_labels[n] = kwargs['span_slot_%s'%n] * span_mask
            if len(span_slot_labels) == 0:
                span_slot_labels = None

            batched_gold_qa_pairs = []
            for batch_index in range(batch_size):
                gold_questions = []
                gold_spans = []
                for span_index in range(labeled_spans.size(1)):
                    if span_mask[batch_index, span_index] == 1:
                        # question
                        slots = []
                        for slot_name in self.question_predictor.question_generator.get_slot_labels():
                            index_to_token = self.vocab.get_index_to_token_vocabulary("slot_%s" % slot_name)
                            slots.append(index_to_token[span_slot_labels[slot_name][batch_index, span_index].item()])
                        gold_questions.append(slots)
                        # answer
                        start = labeled_spans[batch_index, span_index, 0].item()
                        end = labeled_spans[batch_index, span_index, 1].item()
                        gold_spans.append(Span(start, end))
                gold_qa_groups = [(tuples[0][1], list(set([s for s, _ in tuples])))
                                  for q, _tuples_gen in groupby(zip(gold_spans, gold_questions), lambda t: " ".join(t[1]))
                                  for tuples in (list(_tuples_gen),)]
                gold_qa_pairs = []
                for q, spans in gold_qa_groups:
                    gold_qa_pairs.append({"question": q, "answer_spans": spans })
                batched_gold_qa_pairs.append(gold_qa_pairs)

            if self.metric is not None:
                for i in range(batch_size):
                    self.metric(batched_gold_qa_pairs[i], full_predictions[i])
                    # sentence_tokens = metadata[i]["sent_text"].split()
                    # print(metadata[i]["sent_text"])
                    # print("\nGOLD:")
                    # for gold_entry in batched_gold_qa_pairs[i]:
                    #     print("\n" + " ".join(gold_entry["question"]))
                    #     span_strings = [" ".join(sentence_tokens[span.start() : (span.end() + 1)]) for span in gold_entry["answer_spans"]]
                    #     print("   " + "\n   ".join(span_strings))
                    # print("\nPREDICTED:")
                    # for entry in full_predictions[i]:
                    #     print("\n" + " ".join(entry["question"]))
                    #     span_strings = [" ".join(sentence_tokens[span.start() : (span.end() + 1)]) for span in entry["spans"]]
                    #     print("   " + ("\n   ").join(span_strings))

        return {
            "full_predictions": full_predictions
        }

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'AfirstParser':
        span_detector = Model.from_params(vocab, params.pop("span_detector"))
        question_predictor = Model.from_params(vocab, params.pop("question_predictor"))
        metric = AfirstBeamMetric.from_params(params.pop("metric"))
        return AfirstParser(vocab,
                            span_detector = span_detector, question_predictor = question_predictor,
                            metric = metric)
