from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary

from qfirst.models.question_predictor import QuestionPredictor
from qfirst.models.question_answerer import QuestionAnswerer

# should receive verb instances from the qasrl dataset reader
@Model.register("qfirst_parser")
class QfirstParser(Model):
    def __init__(self, vocab: Vocabulary,
                 question_generator: QuestionGenerator,
                 question_answerer: QuestionAnswerer,
                 question_threshold: float = 0.02,
                 span_threshold: float = 0.5,
                 invalid_threshold: float = 0.2,
                 max_beam_size: int = 30):
        super(QfirstParser, self).__init__(vocab)
        self.question_generator = question_generator
        self.question_answerer = question_answerer
        self.slot_names = question_answerer.question_encoder.get_slot_names()
        self.metric = EndToEndMetric(self.slot_names)

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                answers: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                **kwargs):
        if self.training:
            raise ConfigurationError("QfirstParser cannot be trained directly. It must be assembled from its trained components.")
        if predicate_indicator.size(0) != 1:
            raise ConfigurationError("QfirstParser must be run with a batch size of 1.")
        gold_question_slot_labels = {}
        for slot_name in self.slot_names:
            if slot_name in kwargs and kwargs[slot_name] is not None:
                gold_question_slot_labels[slot_name] = kwargs[slot_name]
        for slot_name in self.slot_names:
            if slot_name not in kwargs or kwargs[slot_name] is None:
                gold_question_slot_labels = None
        gold_qa_pairs = []
        answer_spans = answers.squeeze(0)
        span_label_mask = (answer_spans[:, :, 0] >= 0).squeeze(-1).long()
        num_answers_single_batch = num_answers.squeeze(0)
        num_tokens = question_answerer.text_field_embedder(text).size(1)
        answer_span_labels = self.question_answerer.get_prediction_map(answer_spans, span_label_mask,
                                                                       num_tokens, num_answers_single_batch,
                                                                       union_gold_spans = True)
        extracted_answer_spans = self.question_answerer.to_scored_spans(answer_span_labels, span_label_mask)
        for qi in range(answer_spans.size(0)):
            gold_question_dict = {
                "question": [gold_question_slot_labels[n][0, qi] for n in self.slot_names],
                "answer_spans": [span for span, prob in extracted_answer_spans[qi]]
            }

        beam_slots, beam_log_probs = self.question_generator.beam_decode_single(text, predicate_indicator)
        beam_size = beam_log_probs.size(0)
        text_expanded = { k: v.expand(beam_size, -1) for k, v in text.items() }
        predicate_indicator_expanded = predicate_indicator.expand(beam_size, -1)
        answerer_forward_outputs = self.question_answerer.forward(text_expanded, predicate_indicator_expanded, **beam_slots)
        answerer_outputs = self.question_answerer.decode(answerer_forward_outputs)
        full_beam = []
        for i in range(beam_size):
            beam_entry_dict = {
                "question": [beam_slots[n][i] for n in self.slot_names],
                "question_prob": math.exp(beam_log_probs[i]),
                "spans": answerer_outputs["spans"][i],
                "invalidity_prob": answerer_outputs["invalidity_prob"][i]
            }
            full_beam.append(beam_entry_dict)
        filtered_beam = self.filter_beam(full_beam)

        if gold_qa_pairs is not None:
            self.metric(gold_qa_pairs, filtered_beam)
            # TODO print the stuff

        return {
            "full_beam": full_beam
            "filtered_beam": filtered_beam
        }

    def filter_beam(self, beam):
        """
        Non-max suppression (1D, by answer span overlap) filtering of the QA beam.
        Filter out questions with invalidity scores above the threshold or with no remaining non-overlapping spans.
        """
        filtered_beam = []

        def has_overlap(candidate_span):
            for entry in filtered_beam:
                for span in entry["spans"]:
                    if candidate_span.overlaps(span):
                        return True
            return False

        for entry in beam:
            is_valid = entry["invalidity_prob"] < self.invalid_threshold
            valid_spans = [span in span, prob in entry["spans"] if prob >= self.span_threshold]
            novel_spans = [span in valid_spans if not has_overlap(span)]
            if(is_valid and len(novel_spans) > 0):
                filtered_entry = {
                    "question": entry["question"],
                    "question_prob": entry["question_prob"],
                    "answer_spans": novel_spans
                }
                filtered_beam.append(filtered_entry)

        return filtered_beam

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QfirstParser':
        question_generator = Model.from_params(vocab, params.pop("question_generator"))
        question_answerer = Model.from_params(vocab, params.pop("question_answerer"))
        max_beam_size = params.pop("max_beam_size", 30)
        question_threshold = params.pop("question_threshold", [0.02])
        span_threshold = params.pop("span_threshold", [0.5])
        invalid_threshold = params.pop("invalid_threshold", [0.2])

        return QfirstParser(vocab,
                            question_generator = question_generator, question_answerer = question_answerer,
                            max_beam_size = max_beam_size
                            question_threshold = question_threshold,
                            span_threshold = span_threshold, invalid_threshold = invalid_threshold)
