from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.util import QuestionGenerator
from qfirst.models.question_answerer import QuestionAnswerer
from qfirst.data.util import get_slot_label_namespace

import math

from nrl.common.span import Span

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
        self.max_beam_size = max_beam_size
        self.span_threshold = span_threshold
        self.invalid_threshold = invalid_threshold
        self.metric = EndToEndMetric(self.slot_names)

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                answers: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None,
                **kwargs):
        if self.training:
            raise ConfigurationError("QfirstParser cannot be trained directly. It must be assembled from its trained components.")
        if predicate_indicator.size(0) != 1:
            raise ConfigurationError("QfirstParser must be run with a batch size of 1.")

        beam_slots, beam_log_probs = self.question_generator.beam_decode_single(text, predicate_indicator, self.max_beam_size)
        beam_size = beam_log_probs.size(0)
        text_expanded = { k: v.expand(beam_size, -1, -1) for k, v in text.items() }
        predicate_indicator_expanded = predicate_indicator.expand(beam_size, -1)
        answerer_forward_outputs = self.question_answerer.forward(text_expanded, predicate_indicator_expanded, **beam_slots)
        answerer_outputs = self.question_answerer.decode(answerer_forward_outputs)
        full_beam = []
        for i in range(beam_size):
            # [beam_slots[n][i] for n in self.slot_names]
            beam_entry_dict = {
                "question": [self.vocab.get_index_to_token_vocabulary(get_slot_label_namespace(n))[beam_slots[n][i].item()] for n in self.slot_names],
                "question_prob": math.exp(beam_log_probs[i]),
                "spans": answerer_outputs["spans"][i],
                "invalidity_prob": answerer_outputs["invalidity_prob"][i].item()
            }
            full_beam.append(beam_entry_dict)

        filtered_beam = self.filter_beam(full_beam)

        if num_answers is not None: # we have gold datums
            metadata = metadata[0]
            num_answers_cpu = num_answers[0].cpu()
            num_invalids_cpu = num_invalids[0].cpu()
            gold_qa_pairs = []
            for qi, question_label in enumerate(metadata["question_labels"]):
                def get_spans(spansJson):
                    return [Span(s[0], s[1]-1) for s in spansJson]
                all_gold_answer_spans = [s for ans in question_label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"])]
                distinct_gold_answer_spans = list(set(all_gold_answer_spans))
                gold_question_dict = {
                    "question": [question_label["questionSlots"][n] for n in self.slot_names],
                    "answer_spans": distinct_gold_answer_spans,
                    "num_answers": num_answers_cpu[qi],
                    "num_gold_invalids": num_invalids_cpu[qi]
                }
                gold_qa_pairs.append(gold_question_dict)
            self.metric(gold_qa_pairs, filtered_beam)
            # TODO print the stuff
            print("\n" + " ".join(metadata["sentence_tokens"]))
            print("\nGOLD:")
            for gold_entry in gold_qa_pairs:
                print(" ".join(gold_entry["question"]))
                span_strings = [" ".join(metadata["sentence_tokens"][span.start() : (span.end() + 1)]) for span in gold_entry["answer_spans"]]
                print("\n   " + "\n   ".join(span_strings))
            print("\nPREDICTED:")
            for entry in full_beam:
                if entry["question_prob"] > 0.02:
                    print("\n" + ("%4.2f (%4.2f) " % (entry["question_prob"], entry["invalidity_prob"])) + " ".join(entry["question"]))
                    sorted_spans = sorted(entry["spans"], key = lambda t: t[1], reverse = True)
                    span_strings = [" ".join(metadata["sentence_tokens"][span.start() : (span.end() + 1)]) + (" (%.2f)" % prob) for span, prob in sorted_spans if prob > 0.01]
                    print("   " + ("\n   ").join(span_strings))

        return {
            "full_beam": full_beam,
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
            valid_spans = [span for span, prob in entry["spans"] if prob >= self.span_threshold]
            novel_spans = [span for span in valid_spans if not has_overlap(span)]
            if(is_valid and len(novel_spans) > 0):
                filtered_entry = {
                    "question": entry["question"],
                    "question_prob": entry["question_prob"],
                    "spans": novel_spans
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
        span_threshold = params.pop("span_threshold", 0.5)
        invalid_threshold = params.pop("invalid_threshold", 0.2)

        return QfirstParser(vocab,
                            question_generator = question_generator, question_answerer = question_answerer,
                            max_beam_size = max_beam_size,
                            question_threshold = question_threshold,
                            span_threshold = span_threshold, invalid_threshold = invalid_threshold)
