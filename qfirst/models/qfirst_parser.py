from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary

from qfirst.modules.question_generator import QuestionGenerator
from qfirst.models.question_answerer import QuestionAnswerer
from qfirst.data.util import get_slot_label_namespace
from qfirst.metrics.beam_metric import BeamMetric

import math

from nrl.common.span import Span

# should receive verb instances from the qasrl dataset reader
@Model.register("qfirst_parser")
class QfirstParser(Model):
    def __init__(self, vocab: Vocabulary,
                 question_generator: QuestionGenerator,
                 question_answerer: QuestionAnswerer,
                 max_beam_size: int = 20,
                 question_minimum_prob: float = 0.01,
                 span_minimum_prob: float = 0.01,
                 metric: BeamMetric = None):
        super(QfirstParser, self).__init__(vocab)
        self.question_generator = question_generator
        self.question_answerer = question_answerer
        self.slot_names = question_answerer.question_encoder.get_slot_names()
        self.max_beam_size = max_beam_size
        self.question_minimum_prob = question_minimum_prob
        self.span_minimum_prob = span_minimum_prob
        self.metric = metric

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_index: torch.LongTensor,
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

        beam_slots, beam_log_probs = self.question_generator.beam_decode_single(text, predicate_indicator, self.max_beam_size, self.question_minimum_prob)
        beam_size = beam_log_probs.size(0)
        full_beam = []
        if beam_size > 0:
            text_expanded = { k: v.expand(beam_size, -1, -1) for k, v in text.items() }
            predicate_index_expanded = predicate_index.expand(beam_size, -1)
            predicate_indicator_expanded = predicate_indicator.expand(beam_size, -1)
            answerer_forward_outputs = self.question_answerer.forward(text_expanded, predicate_index_expanded, predicate_indicator_expanded, **beam_slots)
            answerer_outputs = self.question_answerer.decode(answerer_forward_outputs)
            for i in range(beam_size):
                question_slots = {
                    n: self.vocab.get_index_to_token_vocabulary(get_slot_label_namespace(n))[beam_slots[n][i].item()]
                    for n in self.slot_names
                }
                question = [question_slots[n] for n in self.slot_names]
                answer_spans = [(s, p) for s, p in answerer_outputs["spans"][i]
                                if p >= self.span_minimum_prob]
                beam_entry_dict = {
                    "question": question,
                    "question_slots": question_slots,
                    "question_prob": math.exp(beam_log_probs[i]),
                    "answer_spans": answer_spans,
                    "invalidity_prob": answerer_outputs["invalid_prob"][i].item()
                }
                full_beam.append(beam_entry_dict)

        if num_answers is not None: # we have gold datums
            metadata = metadata[0]
        #    num_answers_cpu = num_answers[0].cpu()
        #    num_invalids_cpu = num_invalids[0].cpu()
        #    gold_qa_pairs = []
        #    for qi, question_label in enumerate(metadata["question_labels"]):
        #        def get_spans(spansJson):
        #            return [Span(s[0], s[1]-1) for s in spansJson]
        #        all_gold_answer_spans = [s for ans in question_label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"])]
        #        distinct_gold_answer_spans = list(set(all_gold_answer_spans))
        #        gold_question_dict = {
        #            "question_slots": question_label["questionSlots"],
        #            "question": [question_label["questionSlots"][n] for n in self.slot_names],
        #            "answer_spans": distinct_gold_answer_spans,
        #            "num_answers": num_answers_cpu[qi],
        #            "num_gold_invalids": num_invalids_cpu[qi]
        #        }
        #        gold_qa_pairs.append(gold_question_dict)

            if self.metric is not None:
                self.metric(None, full_beam, metadata)
                # self.metric(gold_qa_pairs, full_beam, metadata)

        return {
            "full_beam": full_beam,
        }

    def get_metrics(self, reset: bool = False):
        if self.metric is not None:
            return self.metric.get_metric(reset=reset)
        else:
            return {}

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QfirstParser':
        question_generator = Model.from_params(vocab, params.pop("question_generator"))
        question_answerer = Model.from_params(vocab, params.pop("question_answerer"))
        max_beam_size = params.pop("max_beam_size", 20)
        question_minimum_prob = params.pop("question_minimum_prob", 0.01)
        span_minimum_prob = params.pop("span_minimum_prob", 0.01)
        metric_config = params.pop("metric", None)
        metric = None
        if metric_config is not None:
            metric = BeamMetric.from_params(metric_config)

        return QfirstParser(vocab,
                            question_generator = question_generator, question_answerer = question_answerer,
                            max_beam_size = max_beam_size,
                            question_minimum_prob = question_minimum_prob,
                            span_minimum_prob = span_minimum_prob,
                            metric = metric)
