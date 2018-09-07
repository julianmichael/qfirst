from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.training.metrics.metric import Metric

import math
import copy

from qfirst.data.util import get_slot_label_namespace
from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.dense_end_to_end_metric import DenseEndToEndMetric

from qfirst.util.question_conversion import get_abst_question_slots

class TemplatingE2EMetric(Metric):
    def __init__(self,
                 use_dense_metric: bool = False,
                 template_slots: List[str] = ["abst-wh", "abst-subj", "abst-verb", "abst-obj", "prep", "abst-obj2"],
                 are_questions_already_templated: bool = False):

        self._are_questions_already_templated = are_questions_already_templated

        def make_metric():
            if use_dense_metric:
                return DenseEndToEndMetric()
            else:
                return EndToEndMetric()

        if are_questions_already_templated:
            self._downstream_full_metric = None
        else:
            self._downstream_full_metric = make_metric()
        self._downstream_templated_metric = make_metric()

        self._template_slots = template_slots

        self.reset()

    def reset(self):
        if self._downstream_full_metric is not None:
            self._downstream_full_metric.reset()
        self._downstream_templated_metric.reset()

    # def _templatize_qa_pairs(qa_pairs):
    def _get_question_template(self, slots):
        all_slots = {**slots, **get_abst_question_slots(slots)}
        question_template = [all_slots[n] for n in self._template_slots]
        return question_template

    def _templatize_qa_pairs(self, qa_pairs):
        templated_qa_pairs = []
        for qa_pair in qa_pairs:
            question_template = self._get_question_template(qa_pair["question_slots"])
            question_template_slots = {n: v for n, v in zip(self._template_slots, question_template)}
            new_qa_pair = copy.deepcopy(qa_pair)
            new_qa_pair["question"] = question_template
            new_qa_pair["question_slots"] = question_template_slots
            is_repeat_question = False
            for other_qa in templated_qa_pairs:
                if other_qa["question"] == question_template:
                    other_qa["answer_spans"].extend(new_qa_pair["answer_spans"])
                    is_repeat_question = True
            if not is_repeat_question:
                templated_qa_pairs.append(new_qa_pair)
        return templated_qa_pairs

    def __call__(self,
                 gold_qa_pairs,
                 pred_qa_pairs):

        if self._are_questions_already_templated: # we have full questions as input
            pred_templated_qa_pairs = pred_qa_pairs
        else:
            # assert self._downstream_full_metric is not None
            self._downstream_full_metric(gold_qa_pairs, pred_qa_pairs)
            pred_templated_qa_pairs = self._templatize_qa_pairs(pred_qa_pairs)

        gold_templated_qa_pairs = self._templatize_qa_pairs(gold_qa_pairs)

        self._downstream_templated_metric(gold_templated_qa_pairs, pred_templated_qa_pairs)

    def get_metric(self, reset=False):
        orig_templated_dict = self._downstream_templated_metric.get_metric(reset)
        templated_dict = {("abst-%s" % k): v for k, v in orig_templated_dict.items()}

        if self._are_questions_already_templated:
            result_dict = templated_dict
        else:
            # assert self._downstream_full_metric is not None
            full_dict = self._downstream_full_metric.get_metric(reset)
            result_dict = {**full_dict, **templated_dict}

        if reset:
            self.reset()

        return result_dict
