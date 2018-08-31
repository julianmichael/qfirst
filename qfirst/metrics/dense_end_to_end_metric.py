from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.training.metrics.metric import Metric

import math

from qfirst.data.util import get_slot_label_namespace

class DenseEndToEndMetric(Metric):
    def __init__(self):
        self.reset()

    def reset(self):
        self._gold_question_counts = []
        self._pred_question_counts = []
        self._gold_span_counts = []
        self._pred_span_counts = []

        self._question_acc = {
            "correct-lb": 0,
            "correct-ub": 0,
            "total": 0
        }

        self._question_with_span_acc = {
            "correct-lb": 0,
            "correct-ub": 0,
            "total": 0
        }

        self._span_acc = {
            "correct-lb": 0,
            "correct-ub": 0,
            "total": 0
        }

    def __call__(self,
                 gold_qa_pairs,
                 pred_qa_pairs):

        self._gold_question_counts.append(len(gold_qa_pairs))
        self._pred_question_counts.append(len(pred_qa_pairs))
        self._gold_span_counts.extend([len(qa["answer_spans"]) for qa in gold_qa_pairs])
        self._pred_span_counts.extend([len(qa["spans"]) for qa in pred_qa_pairs])

        def update_acc(acc, correct_lb, correct_ub, total):
            acc["correct-lb"] += correct_lb
            acc["correct-ub"] += correct_ub
            acc["total"] += total
            return

        gold_qs = set([" ".join(qa["question"]) for qa in gold_qa_pairs])
        pred_qs = set([" ".join(qa["question"]) for qa in pred_qa_pairs])

        for pred_qa in pred_qa_pairs:
            pred_spans = set([s for s in pred_qa["spans"]])
            gold_qa = None
            for candidate_gold_qa in gold_qa_pairs:
                if candidate_gold_qa["question"] == pred_qa["question"]:
                    gold_qa = candidate_gold_qa
            if gold_qa is None:
                update_acc(self._question_acc, 0, 1, 1)
                update_acc(self._question_with_span_acc, 0, 1, 1)
                update_acc(self._span_acc, 0, len(pred_spans), len(pred_spans))
            else:
                if gold_qa["num_gold_invalids"] > 1:
                    update_acc(self._question_acc, 0, 0, 1)
                    update_acc(self._question_with_span_acc, 0, 0, 1)
                    update_acc(self._span_acc, 0, 0, len(pred_spans))
                else:
                    gold_spans = set([s for s in gold_qa["answer_spans"]])
                    correct_spans = pred_spans & gold_spans
                    correct_span_bit = 1 if len(correct_spans) > 0 else 0
                    update_acc(self._question_acc, 1, 1, 1)
                    update_acc(self._question_with_span_acc, correct_span_bit, correct_span_bit, 1)
                    update_acc(self._span_acc, len(correct_spans), len(correct_spans), len(pred_spans))

    def get_metric(self, reset=False):

        def get_acc(acc, label):
            return {
                ("%s-acc-lb" % label): acc["correct-lb"] / acc["total"] if acc["total"] > 0 else 0.0,
                ("%s-acc-ub" % label): acc["correct-ub"] / acc["total"] if acc["total"] > 0 else 0.0
            }

        from numpy import float64

        stats = {
            "gold-qs-per-verb": float64(sum(self._gold_question_counts)) / len(self._gold_question_counts),
            "pred-qs-per-verb": float64(sum(self._pred_question_counts)) / len(self._pred_question_counts),
            "gold-spans-per-q": float64(sum(self._gold_span_counts)) / len(self._gold_span_counts),
            "pred-spans-per-q": float64(sum(self._pred_span_counts)) / len(self._pred_span_counts)
        }

        question_acc = get_acc(self._question_acc, "question")
        question_answer_acc = get_acc(self._question_with_span_acc, "question-answer")
        span_acc = get_acc(self._span_acc, "span")

        if reset:
            self.reset()

        return {**stats, **question_acc, **question_answer_acc, **span_acc}
