from typing import Dict, List, Optional, Set, Tuple

import torch
import networkx as nx
import random

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.data.vocabulary import Vocabulary
from allennlp.training.metrics.metric import Metric

import math

from nrl.common.span import Span

# P/R/F1 of spans above thresholds.
# P/R/F1 of invalid judgments above thresholds
class AnswerMetric(Metric):
    def __init__(self,
                 span_thresholds = [.33],
                 invalid_thresholds = [.11],
                 proportion_invalid_answers = 0.01):
        self._span_thresholds = span_thresholds
        self._invalid_thresholds = invalid_thresholds
        self._proportion_invalid_answers = proportion_invalid_answers

        self.reset()

    def reset(self):
        self._span_confs = [{
            "threshold": t,
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        } for t in self._span_thresholds]
        self._invalid_confs = [{
            "threshold": t,
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        } for t in self._invalid_thresholds]
        self._top_invalid_conf = {
            "threshold": "top",
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        self._valid_spans_conf = {
            "threshold": "valid",
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        self._top_acc = {
            "correct": 0,
            "total": 0
        }
        self._top_span_acc = {
            "correct": 0,
            "total": 0
        }
        self._top_acc_relaxed = {
            "correct": 0,
            "total": 0
        }
        self._gold_spans_max_coverage = {
            "covered": 0,
            "true": 0
        }

    def __call__(self,
                 span_probs, # List[List[(Span, float)]]
                 question_labels,  # dicts corresponding to JSON object
                 invalidity_probs,
                 num_invalids,
                 num_answers):

        def update_conf(conf, true, positive):
            if true and positive:
                conf["tp"] += 1
            elif true and not positive:
                conf["tn"] += 1
            elif (not true) and positive:
                conf["fp"] += 1
            elif (not true) and not positive:
                conf["fn"] += 1
            else:
                print("error: should never happen")
            return

        def update_acc(acc, correct):
            acc["total"] += 1
            if correct:
                acc["correct"] += 1
            return

        def update_coverage(cov, covered, true):
            cov["covered"] += covered
            cov["true"] += true
            return

        invalidity_labels = (num_invalids.float() / num_answers.float()) >= self._proportion_invalid_answers
        for conf in self._invalid_confs:
            invalidity_preds = invalidity_probs >= conf["threshold"]
            for b in range(invalidity_probs.size(0)):
                positive = bool(invalidity_preds[b].item())
                true = positive == bool(invalidity_labels[b].item())
                update_conf(conf, true, positive)

        for i, (spans_with_probs, question_label) in enumerate(zip(span_probs, question_labels)):
            spans_in_beam = [span for span, prob in spans_with_probs]
            top_span, top_span_score = max(spans_with_probs, key = lambda t: t[1])
            invalid_score = invalidity_probs[i].item()
            top_invalid = invalid_score > top_span_score

            gold_invalid = invalidity_labels[i].item()
            gold_spans = set([
                Span(s[0], s[1] - 1) for aj in question_label["answerJudgments"] if aj["isValid"] for s in aj["spans"]
            ])

            num_gold_spans_in_beam = len([s for s in spans_in_beam if s in gold_spans])
            update_coverage(self._gold_spans_max_coverage, num_gold_spans_in_beam, len(gold_spans))

            update_conf(self._top_invalid_conf, gold_invalid == top_invalid, top_invalid)

            top_correct = (top_invalid and gold_invalid) or (
                (not top_invalid) and (not gold_invalid) and (top_span in gold_spans))
            update_acc(self._top_acc, top_correct)

            top_correct_relaxed = (top_invalid and gold_invalid) or (top_span in gold_spans)
            update_acc(self._top_acc_relaxed, top_correct_relaxed)

            for conf in self._span_confs:
                for span, prob in spans_with_probs:
                    positive = prob >= conf["threshold"]
                    true = (span in gold_spans) == positive
                    update_conf(conf, true, positive)

            for span, prob in spans_with_probs:
                positive = prob >= invalid_score
                true = ((span in gold_spans) and not gold_invalid) == positive
                update_conf(self._valid_spans_conf, true, positive)

            if len(gold_spans) > 0:
                update_acc(self._top_span_acc, top_span in gold_spans)

    def get_metric(self, reset=False):

        def stats(conf):
            thresh = "%.2f" % conf["threshold"] if isinstance(conf["threshold"], float) else str(conf["threshold"])
            tp = conf["tp"]
            fp = conf["fp"]
            tn = conf["tn"]
            fn = conf["fn"]
            precision = 0.
            if tp + fp > 0.0:
                precision = tp / (tp + fp)
            recall = 0.
            if tp + fn > 0.0:
                recall = tp / (tp + fn)
            f1 = 0.
            if precision + recall > 0.0:
                f1 = 2 * (precision * recall) / (precision + recall)
            mccNum = (tp * tn) - (fp * fn)
            mccDenom = math.sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
            mcc = 0.
            if abs(mccDenom) > 0.0:
                mcc = mccNum / mccDenom
            return {
                ("precision-%s" % thresh): precision,
                ("recall-%s" % thresh): recall,
                ("f1-%s" % thresh): f1,
                ("mcc-%s" % thresh): mcc
            }
        def get_acc(acc):
            return acc["correct"] / acc["total"] if acc["total"] > 0 else 0.0
        def get_cov(cov):
            return cov["covered"] / cov["true"] if cov["true"] > 0 else 0.0


        span_dict = { ("span-%s" % k): v for conf in self._span_confs for k, v in stats(conf).items() }
        valid_span_dict = { ("span-%s" % k): v for k, v in stats(self._valid_spans_conf).items() }
        invalid_dict = { ("invalid-%s" % k): v for conf in self._invalid_confs for k, v in stats(conf).items() }
        top_invalid_dict = { ("invalid-%s" % k): v for k, v in stats(self._top_invalid_conf).items() }
        other_metrics = {
            "top-acc": get_acc(self._top_acc),
            "top-span-acc": get_acc(self._top_span_acc),
            "top-acc-relaxed": get_acc(self._top_acc_relaxed),
            "gold-spans-not-pruned": get_cov(self._gold_spans_max_coverage)
        }

        if reset:
            self.reset()

        return {**span_dict, **valid_span_dict, **invalid_dict, **top_invalid_dict, **other_metrics }
