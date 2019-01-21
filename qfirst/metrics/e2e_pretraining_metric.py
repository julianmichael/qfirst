from typing import Optional, List

import torch

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.beam_metric import BeamMetric

import json

from nrl.common.span import Span

class E2EPretrainingMetric(Metric):
    def __init__(self,
                 thresholds: List[float] = [0.25, 0.5, 0.75]):
        # self._file_path = file_path
        self._thresholds = thresholds
        self.reset()

    def reset(self):
        self._all_clause_confs = [{
            "threshold": threshold,
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        } for threshold in self._thresholds]
        self._all_span_confs = [{
            "threshold": threshold,
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        } for threshold in self._thresholds]
        self._gold_span_coverage = {
            "covered": 0,
            "true": 0
        }
        return

    def __call__(self,
                 clause_probs,
                 clause_labels,
                 span_probs, # List[List[(Span, float)]]
                 span_labels,
                 metadata):
        for conf in self._all_clause_confs:
            t = conf["threshold"]
            clause_predictions = (clause_probs >= t).long()
            clause_num_positive = clause_predictions.sum().item()
            clause_num_true = clause_labels.sum().item()
            clause_num_tp = torch.min(clause_predictions, clause_labels.long()).sum().item()
            conf["tp"] += clause_num_tp
            conf["fn"] += clause_num_true - clause_num_tp
            conf["fp"] += clause_num_positive - clause_num_tp

        num_gold_spans = sum(
            len(set([
                Span(s[0], s[1] - 1)
                for question_label in m["question_labels"]
                for aj in question_label["answerJudgments"] if aj["isValid"]
                for s in aj["spans"]
            ]))
            for m in metadata
        )
        for conf in self._all_span_confs:
            t = conf["threshold"]
            span_predictions = (span_probs >= t).long()
            span_num_positive = span_predictions.sum().item()
            span_num_true = num_gold_spans
            span_num_tp = torch.min(span_predictions, span_labels.long()).sum().item()
            conf["tp"] += span_num_tp
            conf["fn"] += span_num_true - span_num_tp
            conf["fp"] += span_num_positive - span_num_tp
        self._gold_span_coverage["true"] += num_gold_spans
        self._gold_span_coverage["covered"] += span_labels.sum().item()

    def get_metric(self, reset = False):
        def get_stats(conf):
            # thresh = "%.2f" % conf["threshold"] if isinstance(conf["threshold"], float) else str(conf["threshold"])
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
            return {
                "threshold": conf["threshold"],
                "precision": precision,
                "recall": recall,
                "f1": f1
            }
        def get_cov(cov):
            return cov["covered"] / cov["true"] if cov["true"] > 0 else 0.0

        clause_metric_dict = max([get_stats(conf) for conf in self._all_clause_confs], key = lambda d: d["f1"])
        span_metric_dict = max([get_stats(conf) for conf in self._all_span_confs], key = lambda d: d["f1"])
        metric_dict = {
            **{ ("clause-%s" % k) : v for k, v in clause_metric_dict.items() },
            **{   ("span-%s" % k) : v for k, v in   span_metric_dict.items() },
            "gold_spans_in_beam": get_cov(self._gold_span_coverage)
        }

        if reset:
            self.reset()
        return metric_dict
