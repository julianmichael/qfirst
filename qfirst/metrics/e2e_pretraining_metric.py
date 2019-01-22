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
        def make_confs():
            return [{
                "threshold": threshold,
                "tp": 0,
                "tn": 0,
                "fp": 0,
                "fn": 0
            } for threshold in self._thresholds]
        self._all_clause_confs = make_confs()
        self._all_span_confs = make_confs()
        self._gold_span_coverage = {
            "covered": 0,
            "true": 0
        }
        self._all_qarg_confs = make_confs()
        self._all_tan_confs = make_confs()
        self._all_animacy_confs = make_confs()
        return

    def __call__(self,
                 clause_probs,
                 clause_labels,
                 span_probs, # List[List[(Span, float)]]
                 span_labels,
                 qarg_probs,
                 qarg_labels,
                 tan_probs, tan_labels,
                 animacy_probs, animacy_labels,
                 metadata):
        def compute_confs(confs, probs, labels, num_true = None):
            if num_true is None:
                num_true = labels.sum().item()
            for conf in confs:
                t = conf["threshold"]
                predictions = (probs >= t).long()
                num_positive = predictions.sum().item()
                num_tp = torch.min(predictions, labels.long()).sum().item()
                conf["tp"] += num_tp
                conf["fn"] += num_true - num_tp
                conf["fp"] += num_positive - num_tp

        compute_confs(self._all_clause_confs, clause_probs, clause_labels)

        num_gold_spans = sum(
            len(set([
                Span(s[0], s[1] - 1)
                for question_label in m["question_labels"]
                for aj in question_label["answerJudgments"] if aj["isValid"]
                for s in aj["spans"]
            ]))
            for m in metadata
        )
        compute_confs(self._all_span_confs, span_probs, span_labels, num_gold_spans)
        self._gold_span_coverage["true"] += num_gold_spans
        self._gold_span_coverage["covered"] += span_labels.sum().item()

        compute_confs(self._all_qarg_confs, qarg_probs, qarg_labels)
        compute_confs(self._all_tan_confs, tan_probs, tan_labels)
        compute_confs(self._all_animacy_confs, animacy_probs, animacy_labels)

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
        qarg_metric_dict = max([get_stats(conf) for conf in self._all_qarg_confs], key = lambda d: d["f1"])
        tan_metric_dict = max([get_stats(conf) for conf in self._all_tan_confs], key = lambda d: d["f1"])
        animacy_metric_dict = max([get_stats(conf) for conf in self._all_animacy_confs], key = lambda d: d["f1"])
        metric_dict = {
            **{  ("clause-%s" % k) : v for k, v in  clause_metric_dict.items() },
            **{    ("span-%s" % k) : v for k, v in    span_metric_dict.items() },
            **{    ("qarg-%s" % k) : v for k, v in    qarg_metric_dict.items() },
            **{     ("tan-%s" % k) : v for k, v in     tan_metric_dict.items() },
            **{ ("animacy-%s" % k) : v for k, v in animacy_metric_dict.items() },
            "gold_spans_in_beam": get_cov(self._gold_span_coverage)
        }

        if reset:
            self.reset()
        return metric_dict
