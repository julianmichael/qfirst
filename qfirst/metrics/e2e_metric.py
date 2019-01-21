from typing import Optional, List

import torch

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.beam_metric import BeamMetric

import json

class E2EMetric(Metric):
    def __init__(self,
                 thresholds: List[float] = [0.25, 0.5, 0.75]):
        # self._file_path = file_path
        self._thresholds = thresholds
        self.reset()

    def reset(self):
        # self._sentence_jsons = {}
        self._all_confs = [{
            "threshold": threshold,
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        } for threshold in self._thresholds]
        self._gold_qa_coverage = {
            "covered": 0,
            "true": 0
        }
        return

    def __call__(self,
                 probs,
                 labels,
                 metadata):
        for conf in self._all_confs:
            t = conf["threshold"]
            predictions = (probs >= t).long()
            num_positive = predictions.sum().item()
            num_true = labels.sum().item()
            num_tp = torch.min(predictions, labels).sum().item()
            conf["tp"] += num_tp
            conf["fn"] += num_true - num_tp
            conf["fp"] += num_positive - num_tp
        for meta_dict in metadata:
            self._gold_qa_coverage["true"] += len(meta_dict["gold_set"])
        self._gold_qa_coverage["covered"] += labels.sum().item()

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

        metric_dict = max([get_stats(conf) for conf in self._all_confs], key = lambda d: d["f1"])
        metric_dict["gold_in_beam"] = get_cov(self._gold_qa_coverage)

        if reset:
            self.reset()
        return metric_dict
