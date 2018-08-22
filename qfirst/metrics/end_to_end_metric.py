from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.training.metrics.metric import Metric

import math

from qfirst.data.util import get_slot_label_namespace

class EndToEndMetric(Metric):
    def __init__(self,
                 slot_names: List[str],
                 question_thresholds: List[float] = [0.005, 0.01, 0.02, 0.1]):
        self._slot_names = slot_names
        self._question_thresholds = question_thresholds

        self.reset()

    def reset(self):
        self._question_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        self._span_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        # TODO answer span metric that relies on correspondence between Qs and As

    def __call__(self,
                 gold_qa_pairs, # TODO
                 predicted_qa_pairs):

        def update_conf(conf, true, positive):
            if true and positive:
                conf["tp"] += 1
            elif true and not positive:
                conf["tn"] += 1
            elif not true and positive:
                conf["fp"] += 1
            elif not true and not positive:
                conf["fn"] += 1
            else:
                print("error: should never happen")
            return

        return


    def get_metric(self, reset=False):
        if reset:
            self.reset()
        return {}
