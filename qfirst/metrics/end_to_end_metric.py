from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.training.metrics.metric import Metric

import math

from qfirst.data.util import get_slot_label_namespace

class EndToEndMetric(Metric):
    def __init__(self):
        self.reset()

    def reset(self):
        self._gold_question_counts = []
        self._pred_question_counts = []
        self._gold_span_counts = []
        self._pred_span_counts = []
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
        self._q_span_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        self._e2e_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }
        self._q_e2e_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }

    def __call__(self,
                 gold_qa_pairs,
                 pred_qa_pairs):

        self._gold_question_counts.append(len(gold_qa_pairs))
        self._pred_question_counts.append(len(pred_qa_pairs))
        self._gold_span_counts.extend([len(qa["answer_spans"]) for qa in gold_qa_pairs])
        self._pred_span_counts.extend([len(qa["answer_spans"]) for qa in pred_qa_pairs])

        def update_conf(conf, true, positive, n = 1):
            if true and positive:
                conf["tp"] += n
            elif true and not positive:
                conf["tn"] += n
            elif not true and positive:
                conf["fp"] += n
            elif not true and not positive:
                conf["fn"] += n
            else:
                print("error: should never happen")
            return

        gold_qs = set([" ".join(qa["question"]) for qa in gold_qa_pairs])
        pred_qs = set([" ".join(qa["question"]) for qa in pred_qa_pairs])
        # not recording true negatives (really huge space, and not needed for P/R/F1)
        tp_qs = gold_qs & pred_qs
        fn_qs = gold_qs - pred_qs
        fp_qs = pred_qs - gold_qs
        update_conf(self._question_conf, true = True,  positive = True,  n = len(tp_qs))
        update_conf(self._question_conf, true = False, positive = False, n = len(fn_qs))
        update_conf(self._question_conf, true = False, positive = True,  n = len(fp_qs))

        gold_spans = set([s for qa in gold_qa_pairs for s in qa["answer_spans"]])
        pred_spans = set([s for qa in pred_qa_pairs for s in qa["answer_spans"]])
        update_conf(self._span_conf, true = True,  positive = True,  n = len(gold_spans & pred_spans))
        update_conf(self._span_conf, true = False, positive = False, n = len(gold_spans - pred_spans))
        update_conf(self._span_conf, true = False, positive = True,  n = len(pred_spans - gold_spans))

        for gold_qa in gold_qa_pairs:
            if " ".join(gold_qa["question"]) in fn_qs:
                update_conf(self._e2e_conf, true = False, positive = False, n = len(gold_qa["answer_spans"]))
                update_conf(self._q_e2e_conf, true = False, positive = False, n = 1)
            for pred_qa in pred_qa_pairs:
                if gold_qa["question"] == pred_qa["question"]:
                    q_gold_spans = set(gold_qa["answer_spans"])
                    q_pred_spans = set(pred_qa["answer_spans"])
                    shared_spans = q_gold_spans & q_pred_spans
                    missed_gold_spans = q_gold_spans - q_pred_spans
                    erroneously_predicted_spans = q_pred_spans - q_gold_spans
                    def update(conf):
                        update_conf(conf, true = True,  positive = True,  n = len(shared_spans))
                        update_conf(conf, true = False, positive = False, n = len(missed_gold_spans))
                        update_conf(conf, true = False, positive = True,  n = len(erroneously_predicted_spans))
                    update(self._q_span_conf)
                    update(self._e2e_conf)
                    if len(shared_spans) > 0:
                        update_conf(self._q_e2e_conf, true = True, positive = True, n = 1)
                    else:
                        update_conf(self._q_e2e_conf, true = False, positive = True, n = 1)
                        update_conf(self._q_e2e_conf, true = False, positive = False, n = 1)

        for pred_qa in pred_qa_pairs:
            if " ".join(pred_qa["question"]) in fp_qs:
                update_conf(self._e2e_conf, true = False, positive = True,  n = len(pred_qa["answer_spans"]))
                update_conf(self._q_e2e_conf, true = False, positive = True,  n = 1)


    def get_metric(self, reset=False):
        def stats(conf):
            tp = conf["tp"]
            fp = conf["fp"]
            tn = conf["tn"]
            fn = conf["fn"]
            precision = 0.
            if tp + fp > 0.0:
                precision = tp / (tp + fp)
            recall = 0.
            if tp + tn > 0.0:
                recall = tp / (tp + fn)
            f1 = 0.
            if precision + recall > 0.0:
                f1 = 2 * (precision * recall) / (precision + recall)
            return {
                "precision": precision,
                "recall": recall,
                "f1": f1
            }

        from numpy import float64
        stats_dict = {
            "gold-qs-per-verb": float64(sum(self._gold_question_counts)) / len(self._gold_question_counts),
            "pred-qs-per-verb": float64(sum(self._pred_question_counts)) / len(self._pred_question_counts),
            "gold-spans-per-q": float64(sum(self._gold_span_counts)) / len(self._gold_span_counts),
            "pred-spans-per-q": float64(sum(self._pred_span_counts)) / len(self._pred_span_counts)
        }

        question_dict = { ("q-%s" % k): v for k, v in stats(self._question_conf).items() }
        span_dict = { ("a-%s" % k): v for k, v in stats(self._span_conf).items() }
        q_span_dict = { ("qa-%s" % k): v for k, v in stats(self._q_span_conf).items() }
        e2e_dict = { ("e2e-%s" % k): v for k, v in stats(self._e2e_conf).items() }
        q_e2e_dict = { ("q-e2e-%s" % k): v for k, v in stats(self._q_e2e_conf).items() }

        if reset:
            self.reset()

        return {**stats_dict, **question_dict, **span_dict, **q_span_dict, **q_e2e_dict, **e2e_dict}
