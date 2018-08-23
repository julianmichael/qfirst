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
        self._span_matching_conf = {
            "tp": 0,
            "tn": 0,
            "fp": 0,
            "fn": 0
        }

    def __call__(self,
                 gold_qa_pairs, # TODO
                 pred_qa_pairs):

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
        pred_spans = set([s for qa in pred_qa_pairs for s in qa["spans"]])
        update_conf(self._span_conf, true = True,  positive = True,  n = len(gold_spans & pred_spans))
        update_conf(self._span_conf, true = False, positive = False, n = len(gold_spans - pred_spans))
        update_conf(self._span_conf, true = False, positive = True,  n = len(pred_spans - gold_spans))

        for gold_qa in gold_qa_pairs:
            if " ".join(gold_qa["question"]) in fn_qs:
                update_conf(self._e2e_conf, true = False, positive = False, n = len(gold_qa["answer_spans"]))
            for pred_qa in pred_qa_pairs:
                if gold_qa["question"] == pred_qa["question"]:
                    q_gold_spans = set([s for s in gold_qa["answer_spans"]])
                    q_pred_spans = set([s for s in pred_qa["spans"]])
                    def update(conf):
                        update_conf(conf, true = True,  positive = True,  n = len(q_gold_spans & q_pred_spans))
                        update_conf(conf, true = False, positive = False, n = len(q_gold_spans - q_pred_spans))
                        update_conf(conf, true = False, positive = True,  n = len(q_pred_spans - q_gold_spans))
                    update(self._q_span_conf)
                    update(self._e2e_conf)

        for pred_qa in pred_qa_pairs:
            if " ".join(pred_qa["question"]) in fp_qs:
                update_conf(self._e2e_conf, true = False, positive = True,  n = len(pred_qa["spans"]))

        # TODO
        # picked_gold, picked_pred = self.get_matches(pred_spans, gold_spans)

        # for g, tup in picked_gold.items():
        #     f1, pred, gold = tup
        #     if self._match_heuristic(pred, gold):
        #         self._covered[i] += 1

        #         wh = g.slots[0]
        #         self._wh_covered[i].setdefault(wh, 0)
        #         self._wh_covered[i][wh] += 1
        #         if wh in ['what', 'who']:
        #             self._wh_covered[i].setdefault("core", 0)
        #             self._wh_covered[i]["core"] += 1
        #         else:
        #             self._wh_covered[i].setdefault("aux", 0)
        #             self._wh_covered[i]["aux"] += 1

        # for pred, tup in picked_pred.items():
        #     f1, g, gold = tup
        #     if self._match_heuristic(pred, gold):
        #         self._correct[i] += 1

    # def get_matches(self, pred_spans, gold_spans):
    #     gold_spans = [g for g in gold_spans if g.text != 'V']

    #     G = nx.Graph()
    #     for s in gold_spans + pred_spans:
    #         G.add_node(s)

    #     max_golds = {}
    #     max_pred_scores = {}
    #     for gold_span_tuple in gold_spans:
    #         for span in pred_spans:
    #             max_f1, max_gold = max([(g.overlap_f1(span), g) for g in gold_span_tuple.all_spans], key = lambda x: x[0])
    #             max_golds[(gold_span_tuple, span)] = (max_f1, max_gold)
    #             if self._match_heuristic(span, max_gold):
    #                 #G.add_edge(gold_span_tuple, span, weight = max_f1)
    #                 G.add_edge(gold_span_tuple, span, weight = 1)
    #                 if span not in max_pred_scores or max_f1 > max_pred_scores[span][0]:
    #                     max_pred_scores[span] = (max_f1, gold_span_tuple, max_gold)

    #     matching = nx.max_weight_matching(G)

    #     matching = [(g, p) for g, p in matching.items() if g in gold_spans]

    #     picked_gold = {}
    #     picked_pred = {}
    #     for g, p in matching:
    #         f1, max_gold = max_golds[(g, p)]
    #         picked_gold[g] = (f1, p, max_gold)
    #         picked_pred[p] = (f1, g, max_gold)

    #     for p, match in max_pred_scores.items():
    #         if p not in picked_pred:
    #             picked_pred[p] = match

    #     return picked_gold, picked_pred

    #     return

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

        question_dict = { ("q-%s" % k): v for k, v in stats(self._question_conf).items() }
        span_dict = { ("a-%s" % k): v for k, v in stats(self._span_conf).items() }
        q_span_dict = { ("qa-%s" % k): v for k, v in stats(self._q_span_conf).items() }

        if reset:
            self.reset()

        return {**question_dict, **span_dict, **q_span_dict}
