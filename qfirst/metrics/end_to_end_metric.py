from typing import Optional, List

import torch

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

import json

from qfirst.common.span import Span

class EndToEndMetric(Metric):
    def __init__(self,
                 thresholds: List[float] = [0.25, 0.5, 0.75]):
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
        self._span_confs = make_confs()
        self._question_confs = make_confs()
        self._span_strict_confs = make_confs()
        self._question_strict_confs = make_confs()
        self._gold_qa_coverage = {
            "covered": 0,
            "true": 0
        }
        return

    def __call__(self,
                 num_gold_covered,
                 batch_of_beams,
                 metadata):
        num_true = sum([len(m["gold_set"]) for m in metadata])
        self._gold_qa_coverage["true"] += num_true
        self._gold_qa_coverage["covered"] += num_gold_covered

        for batch_index in range(len(batch_of_beams)):
            beam = batch_of_beams[batch_index]
            gold_set = metadata[batch_index]["gold_set"]
            sorted_beam = sorted(beam, key = lambda x: -x["total_prob"])
            chosen_entries = []

            for beam_entry in sorted_beam:
                if not any([beam_entry["span"].overlaps(e["span"]) for e in chosen_entries]):
                    chosen_entries.append(beam_entry)
            import itertools
            def get_groups(xs, key):
                return { k : list(vs) for k, vs in itertools.groupby(sorted(xs, key = key), key = key) }
            pred_by_question = get_groups(chosen_entries, lambda x: (x["clause"], x["qarg"]))
            gold_by_question = get_groups(      gold_set, lambda x: (x[0], x[1]))
            pred_by_span     = get_groups(chosen_entries, lambda x: (x["span"]))
            gold_by_span     = get_groups(      gold_set, lambda x: (Span(x[2][0], x[2][1])))
            # span F1, question F1, span F1 counting questions, question F1 counting spans
            questions = set([q for q in pred_by_question] + [q for q in gold_by_question])
            spans = set([s for s in pred_by_span] + [s for s in gold_by_span])
            for q in questions:
                true = q in gold_by_question
                for (conf, strict_conf) in zip(self._question_confs, self._question_strict_confs):
                    t = conf["threshold"]
                    positive = q in pred_by_question and any(x["total_prob"] > t for x in pred_by_question[q])
                    span_matches = true and positive and any(any(x["span"] == Span(y[2][0], y[2][1]) for y in gold_by_question[q]) for x in pred_by_question[q])
                    if true and positive:
                        conf["tp"] += 1
                        if span_matches:
                            strict_conf["tp"] += 1
                        else:
                            strict_conf["fn"] += 1
                            strict_conf["fp"] += 1
                    elif true:
                        conf["fn"] += 1
                        strict_conf["fn"] += 1
                    elif positive:
                        conf["fp"] += 1
                        strict_conf["fp"] += 1
            for s in spans:
                true = s in gold_by_span
                for (conf, strict_conf) in zip(self._span_confs, self._span_strict_confs):
                    t = conf["threshold"]
                    positive = s in pred_by_span and any(x["total_prob"] > t for x in pred_by_span[s])
                    question_matches = true and positive and any(any((x["clause"], x["qarg"]) == (y[0], y[1]) for y in gold_by_span[s]) for x in pred_by_span[s])
                    if true and positive:
                        conf["tp"] += 1
                        if question_matches:
                            strict_conf["tp"] += 1
                        else:
                            strict_conf["fn"] += 1
                            strict_conf["fp"] += 1
                    elif true:
                        conf["fn"] += 1
                        strict_conf["fn"] += 1
                    elif positive:
                        conf["fp"] += 1
                        strict_conf["fp"] += 1

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

        question_dict = max([get_stats(conf) for conf in self._question_confs], key = lambda d: d["f1"])
        question_strict_dict = max([get_stats(conf) for conf in self._question_strict_confs], key = lambda d: d["f1"])
        span_dict = max([get_stats(conf) for conf in self._span_confs], key = lambda d: d["f1"])
        span_strict_dict = max([get_stats(conf) for conf in self._span_strict_confs], key = lambda d: d["f1"])
        gold_in_beam = get_cov(self._gold_qa_coverage)

        if reset:
            self.reset()
        return {
            **{  ("q-%s" % k) : v for k, v in question_dict.items() },
            **{ ("qs-%s" % k) : v for k, v in question_strict_dict.items() },
            **{  ("s-%s" % k) : v for k, v in span_dict.items() },
            **{ ("ss-%s" % k) : v for k, v in span_strict_dict.items() },
            "gold_qas_in_beam": get_cov(self._gold_qa_coverage)
        }
