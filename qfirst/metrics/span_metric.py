from typing import Dict, List, Optional, Set, Tuple

from allennlp.common import Registrable
from allennlp.training.metrics.metric import Metric

import math

from qfirst.common.span import Span

class SpanMetric(Metric, Registrable):
    def __init__(self,
                 thresholds = [.05, .15, .25, .35, .40, .45, .50, .55, .60, .65, .75, .85, .95]):
        self._thresholds = thresholds
        self.reset()

    def reset(self):
        def make_confs(thresholds):
            return [{
                "threshold": t,
                "tp": 0,
                "tn": 0,
                "fp": 0,
                "fn": 0
            } for t in thresholds]
        self._confs = make_confs(self._thresholds)
        self._gold_spans_max_coverage = {
            "covered": 0,
            "true": 0
        }

    def __call__(self,
                 span_probs, # List[List[(Span, float)]]: batch size, num predicted spans
                 gold_span_sets):  # dicts corresponding to JSON object

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

        def update_coverage(cov, covered, true):
            cov["covered"] += covered
            cov["true"] += true
            return

        for i, (spans_with_probs, gold_spans) in enumerate(zip(span_probs, gold_span_sets)):
            spans_in_beam = [span for span, prob in spans_with_probs]
            num_gold_spans_in_beam = len([s for s in spans_in_beam if s in gold_spans])
            update_coverage(self._gold_spans_max_coverage, num_gold_spans_in_beam, len(gold_spans))

            for conf in self._confs:
                for span, prob in spans_with_probs:
                    positive = prob >= conf["threshold"]
                    true = (span in gold_spans) == positive
                    update_conf(conf, true, positive)

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
                "threshold": conf["threshold"],
                "precision": precision,
                "recall": recall,
                "f1": f1,
                "mcc": mcc
            }
        def get_cov(cov):
            return cov["covered"] / cov["true"] if cov["true"] > 0 else 0.0

        stats_dict = max([stats(conf) for conf in self._confs], key = lambda d: d["f1"])
        output_dict = {
            **{ k: v for k, v in stats_dict.items() if isinstance(v, float) },
            "gold-spans-not-pruned": get_cov(self._gold_spans_max_coverage)
        }

        if reset:
            self.reset()

        return output_dict
