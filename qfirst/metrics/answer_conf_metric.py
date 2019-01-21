from typing import Dict, List, Optional, Set, Tuple

import torch
import networkx as nx
import random

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.data.vocabulary import Vocabulary
from allennlp.training.metrics.metric import Metric

import math

from nrl.common.span import Span

from qfirst.metrics import AnswerMetric

# P/R/F1 of spans above thresholds.
# P/R/F1 of invalid judgments above thresholds
class AnswerConfMetric(AnswerMetric):
    def __init__(self,
                 span_thresholds = [.05, .10, .15, .20, .25, .30, .35, .40, .45, .50],
                 invalid_thresholds = [.05, .10, .15, .20, .25, .30, .35, .40, .45, .50],
                 proportion_invalid_answers = 0.01,
                 verbose = False):
        self._span_thresholds = span_thresholds
        self._invalid_thresholds = invalid_thresholds
        self._proportion_invalid_answers = proportion_invalid_answers
        self._verbose = verbose

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
        self._top_token_f1 = {
            "sum": 0.0,
            "count": 0
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

        def get_token_f1(pred, gold):
            p1, p2 = (pred.start(), pred.end())
            g1, g2 = (gold.start(), gold.end())
            precision = sum([1 for x in range(p1, p2+1) if x >= g1 and x <= g2]) / (p2 - p1 + 1)
            recall    = sum([1 for x in range(g1, g2+1) if x >= p1 and x <= p2]) / (g2 - g1 + 1)
            f1 = 0.
            if precision + recall > 0.0:
                f1 = 2 * (precision * recall) / (precision + recall)
            return f1

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

            self._top_token_f1["count"] += 1
            if gold_invalid:
                if top_invalid:
                    self._top_token_f1["sum"] += 1.0
                # else add 0
            else:
                if not top_invalid:
                    max_f1 = max([get_token_f1(top_span, g) for g in gold_spans])

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
                "threshold": thresh,
                "precision": precision,
                "recall": recall,
                "f1": f1,
                "mcc": mcc
            }
        def get_acc(acc):
            return acc["correct"] / acc["total"] if acc["total"] > 0 else 0.0
        def get_cov(cov):
            return cov["covered"] / cov["true"] if cov["true"] > 0 else 0.0
        def get_token_f1(tok_f1):
            return tok_f1["sum"] / tok_f1["count"] if tok_f1["count"] > 0 else 0.0

        best_span_dict = max([stats(conf) for conf in self._span_confs], key = lambda d: d["f1"])
        span_dict = { ("span-%s" % k): v for k, v in best_span_dict.items() if isinstance(v, float) }
        best_invalid_dict = max([stats(conf) for conf in self._invalid_confs], key = lambda d: d["f1"])
        invalid_dict = { ("invalid-%s" % k): v for k, v in best_invalid_dict.items() if isinstance(v, float) }

        res = None
        if self._verbose:
            valid_span_dict = { ("span-%s-valid" % k): v for k, v in stats(self._valid_spans_conf).items() if isinstance(v, float) }
            top_invalid_dict = { ("top-invalid-%s" % k): v for k, v in stats(self._top_invalid_conf).items() if isinstance(v, float) }
            other_metrics = {
                "top-acc": get_acc(self._top_acc),
                "top-span-acc": get_acc(self._top_span_acc),
                "top-acc-relaxed": get_acc(self._top_acc_relaxed),
                "top-token-f1": get_token_f1(self._top_token_f1),
                "gold-spans-not-pruned": get_cov(self._gold_spans_max_coverage),
                "span_threshold": float(best_span_dict["threshold"]),
                "invalid_threshold": float(best_invalid_dict["threshold"])
            }
            res = {**span_dict, **valid_span_dict, **invalid_dict, **top_invalid_dict, **other_metrics}
        else:
            other_metrics = {
                "gold-spans-not-pruned": get_cov(self._gold_spans_max_coverage),
                "span-threshold": float(best_span_dict["threshold"]),
                "invalid-threshold": float(best_invalid_dict["threshold"]),
                "top-token-f1": get_token_f1(self._top_token_f1)
            }
            res = {**span_dict, **invalid_dict, **other_metrics}

        if reset:
            self.reset()

        return res

    @classmethod
    def from_params(cls, params) -> 'AnswerMetric':
        span_thresholds = params.pop("span_thresholds", [.05, .10, .15, .20, .25, .30, .35, .40, .45, .50])
        invalid_thresholds = params.pop("invalid_thresholds", [.05, .10, .15, .20, .25, .30, .35, .40, .45, .50])
        proportion_invalid_answers = params.pop("proportion_invalid_answers", 0.01)
        verbose = params.pop("verbose", False)
        return AnswerMetric(
            span_thresholds = span_thresholds,
            invalid_thresholds = invalid_thresholds,
            proportion_invalid_answers = proportion_invalid_answers,
            verbose = verbose)