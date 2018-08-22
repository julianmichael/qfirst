from typing import Dict, List, Optional, Set, Tuple

import torch
import networkx as nx
import random

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.data.vocabulary import Vocabulary
from allennlp.training.metrics.metric import Metric

import math

from nrl.common.span import Span

class AnswerMetric(Metric):
    def __init__(self,
                 span_thresholds = [.33],
                 invalid_thresholds = [.11],
                 proportion_invalid_answers = 0.01,
                 match_heuristic = None):
        self._span_thresholds = span_thresholds
        self._invalid_thresholds = invalid_thresholds
        self._proportion_invalid_answers = proportion_invalid_answers
        self._match_heuristic = match_heuristic or (lambda x, y: x == y)

        self.reset()

    def reset(self):
        # self._total_span_nll = 0.
        # self._total_num_spans = 0
        # self._total_invalid_nll = 0.
        # self._total_num_questions = 0
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
            elif not true and positive:
                conf["fp"] += 1
            elif not true and not positive:
                conf["fn"] += 1
            else:
                print("error: should never happen")
            return

        invalidity_labels = (num_invalids / num_answers) >= self._proportion_invalid_answers
        for conf in self._invalid_confs:
            invalidity_preds = invalidity_probs >= conf["threshold"]
            for b in range(invalidity_probs.size(0)):
                positive = invalidity_preds[b].item()
                true = positive == invalidity_labels[b].item()
                update_conf(conf, true, positive)

        for conf in self._span_confs:
            for spans_with_probs, question_label in zip(span_probs, question_labels):
                true_spans = set([
                    Span(s[0], s[1] - 1) for aj in question_label["answerJudgments"] if aj["isValid"] for s in aj["spans"]
                ])
                for span, prob in spans_with_probs:
                    positive = prob >= conf["threshold"]
                    true = (span in true_spans) == positive
                    update_conf(conf, true, positive)

        # self._total_span_nll += span_nll
        # self._total_num_spans += num_spans
        # self._total_invalid_nll += invalid_nll
        # self._total_num_questions += num_questions

    def get_metric(self, reset=False):

        # perplexity_per_span = math.exp(self._total_span_nll / self._total_num_spans)
        # invalid_perplexity_per_question = math.exp(self._total_invalid_nll / self._total_num_questions)

        # return {
        #     "perplexity-per-span": perplexity_per_span,
        #     "invalid-perplexity-per-question": invalid_perplexity_per_question
        # }

        def stats(conf):
            thresh = conf["threshold"]
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
            mccNum = (tp * tn) - (fp * fn)
            mccDenom = math.sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
            mcc = 0.
            if abs(mccDenom) > 0.0:
                mcc = mccNum / mccDenom
            return {
                ("precision-%.2f" % thresh): precision,
                ("recall-%.2f" % thresh): recall,
                ("f1-%.2f" % thresh): f1,
                ("mcc-%.2f" % thresh): mcc
            }

        span_dict = { ("span-%s" % k): v for conf in self._span_confs for k, v in stats(conf).items() }
        invalid_dict = { ("invalid-%s" % k): v for conf in self._invalid_confs for k, v in stats(conf).items() }

        if reset:
            self.reset()

        return {**span_dict, **invalid_dict}
