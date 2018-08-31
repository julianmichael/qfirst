from typing import List

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.dense_end_to_end_metric import DenseEndToEndMetric
from qfirst.metrics.qfirst_beam_metric.qfirst_beam_metric import QfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

@QfirstBeamMetric.register("filter_tuning")
class FilterTuningMetric(QfirstBeamMetric):
    def __init__(self,
                 target: str,
                 question_thresholds: List[float],
                 span_thresholds: List[float],
                 invalid_thresholds: List[float],
                 use_dense_metric: bool = False,
                 recall_constraint: float = 2.0):
        self.target = target
        def make_metric():
            if use_dense_metric:
                return DenseEndToEndMetric()
            else:
                return EndToEndMetric()

        self.filter_metrics = [
            (BeamFilter(q_thresh, s_thresh, i_thresh), make_metric())
            for q_thresh in question_thresholds
            for s_thresh in span_thresholds
            for i_thresh in invalid_thresholds
        ]
        for _, m in self.filter_metrics:
            m.reset()

        self.recall_constraint = recall_constraint

    def reset(self):
        for _, m in self.filter_metrics:
            m.reset()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam,
                 metadata = None):
        for beam_filter, metric in self.filter_metrics:
            filtered_beam = beam_filter(full_beam)
            metric(gold_qa_pairs, filtered_beam)

    def get_metric(self, reset = False):
        all_metric_dicts = [(filtr, metric.get_metric(reset)) for filtr, metric in self.filter_metrics]
        recall_constrained_metric_dicts = [(filtr, metric) for filtr, metric in all_metric_dicts
                                           if metric["pred-qs-per-verb"] > self.recall_constraint]
        if len(recall_constrained_metric_dicts) > 0:
            best_filter, best_metrics = max(recall_constrained_metric_dicts, key = lambda d: d[1][self.target])
        else:
            best_filter, best_metrics = max(all_metric_dicts, key = lambda d: d[1][self.target])

        filter_params = {
            "question-threshold": best_filter.question_threshold,
            "span-threshold": best_filter.span_threshold,
            "invalid-threshold": best_filter.invalid_threshold
        }
        return {**filter_params, **best_metrics}

    @classmethod
    def from_params(cls, params):
        target = params.pop("target")
        question_thresholds = params.pop("question_thresholds", [0.01, 0.02, 0.05, 0.1, 0.15])
        span_thresholds = params.pop("span_thresholds", [0.25, 0.30, 0.35, 0.40, 0.45])
        invalid_thresholds = params.pop("invalid_thresholds", [0.02, 0.05, 0.10, 0.20, 0.30])
        use_dense_metric = params.pop("use_dense_metric", False)
        return FilterTuningMetric(target = target,
                                  question_thresholds = question_thresholds,
                                  span_thresholds = span_thresholds,
                                  invalid_thresholds = invalid_thresholds,
                                  use_dense_metric = use_dense_metric)
