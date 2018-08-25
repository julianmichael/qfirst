from typing import List

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.qfirst_beam_metric.qfirst_beam_metric import QfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

@QfirstBeamMetric.register("filter_tuning")
class FilterTuningMetric(QfirstBeamMetric):
    def __init__(self,
                 target: str,
                 question_thresholds: List[float],
                 span_thresholds: List[float],
                 invalid_thresholds: List[float]):
        self.target = target
        self.filter_metrics = [
            (BeamFilter(q_thresh, s_thresh, i_thresh), EndToEndMetric())
            for q_thresh in question_thresholds
            for s_thresh in span_thresholds
            for i_thresh in invalid_thresholds
        ]
        for _, m in self.filter_metrics:
            m.reset()

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
        best_filter, best_metrics = max(all_metric_dicts, key = lambda d: d[1][self.target])
        filter_params = {
            "question_threshold": best_filter.question_threshold,
            "span_threshold": best_filter.span_threshold,
            "invalid_threshold": best_filter.invalid_threshold
        }
        return {**filter_params, **best_metrics}

    @classmethod
    def from_params(cls, params):
        target = params.pop("target")
        question_thresholds = params.pop("question_thresholds", [0.01, 0.02, 0.03])
        span_thresholds = params.pop("span_thresholds", [0.30, 0.40, 0.50])
        invalid_thresholds = params.pop("invalid_thresholds", [0.10, 0.20, 0.30])
        return FilterTuningMetric(target = target,
                                  question_thresholds = question_thresholds,
                                  span_thresholds = span_thresholds,
                                  invalid_thresholds = invalid_thresholds)
