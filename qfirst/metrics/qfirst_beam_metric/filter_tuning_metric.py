from typing import List

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.qfirst_beam_metric.qfirst_beam_metric import QfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

@QfirstBeamMetric.register("filter_tuning")
class FilterTuningMetric(QfirstBeamMetric):
    def __init__(self,
                 question_thresholds: List[float],
                 span_thresholds: List[float],
                 invalid_thresholds: List[float]):
        self.filter_metrics = [
            (BeamFilter(q_thresh, s_thresh, i_thresh), EndToEndMetric())
            for q_thresh in question_thresholds
            for s_thresh in span_thresholds
            for s_thresh in invalid_thresholds
        ]
        for _, m in self.filter_metrics:
            m.reset()

    def reset(self):
        for _, m in self.filter_metrics:
            m.reset()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam):
        filtered_beam = self.beam_filter(full_beam)
        self.downstream_metric(gold_qa_pairs, filtered_beam)

    def get_metric(self, reset = False):
        return self.downstream_metric.get_metric(self, reset)

    @classmethod
    def from_params(cls, params):
        question_thresholds = params.pop("question_thresholds", [0.01, 0.02, 0.03])
        span_thresholds = params.pop("span_thresholds", [0.30, 0.40, 0.50])
        invalid_thresholds = params.pop("invalid_thresholds", [0.10, 0.20, 0.30])
        return FilterTuningMetric(question_thresholds,
                                  span,thresholds,
                                  invalid_thresholds)
