from typing import List

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.afirst_beam_metric.afirst_beam_metric import AfirstBeamMetric
from qfirst.metrics.afirst_beam_metric.threshold_metric import ThresholdMetric

@AfirstBeamMetric.register("threshold_tuning")
class ThresholdTuningMetric(AfirstBeamMetric):
    def __init__(self,
                 target: str,
                 span_thresholds: List[float]):
        self.target = target
        self.metrics = [ThresholdMetric(threshold) for threshold in span_thresholds]
        for m in self.metrics:
            m.reset()

    def reset(self):
        for m in self.metrics:
            m.reset()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam):
        for metric in self.metrics:
            metric(gold_qa_pairs, full_beam)

    def get_metric(self, reset = False):
        all_metric_dicts = [{**{"threshold": metric.span_threshold}, **metric.get_metric(reset)} for metric in self.metrics]
        best_metrics = max(all_metric_dicts, key = lambda d: d[self.target])
        return best_metrics

    @classmethod
    def from_params(cls, params):
        target = params.pop("target")
        span_thresholds = params.pop("span_thresholds")
        return ThresholdTuningMetric(target, span_thresholds)
