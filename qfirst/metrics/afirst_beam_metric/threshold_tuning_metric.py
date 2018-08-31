from typing import List

from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.afirst_beam_metric.afirst_beam_metric import AfirstBeamMetric
from qfirst.metrics.afirst_beam_metric.threshold_metric import ThresholdMetric

@AfirstBeamMetric.register("threshold_tuning")
class ThresholdTuningMetric(AfirstBeamMetric):
    def __init__(self,
                 target: str,
                 span_thresholds: List[float],
                 use_dense_metric: bool = False,
                 recall_constraint = 0.0):
        self.target = target
        self.metrics = [ThresholdMetric(threshold, use_dense_metric) for threshold in span_thresholds]
        for m in self.metrics:
            m.reset()
        self.recall_constraint = recall_constraint

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
        recall_constrained_metric_dicts = [metric for metric in all_metric_dicts
                                           if metric["pred-qs-per-verb"] > self.recall_constraint]
        if len(recall_constrained_metric_dicts) > 0:
            best_metrics = max(recall_constrained_metric_dicts, key = lambda d: d[self.target])
        else:
            best_metrics = max(all_metric_dicts, key = lambda d: d[self.target])
        return best_metrics


    @classmethod
    def from_params(cls, params):
        target = params.pop("target")
        span_thresholds = params.pop("span_thresholds")
        use_dense_metric = params.pop("use_dense_metric", False)
        recall_constraint = params.pop("recall_constraint", 0.0)
        return ThresholdTuningMetric(target, span_thresholds, use_dense_metric, recall_constraint)
