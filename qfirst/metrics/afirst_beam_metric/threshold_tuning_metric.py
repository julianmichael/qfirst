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
                 recall_pegs = [0.0],
                 save_filepath = None):
        self.target = target
        self.metrics = [ThresholdMetric(threshold, use_dense_metric) for threshold in span_thresholds]
        for m in self.metrics:
            m.reset()
        self.recall_pegs = recall_pegs
        self._save_filepath = save_filepath

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

        if len(self.recall_pegs) == 1:
            recall_constrained_metric_dicts = [metric for metric in all_metric_dicts
                                               if metric["pred-qs-per-verb"] > self.recall_pegs[0]]
            if len(recall_constrained_metric_dicts) > 0:
                best_metrics = max(recall_constrained_metric_dicts, key = lambda d: d[self.target])
            else:
                best_metrics = max(all_metric_dicts, key = lambda d: d[self.target])
            return best_metrics
        else:
            def get_best_metric_opt(recall_floor):
                valid_metrics = [metric for metric in all_metric_dicts if metric["pred-qs-per-verb"] > recall_floor]
                if len(valid_metrics) > 0:
                    return max(valid_metrics, key = lambda d: d[self.target])
                else:
                    return None

            recall_constrained_metric_dicts = {
                ("%.1f" % recall_floor): get_best_metric_opt(recall_floor)
                for recall_floor in self.recall_pegs
            }
            if reset:
                all_metrics_dict = {
                    ("recall-%s-%s" % (recall_floor, metric_key)): metric_value
                    for recall_floor, metric_dict in recall_constrained_metric_dicts.items()
                    if metric_dict is not None
                    for metric_key, metric_value in metric_dict.items()
                }
                if self._save_filepath is not None:
                    with open(self._save_filepath, 'a') as out:
                        out.write("==============================\nValidation complete. Metrics for recall pegs:\n\n")
                        for recall_floor, metric_dict in recall_constrained_metric_dicts.items():
                            if metric_dict is not None:
                                out.write("---------- Recall = %s ----------\n" % recall_floor)
                                for metric_key, metric_value in metric_dict.items():
                                    out.write("%s: %.4f\n" % (metric_key, metric_value))
                return all_metrics_dict
            else:
                summary_metrics_dict = {
                    ("%s-%s" % (recall_floor, self.target)): metric_dict[self.target]
                    for recall_floor, metric_dict in recall_constrained_metric_dicts.items()
                    if metric_dict is not None and self.target in metric_dict
                }
                return summary_metrics_dict

    @classmethod
    def from_params(cls, params):
        target = params.pop("target")
        span_thresholds = params.pop("span_thresholds")
        use_dense_metric = params.pop("use_dense_metric", False)
        recall_pegs = params.pop("recall_pegs", [0.0])
        save_filepath = params.pop("save_filepath", None)
        return ThresholdTuningMetric(target, span_thresholds, use_dense_metric, recall_pegs, save_filepath)
