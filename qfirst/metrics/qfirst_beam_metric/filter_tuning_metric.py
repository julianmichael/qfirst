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
                 try_first_answer_only: bool = False,
                 try_invalid_as_threshold: bool = False,
                 use_dense_metric: bool = False,
                 recall_pegs: float = [0.0],
                 save_filepath: str = None):
        self.target = target
        def make_metric():
            if use_dense_metric:
                return DenseEndToEndMetric()
            else:
                return EndToEndMetric()

        first_answer_bools = [True, False] if try_first_answer_only else [False]
        invalid_as_threshold_bools = [True, False] if try_invalid_as_threshold else [False]
        self.filter_metrics = [
            (BeamFilter(q_thresh, s_thresh, i_thresh, invalid_as_threshold, first_answer_only), make_metric())
            for q_thresh in question_thresholds
            for s_thresh in span_thresholds
            for i_thresh in invalid_thresholds
            for first_answer_only in first_answer_bools
            for invalid_as_threshold in invalid_as_threshold_bools
        ]
        for _, m in self.filter_metrics:
            m.reset()

        self.recall_pegs = recall_pegs
        self._save_filepath = save_filepath

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
        def get_full_dict(filtr, metric_dict):
            filter_params = {
                "question-threshold": filtr.question_threshold,
                "span-threshold": filtr.span_threshold,
                "invalid-threshold": filtr.invalid_threshold,
                "first_answer_only": float(filtr.first_answer_only),
                "invalid_as_threshold": float(filtr.invalid_as_threshold)
            }
            return {**filter_params, **metric_dict}
        all_metric_dicts = [get_full_dict(filtr, metric.get_metric(reset)) for filtr, metric in self.filter_metrics]

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
        question_thresholds = params.pop("question_thresholds", [0.01, 0.02, 0.05, 0.1, 0.15])
        span_thresholds = params.pop("span_thresholds", [0.25, 0.30, 0.35, 0.40, 0.45])
        invalid_thresholds = params.pop("invalid_thresholds", [0.02, 0.05, 0.10, 0.20, 0.30])
        try_first_answer_only = params.pop("try_first_answer_only", False)
        try_invalid_as_threshold = params.pop("try_invalid_as_threshold", False)
        use_dense_metric = params.pop("use_dense_metric", False)
        recall_pegs = params.pop("recall_pegs", [0.0])
        save_filepath = params.pop("save_filepath", None)
        return FilterTuningMetric(target = target,
                                  question_thresholds = question_thresholds,
                                  span_thresholds = span_thresholds,
                                  invalid_thresholds = invalid_thresholds,
                                  try_first_answer_only = try_first_answer_only,
                                  try_invalid_as_threshold = try_invalid_as_threshold,
                                  use_dense_metric = use_dense_metric,
                                  recall_pegs = recall_pegs,
                                  save_filepath = save_filepath)
