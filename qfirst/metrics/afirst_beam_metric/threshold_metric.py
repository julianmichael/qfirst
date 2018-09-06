from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.dense_end_to_end_metric import DenseEndToEndMetric
from qfirst.metrics.afirst_beam_metric.afirst_beam_metric import AfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

@AfirstBeamMetric.register("threshold")
class ThresholdMetric(AfirstBeamMetric):
    def __init__(self,
                 span_threshold,
                 use_dense_metric: bool = False):
        self.span_threshold = span_threshold
        if use_dense_metric:
            self.downstream_metric = DenseEndToEndMetric()
        else:
            self.downstream_metric = EndToEndMetric()
        self.reset()

    def reset(self):
        self.downstream_metric.reset()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam):
        all_spans = []
        def has_overlap(candidate_span, other_spans):
            for span in all_spans:
                if candidate_span.overlaps(span):
                    return True
            for span in other_spans:
                if candidate_span.overlaps(span):
                    return True
            return False

        filtered_beam = []
        for qa in full_beam:
            qa_spans = []
            for s, p in qa["spans"]:
                if p >= self.span_threshold and not has_overlap(s, qa_spans):
                    qa_spans.append(s)
            if len(qa_spans) > 0:
                filtered_beam.append({"question": qa["question"], "spans": qa_spans})
        self.downstream_metric(gold_qa_pairs, filtered_beam)

    def get_metric(self, reset = False):
        return self.downstream_metric.get_metric(reset)

    @classmethod
    def from_params(cls, params):
        span_threshold = params.pop("span_threshold")
        use_dense_metric = params.pop("use_dense_metric", False)
        return ThresholdMetric(span_threshold = span_threshold, use_dense_metric = use_dense_metric)
