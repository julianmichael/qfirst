from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.dense_end_to_end_metric import DenseEndToEndMetric
from qfirst.metrics.qfirst_beam_metric.qfirst_beam_metric import QfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

@QfirstBeamMetric.register("beam_filtering")
class BeamFilteringMetric(QfirstBeamMetric):
    def __init__(self,
                 beam_filter,
                 use_dense_metric: bool = False):
        self.beam_filter = beam_filter
        if use_dense_metric:
            self.downstream_metric = DenseEndToEndMetric()
        else:
            self.downstream_metric = EndToEndMetric()
        self.reset()

    def reset(self):
        self.downstream_metric.reset()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam,
                 metadata):
        filtered_beam = self.beam_filter(full_beam)

        # print("\n" + " ".join(metadata["sentence_tokens"]))
        # print("\nGOLD:")
        # for gold_entry in gold_qa_pairs:
        #     print("\n" + " ".join(gold_entry["question"]))
        #     span_strings = [" ".join(metadata["sentence_tokens"][span.start() : (span.end() + 1)]) for span in gold_entry["answer_spans"]]
        #     print("   " + "\n   ".join(span_strings))
        # print("\nPREDICTED:")
        # for entry in full_beam:
        #     if entry["question_prob"] > 0.02:
        #         print("\n" + ("%4.2f (%4.2f) " % (entry["question_prob"], entry["invalidity_prob"])) + " ".join(entry["question"]))
        #         sorted_spans = sorted(entry["spans"], key = lambda t: t[1], reverse = True)
        #         span_strings = [" ".join(metadata["sentence_tokens"][span.start() : (span.end() + 1)]) + (" (%.2f)" % prob) for span, prob in sorted_spans if prob > 0.01]
        #         print("   " + ("\n   ").join(span_strings))

        self.downstream_metric(gold_qa_pairs, filtered_beam)

    def get_metric(self, reset = False):
        return self.downstream_metric.get_metric(reset)

    @classmethod
    def from_params(cls, params):
        beam_filter = BeamFilter.from_params(params.pop("filter"))
        use_dense_metric = params.pop("use_dense_metric", False)
        return BeamFilteringMetric(beam_filter, use_dense_metric)
