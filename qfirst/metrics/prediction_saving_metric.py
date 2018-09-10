from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.end_to_end_metric import EndToEndMetric
from qfirst.metrics.dense_end_to_end_metric import DenseEndToEndMetric
from qfirst.metrics.templating_e2e_metric import TemplatingE2EMetric
from qfirst.metrics.qfirst_beam_metric.qfirst_beam_metric import QfirstBeamMetric
from qfirst.metrics.afirst_beam_metric.afirst_beam_metric import AfirstBeamMetric
from qfirst.util.beam_filter import BeamFilter

import json

@AfirstBeamMetric.register("prediction_saving")
@QfirstBeamMetric.register("prediction_saving")
class PredictionSavingMetric(QfirstBeamMetric):
    def __init__(self,
                 file_path: str,
                 span_minimum_prob: float = 0.05):
        self._file_path = file_path
        self._span_minimum_prob = span_minimum_prob
        self._json_lines = []

    def reset(self):
        return

    def __call__(self,
                 gold_qa_pairs,
                 full_beam,
                 metadata):
        self._json_lines.append({
            "sentenceId": metadata.get("sentence_id") or metadata.get("sent_id"),
            "sentenceTokens": metadata["sentence_tokens"],
            "verbIndex": metadata["pred_index"],
            "verbInflectedForms": metadata["verb_inflected_forms"],
            "questions": [{
                "questionSlots": beam_entry["question_slots"],
                "questionProb": beam_entry.get("question_prob") or 1.0,
                "invalidProb": beam_entry.get("invalid_prob") or 0.0,
                "answerSpans": [((s.start(), s.end() + 1), p) for s, p in beam_entry["answer_spans"] if p >= self._span_minimum_prob]
            } for beam_entry in full_beam]
        })

    def get_metric(self, reset = False):
        if reset:
            with open(self._file_path, 'w') as out:
                for line_json in self._json_lines:
                    out.write(json.dumps(line_json))
                    out.write("\n")
        return {}

    @classmethod
    def from_params(cls, params):
        file_path = params.pop("file_path")
        span_minimum_prob = params.pop("span_minimum_prob", 0.05)
        return PredictionSavingMetric(file_path = file_path,
                                      span_minimum_prob = span_minimum_prob)
