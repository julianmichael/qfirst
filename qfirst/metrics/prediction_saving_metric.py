from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

from qfirst.metrics.beam_metric import BeamMetric

import json

# TODO: this is kind of a hack for until I do an actual predictor.
@BeamMetric.register("prediction_saving")
class PredictionSavingMetric(BeamMetric):
    def __init__(self, file_path: str):
        self._file_path = file_path
        self._sentence_jsons = {}

    def reset(self):
        self._sentence_jsons = {}
        return

    def __call__(self,
                 gold_qa_pairs,
                 full_beam,
                 metadata):
        sentence_id = metadata.get("sentence_id") or metadata.get("sent_id"),
        if sentence_id not in self._sentence_jsons:
            self._sentence_jsons[sentence_id] = {
                "sentenceId": sentence_id,
                "sentenceTokens": metadata["sentence_tokens"],
                "verbs": []
            }
        self._sentence_jsons[sentence_id]["verbs"].append({
            "verbIndex": metadata["pred_index"],
            "verbInflectedForms": metadata["verb_inflected_forms"],
            "questions": [{
                "questionSlots": beam_entry["question_slots"],
                "questionProb": beam_entry.get("question_prob") or 1.0,
                "invalidProb": beam_entry.get("invalid_prob") or 0.0,
                "answerSpans": [((s.start(), s.end() + 1), p)
                                for s, p in beam_entry["answer_spans"]]
            } for beam_entry in full_beam]
        })

    def get_metric(self, reset = False):
        if reset:
            with open(self._file_path, 'w') as out:
                for _, sentence_json in self._sentence_jsons.items():
                    out.write(json.dumps(sentence_json))
                    out.write("\n")
        return {}

    @classmethod
    def from_params(cls, params):
        file_path = params.pop("file_path")
        return PredictionSavingMetric(file_path = file_path)
