from allennlp.common import Params
from allennlp.training.metrics.metric import Metric

import json

# TODO: this is kind of a hack for until I do an actual predictor.
class RankerSavingMetric(Metric):
    def __init__(self, file_path: str):
        self._file_path = file_path
        self._verb_jsons = {}

    def reset(self):
        self._verb_jsons = {}
        return

    def __call__(self,
                 prob,
                 metadata):
        sentence_id = metadata["sentenceId"]
        verb_index = metdata["verbIndex"]
        key = "%s:%s" (sentence_id, verb_index)
        if key not in self._verb_jsons:
            self._verb_jsons[key] = {
                "sentenceId": sentence_id,
                "sentenceTokens": metadata["sentence_tokens"],
                "verbIndex": verb_index,
                "verbInflectedForms": metadata["verbInflectedForms"],
                "clauses": []
            }
        self._verb_jsons[key]["clauses"].append({
            "string": metadata["string"],
            "structure": metadata["structure"],
            "tan": metadata["tan"],
            "argSpans": metadata["argSpans"],
            "prob": prob.item()
        })

    def get_metric(self, reset = False):
        if reset:
            with open(self._file_path, 'w') as out:
                for _, verb_json in self._verb_jsons.items():
                    out.write(json.dumps(verb_json))
                    out.write("\n")
        return {}

    @classmethod
    def from_params(cls, params):
        file_path = params.pop("file_path")
        return RankerSavingMetric(file_path = file_path)
