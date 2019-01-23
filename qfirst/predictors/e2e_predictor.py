from typing import List

from overrides import overrides

from allennlp.common.util import JsonDict
from allennlp.common.util import get_spacy_model
from allennlp.data import DatasetReader, Instance
from allennlp.models import Model
from allennlp.predictors.predictor import Predictor

from qfirst.data.dataset_readers import QasrlReader
from qfirst.predictors.multi_predictor import MultiPredictor

@Predictor.register("qasrl-e2e")
class E2EPredictor(MultiPredictor):
    def __init__(self, model: Model, dataset_reader: QasrlReader) -> None:
        super().__init__(model, dataset_reader)

    @overrides
    def _json_to_instances(self, json_dict: JsonDict) -> List[Instance]:
        return [i for i in self._dataset_reader.sentence_json_to_instances(json_dict)]
