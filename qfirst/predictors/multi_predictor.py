from typing import List
import json

from allennlp.common import Registrable
from allennlp.common.checks import ConfigurationError
from allennlp.common.util import JsonDict, sanitize
from allennlp.data import DatasetReader, Instance
from allennlp.models import Model
from allennlp.models.archival import Archive, load_archive
from allennlp.predictors.predictor import Predictor

from overrides import overrides

class MultiPredictor(Predictor):
    def __init__(self, model: Model, dataset_reader: DatasetReader) -> None:
        super().__init__(model, dataset_reader)

    # Note the public API change
    @overrides
    def predict_json(self, inputs: JsonDict) -> List[JsonDict]:
        instances = self._json_to_instances(inputs)
        return [self.predict_instance(instance) for instance in instances]

    # Note the name change
    def _json_to_instances(self, json_dict: JsonDict) -> List[Instance]:
        raise NotImplementedError

    @overrides
    def _batch_json_to_instances(self, json_dicts: List[JsonDict]) -> List[Instance]:
        instances = []
        for json_dict in json_dicts:
            instances.extend(self._json_to_instances(json_dict))
        return instances
