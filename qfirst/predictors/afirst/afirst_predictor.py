from typing import List

from overrides import overrides

from allennlp.common.util import JsonDict, sanitize
from allennlp.common.util import get_spacy_model
from allennlp.data.dataset import Batch
from allennlp.data.fields import ListField, SpanField
from allennlp.nn.util import move_to_device
from allennlp.data import DatasetReader, Instance
from allennlp.models import Model
from allennlp.predictors.predictor import Predictor

from qfirst.data.dataset_readers import QasrlReader

from qfirst.models.afirst.afirst_parser import AfirstParser

@Predictor.register("afirst")
class AfirstPredictor(Predictor):
    def __init__(self,
                 model: AfirstParser,
                 dataset_reader: QasrlReader,
                 span_minimum_threshold: float = 0.05,
                 question_minimum_threshold: float = 0.05,
                 question_beam_size: int = 10) -> None:
        super(AfirstPredictor, self).__init__(model, dataset_reader)
        self._span_minimum_threshold = span_minimum_threshold
        self._question_minimum_threshold = question_minimum_threshold
        self._question_beam_size = question_beam_size

    @overrides
    def predict_json(self, inputs: JsonDict) -> List[JsonDict]:
        verb_instances = list(self._dataset_reader.sentence_json_to_instances(inputs, verbs_only = True))
        span_outputs = self._model.get_span_detector().forward_on_instances(verb_instances)
        verb_dicts = []
        for (verb_instance, span_output) in zip(verb_instances, span_outputs):
            scored_spans = [(s, p) for s, p in span_output["spans"] if p >= self._span_minimum_threshold]
            span_fields = [SpanField(span.start(), span.end(), verb_instance["text"]) for span, _ in scored_spans]
            verb_instance.add_field("answer_spans", ListField(span_fields), self._model.get_question_generator().vocab)
            qgen_input_tensors = move_to_device(
                Batch([verb_instance]).as_tensor_dict(),
                self._model.get_question_generator()._get_prediction_device())
            question_beams = self._model.get_question_generator().beam_decode(
                text = qgen_input_tensors["text"],
                predicate_indicator = qgen_input_tensors["predicate_indicator"],
                answer_spans = qgen_input_tensors["answer_spans"],
                max_beam_size = self._question_beam_size,
                min_beam_probability = self._question_minimum_threshold)
            beam = []
            for (span, span_prob), (_, slot_values, question_probs) in zip(scored_spans, question_beams):
                for i in range(len(question_probs)):
                    question_slots = [
                        slot_values[slot_name][i]
                        for slot_name in self._model.get_question_generator().get_slot_names()]
                    beam.append({
                        "question_slots": question_slots,
                        "question_prob": question_probs[i],
                        "span": [span.start(), span.end() + 1],
                        "span_prob": span_prob
                    })
            verb_dicts.append({
                "verbIndex": verb_instance["metadata"]["verb_index"],
                "verbInflectedForms": verb_instance["metadata"]["verb_inflected_forms"],
                "beam": beam
            })
        return {
            "sentenceId": inputs["sentenceId"],
            "sentenceTokens": inputs["sentenceTokens"],
            "verbs": verb_dicts
        }

    @overrides
    def predict_batch_json(self, inputs: List[JsonDict]) -> List[JsonDict]:
        return map(inputs, self.predict_json)
