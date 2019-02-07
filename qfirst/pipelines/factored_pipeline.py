 # completely ridiculous hack to import stuff properly. somebody save me from myself
import importlib
from allennlp.common.util import import_submodules
importlib.invalidate_caches()
import sys
sys.path.append(".")
import_submodules("qfirst")

from typing import List, Iterator, Optional, Dict

import torch, os, json, tarfile, argparse, uuid, shutil
import sys

from overrides import overrides

from allennlp.common.util import lazy_groups_of
from allennlp.common.checks import check_for_gpu, ConfigurationError
from allennlp.common.util import JsonDict, sanitize
from allennlp.common.util import get_spacy_model
from allennlp.data import Instance
from allennlp.data.dataset import Batch
from allennlp.data.fields import ListField, SpanField, LabelField
from allennlp.nn.util import move_to_device
from allennlp.data import DatasetReader, Instance
from allennlp.models import Model
from allennlp.models.archival import load_archive
from allennlp.predictors.predictor import JsonDict, Predictor

from allennlp.common.file_utils import cached_path
from qfirst.data.util import read_lines, get_verb_fields

from qfirst.data.dataset_readers import QasrlReader
from qfirst.models.multiclass import MulticlassModel
from qfirst.models.span import SpanModel
from qfirst.models.clause_and_span_to_answer_slot import ClauseAndSpanToAnswerSlotModel

clause_minimum_threshold_default = 0.05
span_minimum_threshold_default = 0.05

class FactoredPipeline():
    def __init__(self,
                 span_model: SpanModel,
                 span_model_dataset_reader: QasrlReader,
                 clause_model: MulticlassModel,
                 clause_model_dataset_reader: QasrlReader,
                 answer_slot_model: ClauseAndSpanToAnswerSlotModel,
                 answer_slot_model_dataset_reader: QasrlReader,
                 tan_predictions: Dict[str, JsonDict],
                 animacy_predictions: Dict[str, JsonDict],
                 clause_minimum_threshold: float = span_minimum_threshold_default,
                 span_minimum_threshold: float = clause_minimum_threshold_default) -> None:
        self._span_model = span_model
        self._span_model_dataset_reader = span_model_dataset_reader
        self._clause_model = clause_model
        self._clause_model_dataset_reader = clause_model_dataset_reader
        self._answer_slot_model = answer_slot_model
        self._answer_slot_model_dataset_reader = answer_slot_model_dataset_reader
        self._tan_predictions = tan_predictions
        self._animacy_predictions = animacy_predictions
        self._clause_minimum_threshold = clause_minimum_threshold
        self._span_minimum_threshold = span_minimum_threshold

    def predict(self, inputs: JsonDict) -> JsonDict:
        # TODO: animacy and tan stuff

        clause_verb_instances = list(self._clause_model_dataset_reader.sentence_json_to_instances(inputs, verbs_only = True))
        clause_outputs = self._clause_model.forward_on_instances(clause_verb_instances)
        span_verb_instances = list(self._span_model_dataset_reader.sentence_json_to_instances(inputs, verbs_only = True))
        span_outputs = self._span_model.forward_on_instances(span_verb_instances)
        answer_slot_instances = self._answer_slot_model_dataset_reader.sentence_json_to_instances(inputs, verbs_only = True)

        verb_dicts = []
        for instance, clause_output, span_output in zip(answer_slot_instances, clause_outputs, span_outputs):
            beam = []
            scored_clauses = [
                (self._clause_model.vocab.get_token_from_index(c, namespace = "abst-clause-labels"), p)
                for c, p in enumerate(clause_output["probs"].tolist())
                if p >= self._clause_minimum_threshold]
            scored_spans = [
                (s, p)
                for s, p in span_output["spans"]
                if p >= self._span_minimum_threshold]
            clause_span_pairs = [
                (clause, clause_prob, span, span_prob)
                for clause, clause_prob in scored_clauses
                for span, span_prob in scored_spans
            ]
            clause_fields = [
                LabelField(clause, label_namespace = "abst-clause-labels")
                for clause, _, _, _ in clause_span_pairs
            ]
            span_fields = [
                SpanField(span.start(), span.end(), instance["text"])
                for _, _, span, _ in clause_span_pairs
            ]
            if len(clause_fields) > 0:
                instance.add_field("qarg_labeled_clauses", ListField(clause_fields))
                instance.add_field("qarg_labeled_spans", ListField(span_fields))
                answer_slot_output = self._answer_slot_model.forward_on_instance(instance)

                for i in range(len(clause_span_pairs)):
                    clause, clause_prob, span, span_prob = clause_span_pairs[i]
                    answer_slots_with_probs = {
                        self._answer_slot_model.vocab.get_token_from_index(slot_index, namespace = "qarg-labels"): answer_slot_output["probs"][i, slot_index].item()
                        for slot_index in range(self._answer_slot_model.vocab.get_vocab_size("qarg-labels"))
                    }
                    beam.append({
                        "clause": clause,
                        "clauseProb": clause_prob,
                        "span": [span.start(), span.end() + 1],
                        "spanProb": span_prob,
                        "answerSlots": answer_slots_with_probs
                    })
            verb_dicts.append({
                "verbIndex": instance["metadata"]["verb_index"],
                "verbInflectedForms": instance["metadata"]["verb_inflected_forms"],
                "beam": beam
            })
        return {
            "sentenceId": inputs["sentenceId"],
            "sentenceTokens": inputs["sentenceTokens"],
            "verbs": verb_dicts
        }

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description = "Run the answer-first pipeline")
    parser.add_argument('--clause', type=str, help = "Path to clause detector model archive (.tar.gz).")
    parser.add_argument('--span', type=str, help = "Path to span detector model archive (.tar.gz).")
    parser.add_argument('--answer_slot', type=str, help = "Path to answer slot model archive (.tar.gz).")
    parser.add_argument('--tan', type=str, help = "Path to TAN predictions (.jsonl).", default = None) # TODO
    parser.add_argument('--animacy', type=str, help = "Path to animacy predictions (.jsonl).", default = None) # TODO
    parser.add_argument('--cuda_device', type=int, default=-1)
    parser.add_argument('--input_file', type=str)
    parser.add_argument('--output_file', type=str, default = None)
    parser.add_argument('--clause_min_prob', type=float, default = clause_minimum_threshold_default)
    parser.add_argument('--span_min_prob', type=float, default = span_minimum_threshold_default)
    args = parser.parse_args()

    check_for_gpu(args.cuda_device)
    clause_model_archive = load_archive(args.clause, cuda_device = args.cuda_device)
    span_model_archive = load_archive(args.span, cuda_device = args.cuda_device)
    answer_slot_model_archive = load_archive(args.answer_slot, cuda_device = args.cuda_device)
    pipeline = FactoredPipeline(
        clause_model = clause_model_archive.model,
        clause_model_dataset_reader = DatasetReader.from_params(clause_model_archive.config["dataset_reader"].duplicate()),
        span_model = span_model_archive.model,
        span_model_dataset_reader = DatasetReader.from_params(span_model_archive.config["dataset_reader"].duplicate()),
        answer_slot_model = answer_slot_model_archive.model,
        answer_slot_model_dataset_reader = DatasetReader.from_params(answer_slot_model_archive.config["dataset_reader"].duplicate()),
        tan_predictions = args.tan, # TODO
        animacy_predictions = args.animacy, # TODO
        clause_minimum_threshold = args.clause_min_prob,
        span_minimum_threshold = args.span_min_prob)
    if args.output_file is None:
        for line in read_lines(cached_path(args.input_file)):
            input_json = json.loads(line)
            output_json = pipeline.predict(input_json)
            print(json.dumps(output_json))
    else:
        with open(args.output_file, 'w', encoding = 'utf8') as out:
            for line in read_lines(cached_path(args.input_file)):
                input_json = json.loads(line)
                output_json = pipeline.predict(input_json)
                print(json.dumps(output_json), file = out)
