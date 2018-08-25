import codecs
import os
import logging
from typing import Dict, List, Optional, Tuple
from collections import Counter
import json
import gzip

from overrides import overrides

from allennlp.common import Params
from allennlp.common.file_utils import cached_path
from allennlp.data.dataset_readers.dataset_reader import DatasetReader
from allennlp.data.instance import Instance
from allennlp.data.token_indexers import SingleIdTokenIndexer, TokenIndexer
from allennlp.data.fields import Field, IndexField, TextField, SequenceLabelField, LabelField, ListField, MetadataField, SpanField
from allennlp.data.tokenizers import Token
from allennlp.data.dataset_readers.dataset_utils.span_utils import enumerate_spans

from nrl.common.span import Span
from nrl.data.util import cleanse_sentence_text

from qfirst.data.util import get_slot_label_namespace

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

@DatasetReader.register("qfirst_qasrl")
class QasrlReader(DatasetReader):
    def __init__(self,
                 instance_type: str,
                 token_indexers: Dict[str, TokenIndexer] = None,
                 # has_provenance = False,
                 min_answers = 0,
                 min_valid_answers = 0,
                 question_source = None):
        super().__init__(False)
        self._token_indexers = token_indexers or {"tokens": SingleIdTokenIndexer(lowercase_tokens=True)}
        # self._has_provenance = has_provenance
        # self._invalid_thresh = 0
        # self._max_spans = None
        self._min_answers = min_answers
        self._min_valid_answers = min_valid_answers
        logger.info("Reading QA-SRL questions with at least %s answers and %s valid answers." % (self._min_answers, self._min_valid_answers))
        self._slot_names = ["wh", "aux", "subj", "verb", "obj", "prep", "obj2"]

        self._instance_type = instance_type

        self._num_instances = 0
        self._num_verbs = 0

        self._question_source = question_source

        # self._qa_pairs = 0
        # self._no_ann = 0
        # self._not_enough_answers = 0
        # self._not_enough_valid_answers = 0

    @overrides
    def _read(self, file_list: str):
        instances = []
        for file_path in file_list.split(","):
            if file_path.strip() == "":
                continue

            logger.info("Reading QASRL instances from dataset file at: %s", file_path)
            data = []
            if file_path.endswith('.gz'):
                with gzip.open(cached_path(file_path), 'r') as f:
                    for line in f:
                        data.append(json.loads(line))
            elif file_path.endswith(".jsonl"):
                with codecs.open(cached_path(file_path), 'r', encoding='utf8') as f:
                    for line in f:
                        data.append(json.loads(line))

            for item in data:
                sentence_id = item["sentenceId"]
                sentence_tokens = item["sentenceTokens"]

                for _, verb_entry in item["verbEntries"].items():
                    verb_index = verb_entry["verbIndex"]

                    def is_valid(question_label):
                        answers = question_label["answerJudgments"]
                        valid_answers = [a for a in answers if a["isValid"]]
                        is_source_valid = self._question_source is None or any([l.startswith(self._question_source) for l in question_label["questionSources"]])
                        return (len(answers) >= self._min_answers) and (len(valid_answers) >= self._min_valid_answers) and is_source_valid
                    question_labels = [l for q, l in verb_entry["questionLabels"].items() if is_valid(l)]

                    self._num_verbs += 1
                    if self._instance_type == "verb":
                        if len(question_labels) != 0:
                            yield self._make_gold_verb_instance(sentence_id, sentence_tokens, verb_index, question_labels)
                    elif self._instance_type == "question":
                        for l in question_labels:
                            yield self._make_gold_question_instance(sentence_id, sentence_tokens, verb_index, l)

                    # each item in question_labels includes info for one question and all of its answer judgments
                    # question_labels = []
                    # for _, question_label in .items():
                    #     answers = len(question_label["answerJudgments"])
                    #     valid_answers = len([ans for ans in question_label["answerJudgments"] if ans["isValid"]])

                    #     if self._question_source is not None:
                    #         if not any([source.startswith(prefix) for source in question_label["questionSources"] for prefix in self._question_source]):
                    #             continue

                    #     if answers < self._min_answers:
                    #         self._not_enough_answers += 1
                    #         continue
                    #     if valid_answers < self._min_valid_answers:
                    #         self._not_enough_valid_answers += 1
                    #         continue

                    #     slots = [slots.append(question_label["questionSlots"][l]) for l in self._slot_labels]
                    #     provenance = list(question_label["questionSources"])[0]
                    #     spans = [Span(s[0], s[1]-1) for s in ans["spans"] if ans["isValid"] for ans in question_label["answerJudgments"]]
                    #     num_invalid = len(questionLabel["answerJudgments"]) - len(spans)

                    #     self._qa_pairs += 1
                    #     annotations.append(AnnotatedSpan(slots = slots, all_spans = spans, provenance=provenance))

                    # if annotations:
                    #     self._instances += 1
                    #     yield self._make_instance_from_text(sentence_tokens, verb_index, annotations = annotations, sentence_id = sentence_id)
                    # else:
                    #     self._no_ann += 1

        logger.info("Produced %d instances"%self._num_instances)
        logger.info("\t%d Verbs"%self._num_verbs)
        # logger.info("\t%d QA pairs"%self._qa_pairs)
        # logger.info("\t%d no annotation"%self._no_ann)
        # logger.info("\t%d not enough answers"%self._not_enough_answers)
        # logger.info("\t%d not enough valid answers"%self._not_enough_valid_answers)

    def _make_gold_verb_instance(self, #type: ignore
                            sentence_id: str,
                            sent_tokens: List[str],
                            pred_index: int,
                            question_labels): # List[Json]
        """
        Returns
        -------
        An ``Instance`` containing the following fields:
            text : ``TextField`` (length n)
                The tokens in the sentence.
            predicate_index : ``IndexField`` over text
            predicate_indicator : ``SequenceLabelField`` over text
                Sequence of 0 or 1 with a single 1 marking the predicate.
            slot_X : ``ListField[LabelField]``
                Slot X value for questions involving the predicate.
                There is one of these for each slot X, each having its own label space.
            answers : ``ListField[ListField[SpanField]]`` (spans over text)
                Answer spans for each question, in order.
                Collapses together spans provided by different annotators into a single list.
                Retains repeats.
            num_invalids: List[int]
                Number of annotator judgments that a question was invalid.
            metadata : ``Metadatafield`` containing:
                text : sentence tokens joined by spaces
                pred_index : index of the predicate in the token list
                sentence_id : the sentence ID string.
                question_sources : the IDs of the question sources, in order.
                question_labels : the question labels.
        """
        instance_dict = {}

        if isinstance(sent_tokens, str):
            sent_tokens = sent_tokens.split()
        sent_tokens = cleanse_sentence_text(sent_tokens)
        text_field = TextField([Token(t) for t in sent_tokens], self._token_indexers)
        predicate_index_field = IndexField(pred_index, text_field)
        predicate_indicator_field = SequenceLabelField([1 if i == pred_index else 0 for i in range(len(sent_tokens))], text_field)

        def get_slot_values_field(slot_name):
            def get_slot_label_field(value):
                return LabelField(label = value, label_namespace = get_slot_label_namespace(slot_name))
            return ListField([get_slot_label_field(l["questionSlots"][slot_name]) for l in question_labels])
        slots_dict = { slot_name : get_slot_values_field(slot_name) for slot_name in self._slot_names }

        def get_answers_field_for_question(label):
            def get_spans(spansJson):
                return [SpanField(s[0], s[1]-1, text_field) for s in spansJson]
            return ListField([s for ans in label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"]) ])
        answer_fields = [get_answers_field_for_question(l) for l in question_labels]
        num_answers = [len(l["answerJudgments"]) for l in question_labels]
        num_answers_field = ListField([LabelField(label = n, skip_indexing = True) for n in num_answers])
        num_invalids = [len(l["answerJudgments"]) - len([aj for aj in l["answerJudgments"] if aj["isValid"]]) for l in question_labels]
        num_invalids_field  = ListField([LabelField(label = n, skip_indexing = True) for n in num_invalids])

        metadata = {'pred_index' : pred_index, 'sent_text': " ".join(sent_tokens)}
        metadata['sentence_id'] = sentence_id
        metadata['sentence_tokens'] = sent_tokens
        metadata['question_sources'] = [l["questionSources"] for l in question_labels]
        metadata['question_labels'] = question_labels

        instance_dict = {
            'text': text_field,
            'predicate_index': predicate_index_field,
            'predicate_indicator': predicate_indicator_field,
            'answers': ListField(answer_fields),
            'num_answers': num_answers_field,
            'num_invalids': num_invalids_field,
            'metadata': MetadataField(metadata),
        }

        return Instance({**instance_dict, **slots_dict})

    def _make_gold_question_instance(self, #type: ignore
                            sentence_id: str,
                            sent_tokens: List[str],
                            pred_index: int,
                            question_label): # Json
        """
        Returns
        -------
        An ``Instance`` containing the following fields:
            text : ``TextField`` (length n)
                The tokens in the sentence.
            predicate_indicator : ``SequenceLabelField`` over text
                Sequence of 0 or 1 with a single 1 marking the predicate.
            slot_X : ``LabelField``
                Slot X value for questions involving the predicate.
                There is one of these for each slot X, each having its own label space.
            answers : ``ListField[SpanField]`` (spans over text)
                Answer spans for the question.
                Collapses together spans provided by different annotators into a single list.
                Retains repeats.
            num_invalids: int
                Number of annotator judgments that a question was invalid.
            metadata : ``Metadatafield`` containing:
                text : sentence tokens joined by spaces
                pred_index : index of the predicate in the token list
                sentence_id : the sentence ID string.
                question_sources : the IDs of the question sources, in order.
                question_label : the question label.
        """
        instance_dict = {}

        if isinstance(sent_tokens, str):
            sent_tokens = sent_tokens.split()
        sent_tokens = cleanse_sentence_text(sent_tokens)
        text_field = TextField([Token(t) for t in sent_tokens], self._token_indexers)
        predicate_index_field = IndexField(pred_index, text_field)
        predicate_indicator_field = SequenceLabelField([1 if i == pred_index else 0 for i in range(len(sent_tokens))], text_field)

        def get_slot_value_field(slot_name):
            namespace = get_slot_label_namespace(slot_name)
            value = question_label["questionSlots"][slot_name]
            return LabelField(label = value, label_namespace = namespace)
        slots_dict = { slot_name : get_slot_value_field(slot_name) for slot_name in self._slot_names }

        def get_answers_field_for_question(label):
            def get_spans(spansJson):
                return [SpanField(s[0], s[1]-1, text_field) for s in spansJson]
            span_list = [s for ans in label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"]) ]
            if len(span_list) == 0:
                return ListField([SpanField(-1, -1, text_field)])
            else:
                return ListField(span_list)
        answers_field = get_answers_field_for_question(question_label)
        num_answers = len(question_label["answerJudgments"])
        num_invalids = num_answers - len([aj for aj in question_label["answerJudgments"] if aj["isValid"]])
        num_answers_field = LabelField(label = num_answers, skip_indexing = True)
        num_invalids_field = LabelField(label = num_invalids, skip_indexing = True)

        metadata = {'pred_index' : pred_index, 'sent_text': " ".join(sent_tokens)}
        metadata['sentence_id'] = sentence_id
        metadata['question_sources'] = question_label["questionSources"]
        metadata['question_label'] = question_label

        instance_dict = {
            'text': text_field,
            'predicate_index': predicate_index_field,
            'predicate_indicator': predicate_indicator_field,
            'answer_spans': answers_field,
            'num_answers': num_answers_field,
            'num_invalids': num_invalids_field,
            'metadata': MetadataField(metadata),
        }

        return Instance({**instance_dict, **slots_dict})

    @classmethod
    def from_params(cls, params: Params) -> 'QasrlReader':
        token_indexers = TokenIndexer.dict_from_params(params.pop('token_indexers', Params({"tokens": {"type": "single_id", "lowercase_tokens": True}})))
        instance_type = params.pop("instance_type")

        # has_provenance = params.pop("has_provenance", False)

        min_answers = params.pop("min_answers", 0)
        min_valid_answers = params.pop("min_valid_answers", 0)

        question_source = params.pop("question_source", None)

        params.assert_empty(cls.__name__)
        return QasrlReader(token_indexers = token_indexers, instance_type = instance_type,
                           min_answers = min_answers, min_valid_answers = min_valid_answers,
                           question_source = question_source)
