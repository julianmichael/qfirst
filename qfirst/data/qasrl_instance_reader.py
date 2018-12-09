from typing import List, Dict

from allennlp.common import Registrable
from allennlp.data.token_indexers import TokenIndexer
from allennlp.data.fields import Field, IndexField, TextField, SequenceLabelField, LabelField, ListField, MetadataField, SpanField
from allennlp.data.dataset_readers.dataset_reader import DatasetReader
from allennlp.data.instance import Instance
from allennlp.data.token_indexers import SingleIdTokenIndexer, TokenIndexer
from allennlp.data.tokenizers import Token, Tokenizer, WordTokenizer

from nrl.common.span import Span
from nrl.data.util import cleanse_sentence_text

from qfirst.data.util import *

from overrides import overrides

class QasrlInstanceReader(Registrable):
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # Iterable[Dict[str, ?Field]]
        raise NotImplementedError

@QasrlInstanceReader.register("question")
class QasrlQuestionReader(QasrlInstanceReader):
    def __init__(self,
                 include_slots: bool = True,
                 include_abstract_slots: bool = False,
                 clause_info_files: List[str] = []):
        self._include_slots = include_slots
        self._include_abstract_slots = include_abstract_slots
        self._clause_info = None
        if len(clause_info_files) > 0:
            self._clause_info = {}
            for file_path in clause_info_files:
                read_clause_info(self._clause_info, file_path)
        self._tokenizer = WordTokenizer()
    @overrides
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # Iterable[Dict[str, ?Field]]
        verb_fields = get_verb_fields(token_indexers, sentence_tokens, verb_index)
        for question_label in question_labels:
            question_text_field = TextField(self._tokenizer.tokenize(question_label["questionString"]), token_indexers)
            question_slots_dict = get_question_slot_fields(question_label["questionSlots"]) if self._include_slots else {}
            abstract_slots_dict = get_abstract_question_slot_fields(question_label) if self._include_abstract_slots else {}
            if self._clause_info is not None:
                try:
                    clause_dict = self._clause_info[sentence_id][pred_index][question_label["questionString"]]
                    clause_slots_dict = { ("clause-%s" % k) : get_clause_slot_field(k, v) for k, v in clause_dict["slots"].items() }
                except KeyError:
                    logger.info("Omitting instance without clause data: %s / %s / %s" % (sentence_id, pred_index, question_label["questionString"]))
                    continue
            else:
                clause_slots_dict = {}

            question_fields = {
                **question_slots_dict, **abstract_slots_dict, **clause_slots_dict,
                'question_text': question_text_field,
            }

            answer_fields = get_answer_fields(question_label, verb_fields["text"])

            metadata = { "question_label": question_label }

            yield {
                "metadata": metadata,
                **verb_fields, **question_fields, **answer_fields
            }

@QasrlInstanceReader.register("verb_only")
class QasrlVerbReader(QasrlInstanceReader):
    @overrides
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # Iterable[Instance]
        yield Instance(get_verb_fields(token_indexers, sentence_tokens, verb_index))

# @QasrlInstanceReader.register("question_only_clause_filling")
# class QasrlClauseFillingReader(QasrlInstanceReader):
#     def __init__(self,
#                  clause_info_files: List[str] = []):
#         self._include_slots = include_slots
#         self._include_abstract_slots = include_abstract_slots
#         self._clause_info = {}
#         if len(clause_info_files) == 0:
#             raise ConfigurationError("Must provide clause_info_files to QasrlClauseFillingReader")
#         for file_path in clause_info_files:
#             read_clause_info(self._clause_info, file_path)
