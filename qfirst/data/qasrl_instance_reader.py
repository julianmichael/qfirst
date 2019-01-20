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

@QasrlInstanceReader.register("question_factored")
class QasrlQuestionReader(QasrlInstanceReader):
    def __init__(self,
                 clause_info_files: List[str] = []):
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
        # abstracted clause as LabelField
        # tense/aspect/modality as LabelField
        # negation as LabelField
        # qarg as LabelField
        # construct mapping between clauses and sets of appropriate question slots
        clause_string_fields = []
        tan_string_fields = []
        qarg_fields = []
        all_answer_fields = []
        animacy_fields = []
        gold_tuples = []
        for question_label in question_labels:
            # question_text_field = TextField(self._tokenizer.tokenize(question_label["questionString"]), token_indexers)
            # question_slots_dict = get_question_slot_fields(question_label["questionSlots"]) if self._include_slots else {}
            clause_slots = {}
            try:
                clause_slots = self._clause_info[sentence_id][verb_index][question_label["questionString"]]["slots"]
            except KeyError:
                logger.info("Omitting instance without clause data: %s / %s / %s" % (sentence_id, pred_index, question_label["questionString"]))
                continue

            def abst_noun(x):
                return "something" if (x == "someone") else x
            clause_slots["abst-subj"] = abst_noun(clause_slots["subj"])
            clause_slots["abst-verb"] = "verb[pss]" if question_label["isPassive"] else "verb"
            clause_slots["abst-obj"] = abst_noun(clause_slots["obj"])
            clause_slots["abst-prep1-obj"] = abst_noun(clause_slots["prep1-obj"])
            clause_slots["abst-prep2-obj"] = abst_noun(clause_slots["prep2-obj"])
            clause_slots["abst-misc"] = abst_noun(clause_slots["misc"])
            abst_slot_names = ["abst-subj", "abst-verb", "abst-obj", "prep1", "abst-prep1-obj", "prep2", "abst-prep2-obj", "abst-misc"]
            clause_string = " ".join([clause_slots[slot_name] for slot_name in abst_slot_names])
            clause_string_field = LabelField(label = clause_string, label_namespace = "abst-clause-labels")
            clause_string_fields.append(clause_string_field)

            tense_string = question_label["tense"]
            perfect_string = "+pf" if question_label["isPerfect"] else "-pf"
            progressive_string = "+prog" if question_label["isProgressive"] else "-prog"
            negation_string = "+neg" if question_label["isNegated"] else "-neg"
            tan_string = " ".join([tense_string, perfect_string, progressive_string, negation_string])
            tan_string_field = LabelField(label = tan_string, label_namespace = "tan-string-labels")
            tan_string_fields.append(tan_string_field)

            qarg_fields.append(LabelField(label = clause_slots["qarg"], label_namespace = "qarg-labels"))

            answer_fields = get_answer_fields(question_label, verb_fields["text"])
            all_answer_fields.append(answer_fields)

            animacy_flag = -1
            if clause_slots["qarg"] in clause_slots and clause_slots["qarg"] == "someone":
                animacy_flag = 1
            elif clause_slots["qarg"] in clause_slots and clause_slots["qarg"] == "something":
                animacy_flag = 0
            animacy_fields.append(LabelField(label = animacy_flag, skip_indexing = True))

            for s in answer_fields["answer_spans"]:
                if s.span_start > -1:
                    gold_tuples.append((clause_string, clause_slots["qarg"], (s.span_start, s.span_end)))

        yield {
            **verb_fields,
            "clause_strings": ListField(clause_string_fields),
            "tan_strings": ListField(tan_string_fields),
            "qargs": ListField(qarg_fields),
            "answer_spans": ListField([f["answer_spans"] for f in all_answer_fields]),
            "num_answers": ListField([f["num_answers"] for f in all_answer_fields]),
            "num_invalids": ListField([f["num_invalids"] for f in all_answer_fields]),
            "animacy_flags": ListField(animacy_fields),
            "metadata": MetadataField({
                "gold_set": set(gold_tuples) # TODO make it a multiset so we can change span selection policy?
            })
        }

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
