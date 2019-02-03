from typing import List, Dict

from allennlp.common import Registrable
from allennlp.data.token_indexers import TokenIndexer
from allennlp.data.fields import Field, IndexField, TextField, SequenceLabelField, LabelField, ListField, MetadataField, SpanField
from allennlp.data.dataset_readers.dataset_reader import DatasetReader
from allennlp.data.instance import Instance
from allennlp.data.token_indexers import SingleIdTokenIndexer, TokenIndexer
from allennlp.data.tokenizers import Token, Tokenizer, WordTokenizer

from qfirst.common.span import Span
from qfirst.data.fields.multilabel_field_new import MultiLabelField_New
from qfirst.data.util import *

from overrides import overrides

import logging
logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

class QasrlInstanceReader(Registrable):
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # Iterable[Dict[str, ?Field]]
        raise NotImplementedError

@QasrlInstanceReader.register("verb_only")
class QasrlVerbOnlyReader(QasrlInstanceReader):
    @overrides
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # Iterable[Instance]
        yield get_verb_fields(token_indexers, sentence_tokens, verb_index)

@QasrlInstanceReader.register("verb_answers")
class QasrlVerbAnswersReader(QasrlInstanceReader):
    @overrides
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # -> Iterable[Instance]
        verb_dict = get_verb_fields(token_indexers, sentence_tokens, verb_index)
        answer_spans = [s for ql in question_labels for s in get_answer_spans(ql)]
        answer_spans_field = get_answer_spans_field(answer_spans, verb_dict["text"])
        yield {
            **verb_dict,
            "answer_spans": answer_spans_field,
            "metadata": {"gold_spans": set(answer_spans)}
        }

@QasrlInstanceReader.register("verb_qas")
class QasrlVerbQAsReader(QasrlInstanceReader):
    @overrides
    def read_instances(self,
                       token_indexers: Dict[str, TokenIndexer],
                       sentence_id: str,
                       sentence_tokens: List[str],
                       verb_index: int,
                       verb_inflected_forms: Dict[str, str],
                       question_labels): # -> Iterable[Instance]
        verb_dict = get_verb_fields(token_indexers, sentence_tokens, verb_index)
        question_slot_field_lists = {}
        answer_spans = []
        for question_label in question_labels:
            question_slots_dict = get_question_slot_fields(question_label["questionSlots"])
            for span in set(get_answer_spans(question_label)):
                answer_spans.append(span)
                for k, v in question_slots_dict.items():
                    if k not in question_slot_field_lists:
                        question_slot_field_lists[k] = []
                    question_slot_field_lists[k].append(v)

        question_slot_list_fields = {
            k : ListField(v) for k, v in question_slot_field_lists.items()
        }

        if len(answer_spans) > 0:
            yield {
                **verb_dict,
                **question_slot_list_fields,
                "answer_spans": get_answer_spans_field(answer_spans, verb_dict["text"])
            }

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
                    clause_dict = self._clause_info[sentence_id][verb_index][question_label["questionString"]]
                    clause_slots_dict = { ("clause-%s" % k) : get_clause_slot_field(k, v) for k, v in clause_dict["slots"].items() }
                except KeyError:
                    logger.info("Omitting instance without clause data: %s / %s / %s" % (sentence_id, verb_index, question_label["questionString"]))
                    continue
            else:
                clause_slots_dict = {}

            question_fields = {
                **question_slots_dict, **abstract_slots_dict, **clause_slots_dict,
                'question_text': question_text_field,
            }

            answer_spans = [s for ql in question_labels for s in get_answer_spans(ql)]
            answer_fields = get_answer_fields(question_label, verb_fields["text"])

            metadata = {
                "question_label": question_label,
                "gold_spans": set(answer_spans)
            }

            yield {
                **verb_fields, **question_fields, **answer_fields,
                "metadata": metadata
            }

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
        clause_string_fields = []
        clause_strings = []
        tan_strings = []
        tan_string_fields = []
        qarg_fields = []
        all_answer_fields = []
        gold_tuples = []
        animacy_label_map = {}
        for question_label in question_labels:
            clause_slots = {}
            try:
                clause_slots = self._clause_info[sentence_id][verb_index][question_label["questionString"]]["slots"]
            except KeyError:
                logger.info("Omitting instance without clause data: %s / %s / %s" % (sentence_id, verb_index, question_label["questionString"]))
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
            clause_strings.append(clause_string)
            clause_string_fields.append(clause_string_field)

            tense_string = question_label["tense"]
            perfect_string = "+pf" if question_label["isPerfect"] else "-pf"
            progressive_string = "+prog" if question_label["isProgressive"] else "-prog"
            negation_string = "+neg" if question_label["isNegated"] else "-neg"
            tan_string = " ".join([tense_string, perfect_string, progressive_string, negation_string])
            tan_string_field = LabelField(label = tan_string, label_namespace = "tan-string-labels")
            tan_strings.append(tan_string)
            tan_string_fields.append(tan_string_field)

            qarg_fields.append(LabelField(label = clause_slots["qarg"], label_namespace = "qarg-labels"))

            answer_fields = get_answer_fields(question_label, verb_fields["text"])
            all_answer_fields.append(answer_fields)

            for span_field in answer_fields["answer_spans"]:
                if span_field.span_start > -1:
                    s = (span_field.span_start, span_field.span_end)
                    gold_tuples.append((clause_string, clause_slots["qarg"], s))

                    if s not in animacy_label_map:
                        animacy_label_map[s] = []
                    if clause_slots["qarg"] in clause_slots and clause_slots[clause_slots["qarg"]] == "someone":
                        animacy_label_map[s].append(1)
                    elif clause_slots["qarg"] in clause_slots and clause_slots[clause_slots["qarg"]] == "something":
                        animacy_label_map[s].append(0)

        all_clause_strings = set(clause_strings)
        all_spans = set([t[2] for t in gold_tuples])
        all_qargs = set([t[1] for t in gold_tuples])
        qarg_pretrain_clause_fields = []
        qarg_pretrain_span_fields = []
        qarg_pretrain_multilabel_fields = []
        for clause_string in all_clause_strings:
            for span in all_spans:
                valid_qargs = [qarg for qarg in all_qargs if (clause_string, qarg, span) in gold_tuples]
                qarg_pretrain_clause_fields.append(LabelField(clause_string, label_namespace = "abst-clause-labels"))
                qarg_pretrain_span_fields.append(SpanField(span[0], span[1], verb_fields["text"]))
                qarg_pretrain_multilabel_fields.append(MultiLabelField_New(valid_qargs, label_namespace = "qarg-labels"))

        animacy_span_fields = []
        animacy_label_fields = []
        for s, labels in animacy_label_map.items():
            if len(labels) > 0 and not (0 in labels and 1 in labels):
                animacy_span_fields.append(SpanField(s[0], s[1], verb_fields["text"]))
                animacy_label_fields.append(LabelField(label = labels[0], skip_indexing = True))

        tan_multilabel_field = MultiLabelField_New(list(set(tan_strings)), label_namespace = "tan-string-labels")

        if len(animacy_span_fields) == 0:
            animacy_span_fields = [SpanField(-1, -1, verb_fields["text"])]
            animacy_label_fields = [LabelField(label = -1, skip_indexing = True)]

        if len(clause_string_fields) > 0:
            yield {
                **verb_fields,
                "clause_strings": ListField(clause_string_fields),
                "clause_set": MultiLabelField_New(clause_strings, label_namespace = "abst-clause-labels"),
                "tan_strings": ListField(tan_string_fields),
                "qargs": ListField(qarg_fields),
                "answer_spans": ListField([f["answer_spans"] for f in all_answer_fields]),
                "num_answers": ListField([f["num_answers"] for f in all_answer_fields]),
                "num_invalids": ListField([f["num_invalids"] for f in all_answer_fields]),
                "tan_set": tan_multilabel_field,
                "animacy_spans": ListField(animacy_span_fields),
                "animacy_labels": ListField(animacy_label_fields),
                "metadata": MetadataField({
                    "gold_set": set(gold_tuples) # TODO make it a multiset so we can change span selection policy?
                }),
                "qarg_pretrain_clauses": ListField(qarg_pretrain_clause_fields),
                "qarg_pretrain_spans": ListField(qarg_pretrain_span_fields),
                "qarg_pretrain_labels": ListField(qarg_pretrain_multilabel_fields),
            }
