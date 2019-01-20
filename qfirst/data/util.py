from typing import NamedTuple, Dict, List
from allennlp.data.token_indexers import TokenIndexer
from allennlp.data.fields import Field, IndexField, TextField, SequenceLabelField, LabelField, ListField, MetadataField, SpanField
from allennlp.data.tokenizers import Token
import codecs

from nrl.data.util import cleanse_sentence_text

# used for normal slots and abstract slots
def get_slot_label_namespace(slot_name: str) -> str:
    return "slot_%s_labels" % slot_name

def read_lines(file_path):
    if file_path.endswith('.gz'):
        with gzip.open(file_path, 'r') as f:
            for line in f:
                yield line
    else:
        with codecs.open(file_path, 'r', encoding='utf8') as f:
            for line in f:
                yield line

def read_clause_info(target, file_path):
    def get(targ, key):
        if key not in targ:
            targ[key] = {}
        return targ[key]
    def add_json(obj):
        sentence = get(target, obj["sentenceId"])
        verb = get(sentence, obj["verbIndex"])
        clause = get(verb, obj["question"])
        slot_dict = obj["slots"]
        slot_dict["qarg"] = obj["answerSlot"]
        clause["slots"] = slot_dict

    import json
    for line in read_lines(file_path):
        add_json(json.loads(line))
    return

qasrl_slot_names = ["wh", "aux", "subj", "verb", "obj", "prep", "obj2"]

abstract_slot_names = [
    ("wh", lambda x: "what" if (x == "who") else x),
    ("subj", lambda x: "something" if (x == "someone") else x),
    ("obj", lambda x: "something" if (x == "someone") else x),
    ("obj2", lambda x: "something" if (x == "someone") else x),
    ("prep", lambda x: "_" if (x == "_") else "<prep>")
]

### Field constructors

def get_verb_fields(token_indexers: Dict[str, TokenIndexer],
                    sentence_tokens: List[str],
                    verb_index: int):
    text_field = TextField([Token(t) for t in cleanse_sentence_text(sentence_tokens)], token_indexers)
    return {
        "text": text_field,
        "predicate_index": IndexField(verb_index, text_field),
        "predicate_indicator": SequenceLabelField(
            [1 if i == verb_index else 0 for i in range(len(sentence_tokens))], text_field)
    }

def get_question_slot_fields(question_slots):
    """
    Input: json dicty thing from QA-SRL Bank
    Output: dict from question slots to fields
    """
    def get_slot_value_field(slot_name):
        slot_value = question_slots[slot_name]
        namespace = get_slot_label_namespace(slot_name)
        return LabelField(label = slot_value, label_namespace = namespace)
    return { slot_name : get_slot_value_field(slot_name) for slot_name in qasrl_slot_names }

# def get_abstract_question_slots(question_label):
#     def get_abstract_slot_value(slot_name, get_abstracted_value):
#         return get_abstracted_value(question_label["questionSlots"][slot_name])
#     abstract_slots_dict = { ("abst-%s" % slot_name): get_abstract_slot_value(slot_name, get_abstracted_value)
#                                    for slot_name, get_abstracted_value in abstract_slot_names }
#     abst_verb_value = "verb[pss]" if question_label["isPassive"] else "verb"
#     return {**abstract_slots_dict, **{"abst-verb": abst_verb_value}}

def get_abstract_question_slot_fields(question_label):
    def get_abstract_slot_value_field(slot_name, get_abstracted_value):
        abst_slot_name = "abst-%s" % slot_name
        namespace = get_slot_label_namespace(abst_slot_name)
        abst_slot_value = get_abstracted_value(question_label["questionSlots"][slot_name])
        return LabelField(label = abst_slot_value, label_namespace = namespace)
    direct_abstract_slots_dict = { ("abst-%s" % slot_name): get_abstract_slot_value_field(slot_name, get_abstracted_value)
                                for slot_name, get_abstracted_value in abstract_slot_names }
    abst_verb_value = "verb[pss]" if question_label["isPassive"] else "verb"
    abst_verb_field = LabelField(label = abst_verb_value, label_namespace = get_slot_label_namespace("abst-verb"))
    return {**direct_abstract_slots_dict, **{"abst-verb": abst_verb_field}}

def get_clause_slot_field(slot_name: str, slot_value: str):
    clause_slot_name = "clause-%s" % slot_name
    namespace = get_slot_label_namespace(clause_slot_name)
    return LabelField(label = slot_value, label_namespace = namespace)

def get_answer_spans_field(label, text_field):
    def get_spans(spansJson):
        return [SpanField(s[0], s[1]-1, text_field) for s in spansJson]
    span_list = [s for ans in label["answerJudgments"] if ans["isValid"] for s in get_spans(ans["spans"]) ]
    if len(span_list) == 0:
        return ListField([SpanField(-1, -1, text_field)])
    else:
        return ListField(span_list)

def get_num_answers_field(question_label):
    return LabelField(label = len(question_label["answerJudgments"]), skip_indexing = True)

def get_num_invalids_field(question_label):
    num_invalids = len(question_label["answerJudgments"]) - len([aj for aj in question_label["answerJudgments"] if aj["isValid"]])
    return LabelField(label = num_invalids, skip_indexing = True)

def get_answer_fields(question_label, text_field):
    answer_spans_field = get_answer_spans_field(question_label, text_field)
    num_answers_field = get_num_answers_field(question_label)
    num_invalids_field = get_num_invalids_field(question_label)
    return {
        "answer_spans": answer_spans_field,
        "num_answers": num_answers_field,
        "num_invalids": num_invalids_field
    }
