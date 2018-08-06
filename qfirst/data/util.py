from typing import NamedTuple

def get_slot_label_namespace(slot_name: str) -> str:
    return "slot_%s_labels" % slot_name

# not used right now

# class VerbInflectedForms(NamedTuple):
#     stem: str
#     presentSingular3rd: str
#     presentParticiple: str
#     past: str
#     pastParticiple: str

# class Verb(NamedTuple):
#     sentence: QASRLSentence
#     verb_index: int
#     verb_inflected_forms: VerbInflectedForms
#     questions: List[QuestionLabel]

# class Sentence(NamedTuple):
#     sentence_id: str
#     tokens: List[str]
#     verbs: List[Verb]
