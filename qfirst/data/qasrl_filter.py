from typing import List
from allennlp.common import Registrable

class QasrlFilter(Registrable):
    def __init__(self,
                 min_answers: int = 1,
                 min_valid_answers: int = 0,
                 domains: List[str] = None,
                 question_sources: List[str] = None):
        self._min_answers = min_answers
        self._min_valid_answers = min_valid_answers
        self._domains = [d.lower() for d in domains] if domains is not None else None
        self._question_sources = question_sources
    def filter_sentence(self, sentence_json): # -> Iterable[Dict[str, ?]]
        is_sentence_in_domain = self._domains is None or any([d in sentence_json["sentenceId"].lower() for d in self._domains])
        base_dict = {
            "sentence_id": sentence_json["sentenceId"],
            "sentence_tokens": sentence_json["sentenceTokens"],
        }
        if is_sentence_in_domain:
            for _, verb_entry in sentence_json["verbEntries"].items():
                verb_index = verb_entry["verbIndex"]
                verb_inflected_forms = verb_entry["verbInflectedForms"]
                def is_valid(question_label):
                    answers = question_label["answerJudgments"]
                    valid_answers = [a for a in answers if a["isValid"]]
                    is_source_valid = self._question_sources is None or any([l.startswith(source) for source in self._question_sources for l in question_label["questionSources"]])
                    return (len(answers) >= self._min_answers) and (len(valid_answers) >= self._min_valid_answers) and is_source_valid
                question_labels = [l for q, l in verb_entry["questionLabels"].items() if is_valid(l)]

                verb_dict = {
                    "verb_index": verb_entry["verbIndex"],
                    "verb_inflected_forms": verb_entry["verbInflectedForms"],
                    "question_labels": question_labels
                }
                yield {**base_dict, **verb_dict}
