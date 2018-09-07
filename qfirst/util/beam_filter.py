from allennlp.common import Params

class BeamFilter():
    def __init__(self,
                 question_threshold: float,
                 span_threshold: float,
                 invalid_threshold: float,
                 invalid_as_threshold: bool = False,
                 first_answer_only: bool = False):
        self.question_threshold = question_threshold
        self.span_threshold = span_threshold
        self.invalid_threshold = invalid_threshold
        self.invalid_as_threshold = invalid_as_threshold
        self.first_answer_only = first_answer_only

    def __call__(self, beam):
        """
        Non-max suppression (1D, by answer span overlap) filtering of the QA beam.
        Filter out questions with invalidity scores above the threshold or with no remaining non-overlapping spans.
        """
        filtered_beam = []

        def has_overlap(candidate_span, other_spans):
            for entry in filtered_beam:
                for span in entry["answer_spans"]:
                    if candidate_span.overlaps(span):
                        return True
            for span in other_spans:
                if candidate_span.overlaps(span):
                    return True
            return False

        entries_with_spans = [(entry, s) for entry in beam for s in entry["answer_spans"]]
        for entry in beam:
            q_prob = entry["question_prob"]
            invalid_prob = entry["invalidity_prob"]
            is_likely = q_prob > self.question_threshold
            is_valid = invalid_prob < self.invalid_threshold
            valid_spans = [(span, prob) for span, prob in entry["answer_spans"]
                           if prob >= self.span_threshold
                           and ((not self.invalid_as_threshold) or prob >= invalid_prob)]
            sorted_valid_spans = [span for span, _ in sorted(valid_spans, key = lambda t: t[1], reverse = True)]
            novel_spans = []
            for span in sorted_valid_spans:
                if not has_overlap(span, novel_spans):
                    novel_spans.append(span)
            if(is_likely and is_valid and len(novel_spans) > 0):
                if self.first_answer_only:
                    only_span, _ = max(entry["answer_spans"], key = lambda t: t[1])
                    novel_spans = [only_span]
                filtered_entry = {
                    "question": entry["question"],
                    "question_slots": entry["question_slots"],
                    "answer_spans": novel_spans
                }
                filtered_beam.append(filtered_entry)

        return filtered_beam

    @classmethod
    def from_params(cls, params: Params) -> 'BeamFilter':
        question_threshold = params.pop("question_threshold")
        span_threshold = params.pop("span_threshold")
        invalid_threshold = params.pop("invalid_threshold")
        invalid_as_threshold = params.pop("invalid_as_threshold", False)
        first_answer_only = params.pop("first_answer_only", False)
        return BeamFilter(question_threshold = question_threshold,
                          span_threshold = span_threshold,
                          invalid_threshold = invalid_threshold,
                          invalid_as_threshold = invalid_as_threshold,
                          first_answer_only = first_answer_only)
