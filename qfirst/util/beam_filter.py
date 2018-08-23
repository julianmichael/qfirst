from allennlp.common import Params

class BeamFilter():
    def __init__(self,
                 question_threshold: float,
                 span_threshold: float,
                 invalid_threshold: float):
        self.question_threshold = question_threshold
        self.span_threshold = span_threshold
        self.invalid_threshold = invalid_threshold

    def __call__(self, beam):
        """
        Non-max suppression (1D, by answer span overlap) filtering of the QA beam.
        Filter out questions with invalidity scores above the threshold or with no remaining non-overlapping spans.
        """
        filtered_beam = []

        def has_overlap(candidate_span):
            for entry in filtered_beam:
                for span in entry["spans"]:
                    if candidate_span.overlaps(span):
                        return True
            return False

        for entry in beam:
            is_likely = entry["question_prob"] > self.question_threshold
            is_valid = entry["invalidity_prob"] < self.invalid_threshold
            valid_spans = [span for span, prob in entry["spans"] if prob >= self.span_threshold]
            novel_spans = [span for span in valid_spans if not has_overlap(span)]
            if(is_likely and is_valid and len(novel_spans) > 0):
                filtered_entry = {
                    "question": entry["question"],
                    "question_prob": entry["question_prob"],
                    "spans": novel_spans
                }
                filtered_beam.append(filtered_entry)

        return filtered_beam

    @classmethod
    def from_params(cls, params: Params) -> 'BeamFilter':
        question_threshold = params.pop("question_threshold")
        span_threshold = params.pop("span_threshold")
        invalid_threshold = params.pop("invalid_threshold")
        return BeamFilter(question_threshold = question_threshold,
                         span_threshold = span_threshold,
                         invalid_threshold = invalid_threshold)
