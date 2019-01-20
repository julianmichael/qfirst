from typing import Dict, List, Optional, Set, Tuple

from allennlp.common import Registrable
from allennlp.training.metrics.metric import Metric

class AnswerMetric(Metric, Registrable):
    def __call__(self,
                 span_probs, # List[List[(Span, float)]]
                 question_labels,  # dicts corresponding to JSON object
                 invalidity_probs,
                 num_invalids,
                 num_answers):
        raise NotImplementedError


