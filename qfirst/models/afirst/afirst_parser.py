from typing import Dict

from overrides import overrides

import torch

from allennlp.models.model import Model
from allennlp.common import Params
from allennlp.data import Vocabulary
from allennlp.data.dataset import Batch

import math

from itertools import groupby

from qfirst.common.span import Span
from qfirst.models.afirst.afirst_span_detector import AfirstSpanDetector
from qfirst.models.afirst.afirst_question_generator import AfirstQuestionGenerator

# "Reproduction" of Nicholas's model. Actual logic is in the predictor.
@Model.register("afirst_parser")
class AfirstParser(Model):
    def __init__(self, vocab: Vocabulary,
                 span_detector: AfirstSpanDetector,
                 question_generator: AfirstQuestionGenerator):
        super(AfirstParser, self).__init__(vocab)
        self._span_detector = span_detector
        self._question_generator = question_generator

    def get_span_detector(self):
        return self._span_detector

    def get_question_generator(self):
        return self._question_generator

    @overrides
    def forward(self, **kwargs):
        raise NotImplementedError
