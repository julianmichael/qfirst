from allennlp.models.model import Model
from allennlp.data import Vocabulary

from qfirst.models.qfirst.qfirst_question_generator import QfirstQuestionGenerator
from qfirst.models.qfirst.qfirst_question_answerer import QfirstQuestionAnswerer

@Model.register("qfirst_parser")
class QfirstParser(Model):
    def __init__(self, vocab: Vocabulary,
                 question_generator: QfirstQuestionGenerator,
                 question_answerer: QfirstQuestionAnswerer):
        super(QfirstParser, self).__init__(vocab)
        self._question_generator = question_generator
        self._question_answerer = question_answerer

    def get_question_generator(self):
        return self._question_generator

    def get_question_answerer(self):
        return self._question_answerer
