import torch

from typing import List

from allennlp.data import Vocabulary
from allennlp.common import Params, Registrable

class QuestionSetModel(torch.nn.Module, Registrable):
    def __init__(self,
            vocab: Vocabulary,
            slot_names : List[str],
            input_dim: int):
        super(QuestionSetModel, self).__init__()
        self._vocab = vocab
        self._slot_names = slot_names
        self._input_dim = input_dim

    def get_slot_names(self):
        return self._slot_names

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QuestionSetModel':
        choice = params.pop_choice('type', cls.list_available())
        return cls.by_name(choice).from_params(vocab, params)
