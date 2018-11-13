import torch

from typing import List

from allennlp.data import Vocabulary
from allennlp.common import Params, Registrable

from qfirst.data.util import get_slot_label_namespace

class QuestionEncoder(torch.nn.Module, Registrable):
    def __init__(self,
                 vocab: Vocabulary,
                 slot_names : List[str],
                 input_dim: int,
                 output_dim: int):
        super(QuestionEncoder, self).__init__()
        self._vocab = vocab
        self._slot_names = slot_names
        self._input_dim = input_dim
        self._output_dim = output_dim

    def get_slot_names(self):
        return self._slot_names

    def get_input_dim(self):
        return self._input_dim

    def get_output_dim(self):
        return self._output_dim
