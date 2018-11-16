from typing import Dict

import torch

from allennlp.data import Vocabulary
from allennlp.common import Params, Registrable

class SpanSelector(torch.nn.Module, Registrable):
    def get_input_dim(self):
        raise NotImplementedError()
