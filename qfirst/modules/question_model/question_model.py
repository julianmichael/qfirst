import torch

from typing import List

from allennlp.data import Vocabulary
from allennlp.common import Params, Registrable

from qfirst.data.util import get_slot_label_namespace

# input: a vector of input_dim
# forward: returns logits over each slot computed using teacher forcing
# beam_decode_single: list of questions with scores (not necessarily probabilistic) computed by beam search
class QuestionModel(torch.nn.Module, Registrable):
    def __init__(self,
            vocab: Vocabulary,
            slot_names : List[str],
            input_dim: int):
        super(QuestionModel, self).__init__()
        self._vocab = vocab
        self._slot_names = slot_names
        self._input_dim = input_dim
        question_space_size = 1
        for slot_name in slot_names:
            num_values_for_slot = len(vocab.get_index_to_token_vocabulary(get_slot_label_namespace(slot_name)))
            question_space_size *= num_values_for_slot
            print("%s values for slot %s" % (num_values_for_slot, slot_name))
        print("Question generation space: %s possible questions" % question_space_size)

    def get_slot_names(self):
        return self._slot_names

    def get_input_dim(self):
        return self._input_dim

    def beam_decode_single(self, pred_rep, max_beam_size = 1):
        raise NotImplementedError
