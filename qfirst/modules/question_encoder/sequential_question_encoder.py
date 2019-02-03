from qfirst.modules.question_encoder.question_encoder import QuestionEncoder

from typing import List, Dict

import torch

from torch.autograd import Variable
from torch.nn import Parameter
from torch.nn.modules import Linear, Dropout, Embedding, LSTMCell
import torch.nn.functional as F

from allennlp.data import Vocabulary
from allennlp.common import Params
from allennlp.modules import TimeDistributed

from qfirst.util.model_utils import block_orthonormal_initialization
from qfirst.data.util import get_slot_label_namespace

@QuestionEncoder.register("sequential")
class SequentialQuestionEncoder(QuestionEncoder):
    def __init__(self,
            vocab: Vocabulary,
            slot_names: List[str],
            input_dim: int,
            dim_rnn_hidden: int = 200,
            dim_embedding: int = 100,
            rnn_layers: int = 1,
            recurrent_dropout: float = 0.1,
            highway: bool = True,
            share_rnn_cell: bool =  False):
        super(SequentialQuestionEncoder, self).__init__(vocab, slot_names, input_dim, dim_rnn_hidden)
        self._dim_embedding = dim_embedding
        self._dim_rnn_hidden = dim_rnn_hidden
        self._rnn_layers = rnn_layers
        self._recurrent_dropout = recurrent_dropout

        slot_embedders = []
        for i, n in enumerate(self.get_slot_names()[:-1]):
            num_labels = self._vocab.get_vocab_size(get_slot_label_namespace(n))
            assert num_labels > 0, "Slot named %s has 0 vocab size"%(n)
            embedder = Embedding(num_labels, self._dim_embedding)
            self.add_module('embedder_%s'%n, embedder)
            slot_embedders.append(embedder)

        self._slot_embedders = slot_embedders

        self._highway = highway

        rnn_cells = []
        highway_nonlin = []
        highway_lin = []
        for l in range(self._rnn_layers):
            layer_cells = []
            layer_highway_nonlin = []
            layer_highway_lin = []
            shared_cell = None
            layer_input_size = self.get_input_dim() + self._dim_embedding if l == 0 else self._dim_rnn_hidden
            for i, n in enumerate(self._slot_names):
                if share_rnn_cell:
                    if shared_cell is None:
                        shared_cell = LSTMCell(layer_input_size, self._dim_rnn_hidden)
                        self.add_module('layer_%d_cell'%l, shared_cell)
                        if highway:
                            shared_highway_nonlin = Linear(layer_input_size + self._dim_rnn_hidden, self._dim_rnn_hidden)
                            shared_highway_lin = Linear(layer_input_size, self._dim_rnn_hidden, bias = False)
                            self.add_module('layer_%d_highway_nonlin'%l, shared_highway_nonlin)
                            self.add_module('layer_%d_highway_lin'%l, shared_highway_lin)
                    layer_cells.append(shared_cell)
                    if highway:
                        layer_highway_nonlin.append(shared_highway_nonlin)
                        layer_highway_lin.append(shared_highway_lin)
                else:
                    cell = LSTMCell(layer_input_size, self._dim_rnn_hidden)
                    cell.weight_ih.data.copy_(block_orthonormal_initialization(layer_input_size, self._dim_rnn_hidden, 4).t())
                    cell.weight_hh.data.copy_(block_orthonormal_initialization(self._dim_rnn_hidden, self._dim_rnn_hidden, 4).t())
                    self.add_module('layer_%d_cell_%s'%(l, n), cell)
                    layer_cells.append(cell)
                    if highway:
                        nonlin = Linear(layer_input_size + self._dim_rnn_hidden, self._dim_rnn_hidden)
                        lin = Linear(layer_input_size, self._dim_rnn_hidden, bias = False)
                        nonlin.weight.data.copy_(block_orthonormal_initialization(layer_input_size + self._dim_rnn_hidden, self._dim_rnn_hidden, 1).t())
                        lin.weight.data.copy_(block_orthonormal_initialization(layer_input_size, self._dim_rnn_hidden, 1).t())
                        self.add_module('layer_%d_highway_nonlin_%s'%(l, n), nonlin)
                        self.add_module('layer_%d_highway_lin_%s'%(l, n), lin)
                        layer_highway_nonlin.append(nonlin)
                        layer_highway_lin.append(lin)

            rnn_cells.append(layer_cells)
            highway_nonlin.append(layer_highway_nonlin)
            highway_lin.append(layer_highway_lin)

        self._rnn_cells = rnn_cells
        if highway:
            self._highway_nonlin = highway_nonlin
            self._highway_lin = highway_lin

        slot_num_labels = []
        for i, n in enumerate(self._slot_names):
            num_labels = self._vocab.get_vocab_size(get_slot_label_namespace(n))
            slot_num_labels.append(num_labels)

        self._slot_num_labels = slot_num_labels

        self._start_symbol = Parameter(torch.Tensor(self._dim_embedding).normal_(0, 1))

    def forward(self,
                pred_reps,
                slot_labels: Dict[str, torch.LongTensor]):
        # Shape: batch_size, numpred_rep_dim
        batch_size, pred_rep_dim = pred_reps.size()

        # hidden state: start with batch size start symbols
        curr_embedding = self._start_symbol.view(1, -1).expand(batch_size, -1)
        # print("curr_embedding: " + str(curr_embedding.size()))

        # initialize the memory cells
        curr_mem = []
        for l in range(self._rnn_layers):
            curr_mem.append((Variable(pred_reps.data.new().resize_(batch_size, self._dim_rnn_hidden).zero_()),
                             Variable(pred_reps.data.new().resize_(batch_size, self._dim_rnn_hidden).zero_())))

        last_h = None
        for i, n in enumerate(self._slot_names):
            next_mem  = []
            curr_input = torch.cat([pred_reps, curr_embedding], -1)
            for l in range(self._rnn_layers):
                new_h, new_c = self._rnn_cells[l][i](curr_input, curr_mem[l])
                if self._recurrent_dropout > 0:
                    new_h = F.dropout(new_h, p = self._recurrent_dropout, training = self.training)
                next_mem.append((new_h, new_c))
                if self._highway:
                    nonlin = self._highway_nonlin[l][i](torch.cat([curr_input, new_h], -1))
                    gate = F.sigmoid(nonlin)
                    curr_input = gate * new_h + (1. - gate) * self._highway_lin[l][i](curr_input)
                else:
                    curr_input = new_h
            curr_mem = next_mem
            last_h = new_h

            if i < len(self._slot_names) - 1:
                curr_embedding = self._slot_embedders[i](slot_labels[n])

        return last_h

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'SequentialQuestionEncoder':
        slot_names = params.pop("slot_names")
        input_dim = params.pop("input_dim")
        rnn_layers = params.pop("rnn_layers", 1)
        share_rnn_cell = params.pop("share_rnn_cell", True)
        dim_rnn_hidden = params.pop("dim_rnn_hidden", 200)
        dim_embedding = params.pop("dim_embedding", 100)
        recurrent_dropout = params.pop("recurrent_dropout", 0.1)

        params.assert_empty(cls.__name__)

        return SequentialQuestionEncoder(vocab, slot_names, input_dim=input_dim, rnn_layers = rnn_layers, share_rnn_cell = share_rnn_cell, dim_rnn_hidden = dim_rnn_hidden, dim_embedding = dim_embedding, recurrent_dropout = recurrent_dropout)
