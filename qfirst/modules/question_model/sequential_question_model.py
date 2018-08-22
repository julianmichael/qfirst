from qfirst.modules.question_model.question_model import QuestionModel

from typing import List, Dict

import torch

from torch.autograd import Variable
from torch.nn import Parameter
from torch.nn.modules import Linear, Dropout, Embedding, LSTMCell
import torch.nn.functional as F

from allennlp.data import Vocabulary
from allennlp.common import Params
from allennlp.modules import TimeDistributed

from nrl.util.model_utils import block_orthonormal_initialization
from qfirst.data.util import get_slot_label_namespace

@QuestionModel.register("sequential")
class SequentialQuestionModel(QuestionModel):
    def __init__(self,
            vocab: Vocabulary,
            slot_names: List[str],
            input_dim: int,
            dim_slot_hidden: int = 100,
            dim_rnn_hidden: int = 200,
            dim_embedding: int = 100,
            rnn_layers: int = 1,
            recurrent_dropout: float = 0.1,
            highway: bool = True,
            share_rnn_cell: bool =  False,
            share_slot_hidden: bool = False):
        super(SequentialQuestionModel, self).__init__(vocab, slot_names, input_dim)
        self._dim_embedding = dim_embedding
        self._dim_slot_hidden = dim_slot_hidden
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
            layer_input_size = self._input_dim + self._dim_embedding if l == 0 else self._dim_rnn_hidden
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

        shared_slot_hidden = None
        slot_hiddens = []
        slot_preds = []
        slot_num_labels = []
        for i, n in enumerate(self._slot_names):
            num_labels = self._vocab.get_vocab_size(get_slot_label_namespace(n))
            slot_num_labels.append(num_labels)

            if share_slot_hidden:
                if shared_slot_hidden is None:
                    shared_slot_hidden = Linear(self._dim_rnn_hidden, self._dim_slot_hidden)
                    self.add_module('slot_hidden', shared_slot_hidden)
                slot_hiddens.append(shared_slot_hidden)
            else:
                slot_hidden = Linear(self._dim_rnn_hidden, self._dim_slot_hidden)
                slot_hiddens.append(slot_hidden)
                self.add_module('slot_hidden_%s'%n, slot_hidden)

            slot_pred = Linear(self._dim_slot_hidden, num_labels)
            slot_preds.append(slot_pred)
            self.add_module('slot_pred_%s'%n, slot_pred)

        self._slot_hiddens = slot_hiddens
        self._slot_preds = slot_preds
        self._slot_num_labels = slot_num_labels

        self._start_symbol = Parameter(torch.Tensor(self._dim_embedding).normal_(0, 1))

    def _slot_quasi_recurrence(self,
                              slot_index,
                              slot_name,
                              pred_reps,
                              curr_embedding,
                              curr_mem):

        next_mem  = []
        curr_input = torch.cat([pred_reps, curr_embedding], -1)
        for l in range(self._rnn_layers):
            new_h, new_c = self._rnn_cells[l][slot_index](curr_input, curr_mem[l])
            if self._recurrent_dropout > 0:
                new_h = F.dropout(new_h, p = self._recurrent_dropout, training = self.training)
            next_mem.append((new_h, new_c))
            if self._highway:
                nonlin = self._highway_nonlin[l][slot_index](torch.cat([curr_input, new_h], -1))
                gate = F.sigmoid(nonlin)
                curr_input = gate * new_h + (1. - gate) * self._highway_lin[l][slot_index](curr_input)
            else:
                curr_input = new_h
        # curr_mem = next_mem
        hidden = F.relu(self._slot_hiddens[slot_index](new_h))
        logits = self._slot_preds[slot_index](hidden)

        # TODO how do we choose our set of outputs and the next input?
        # different in training (with teacher forcing) and in prediction (with beam search).
        # take various assigned variables and put them in a dict to return.

        return {
            "next_mem": next_mem,
            "logits": logits
        }

    def _init_recurrence(self, pred_reps):
        # start with batch_size start symbols and init multi-layer memory cell
        batch_size, _ = pred_reps.size()
        emb = self._start_symbol.view(1, -1).expand(batch_size, -1)
        mem = []
        for l in range(self._rnn_layers):
            mem.append((Variable(pred_reps.data.new().resize_(batch_size, self._dim_rnn_hidden).zero_()),
                        Variable(pred_reps.data.new().resize_(batch_size, self._dim_rnn_hidden).zero_())))
        return emb, mem

    def forward(self,
                pred_reps,
                slot_labels: Dict[str, torch.LongTensor],
                **kwargs):
        batch_size, pred_rep_dim = pred_reps.size()

        # TODO check input_dim == pred_rep_dim

        curr_embedding, curr_mem = self._init_recurrence(pred_reps)
        slot_logits = {}
        for i, n in enumerate(self._slot_names):
            recurrence_dict = self._slot_quasi_recurrence(i, n, pred_reps, curr_embedding, curr_mem)
            slot_logits[n] = recurrence_dict["logits"]
            curr_mem = recurrence_dict["next_mem"]

            if i < len(self._slot_names) - 1:
                curr_embedding = self._slot_embedders[i](slot_labels[n])

        return slot_logits

    def beam_decode_single(self,
                           pred_rep, # shape: 1, input_dim
                           max_beam_size = 1):
        batch_size, pred_rep_dim = pred_reps.size()
        if batch_size != 1:
            raise ConfigurationError("beam_decode_single must be run with a batch size of 1.")
        if pred_rep_dim != self.get_input_dim():
            raise ConfigurationError("predicate representation must match dimensionality of question model input.")

        ## metadata to recover sequences
        # slot_name -> List/Tensor of shape (beam_size) where value is index into slot's beam
        backpointers = {}
        # slot_name -> list (length <= beam_size) of indices indicating slot values
        slot_beam_labels = {}

        ## initialization for beam search loop
        init_embedding, init_mem = self._init_recurrence(pred_reps)
        # current state of the beam search: list of (input embedding, memory cells, log_prob), ordered by probability
        current_beam_states = [init_embedding, init_mem, 0.]

        for slot_index, slot_name in enumerate(self._slot_names):
            # list of pairs (of backpointer, slot_value_index, new_embedding, new_mem, log_prob) ?
            candidate_new_beam_states = []
            for i, (emb, mem, prev_log_prob) in enumerate(current_beam_states):
                recurrence_dict = self._slot_quasi_recurrence(slot_index, slot_name, pred_reps, emb, mem)
                next_mem = recurrence_dict["next_mem"]
                logits = recurrence_dict["logits"].squeeze()
                log_probabilities = F.log_softmax(logits)
                num_slot_values = self._vocab.get_vocab_size(get_slot_label_namespace(slot_name))
                slot_name_dict = self._vocab.get_index_to_token_vocabulary(get_slot_label_namespace(slot_name))
                for pred_slot_index in range(0, math.min(max_beam_size, num_slot_values)):
                    log_prob = log_probabilities[pred_slot_index] + prev_log_prob
                    new_input_embedding = self._slot_embedders[slot_index](pred_slot_index)
                    candidate_new_beam_states.append((i, pred_slot_index, new_input_embedding, next_mem, log_prob))
            candidate_new_beam_states.sort(key = lambda t: t[4], reverse = True)
            new_beam_states = candidate_new_beam_states[:max_beam_size]
            backpointers[slot_name] = [t[0] for t in new_beam_states]
            slot_beam_labels[slot_name] = [t[1] for t in new_beam_states]
            current_beam_states = [(t[2], t[3], t[4]) for t in new_beam_states]

        final_beam_size = len(current_beam_states)
        final_slots = {}
        for slot_name in reversed(self._slot_names):
            final_slots[slot_name] = torch.zeros([final_beam_size], dtype = torch.int32)
        final_probs = torch.zeros([final_beam_size], dtype = torch.float64)
        for beam_index in range(final_beam_size):
            final_probs[beam_index] = current_beam_states[beam_index][2]
            current_backpointer = beam_index
            for slot_name in reversed(self._slot_names):
                final_slots[slot_name][beam_index] = slot_beam_labels[slot_name][current_backpointer]
                current_backpointer = backpointers[slot_name][current_backpointer]

        return final_slots, final_probs

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'SequentialQuestionModel':
        slot_names = params.pop("slot_names")
        input_dim = params.pop("input_dim")
        dim_slot_hidden = params.pop("dim_slot_hidden")
        share_slot_hidden = params.pop("share_slot_hidden", False)
        rnn_layers = params.pop("rnn_layers", 1)
        share_rnn_cell = params.pop("share_rnn_cell", True)
        dim_rnn_hidden = params.pop("dim_rnn_hidden", 200)
        dim_slot_hidden = params.pop("dim_slot_hidden", 100)
        dim_embedding = params.pop("dim_embedding", 100)
        recurrent_dropout = params.pop("recurrent_dropout", 0.1)

        params.assert_empty(cls.__name__)

        return SequentialQuestionModel(vocab, slot_names, input_dim=input_dim, share_slot_hidden=share_slot_hidden, rnn_layers = rnn_layers, share_rnn_cell = share_rnn_cell, dim_rnn_hidden = dim_rnn_hidden, dim_slot_hidden = dim_slot_hidden, dim_embedding = dim_embedding, recurrent_dropout = recurrent_dropout)
