from typing import Dict, List, TextIO, Optional, Set, Tuple

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout, Sequential, ReLU
from torch.autograd import Variable
import torch.nn.functional as F

from allennlp.common import Params
from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, TimeDistributed, TextFieldEmbedder
from allennlp.modules.token_embedders import Embedding
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, batched_index_select
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.modules.question_generator import QuestionGenerator
from qfirst.modules.question_model import QuestionModel
from qfirst.metrics.question_metric import QuestionMetric

# @QuestionGenerator.register("noctx_conditional_lm")
@Model.register("noctx_conditional_lm")
class NoCtxConditionalLM(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 question_model: QuestionModel,
                 pred_hidden_dim: int = 100,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(NoCtxConditionalLM, self).__init__(vocab, regularizer)
        self.text_field_embedder = text_field_embedder
        self.question_model = question_model
        self.pred_hidden_dim = pred_hidden_dim
        self.pred_mlp = Sequential(
            Linear(self.text_field_embedder.get_output_dim(), self.pred_hidden_dim),
            ReLU(),
            Linear(self.pred_hidden_dim, self.question_model.get_input_dim()))
        self.slot_names = question_model.get_slot_names()
        self.metric = QuestionMetric(vocab, question_model.get_slot_names())

    def _get_gold_slot_labels(self, instance_slot_labels_dict):
        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        gold_slot_labels = {}
        for slot_name in self.slot_names:
            if slot_name in instance_slot_labels_dict and instance_slot_labels_dict[slot_name] is not None:
                gold_slot_labels[slot_name] = instance_slot_labels_dict[slot_name]
        for slot_name in self.slot_names:
            if slot_name not in instance_slot_labels_dict or instance_slot_labels_dict[slot_name] is None:
                gold_slot_labels = None
        return gold_slot_labels

    def _get_pred_rep(self,
                      text: Dict[str, torch.LongTensor],
                      predicate_index: torch.LongTensor):
        # TODO embed _after_ select?
        # Shape: batch_size, num_tokens, embedding_dim
        embedded_text = self.text_field_embedder(text)
        pred_embedding = batched_index_select(embedded_text, predicate_index).squeeze(1)
        pred_rep = self.pred_mlp(pred_embedding)
        return pred_rep

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_index: torch.LongTensor,
                **kwargs):
        # predicate_indicator: batch_size, num_tokens
        # kwargs: Dict[str, torch.LongTensor]
        #     str: slot_name
        #     shape: batch_size

        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = self._get_pred_rep(text, predicate_index)

        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        gold_slot_labels = self._get_gold_slot_labels(kwargs)

        if gold_slot_labels is not None:

            # Shape: slot_name -> batch_size, slot_name_vocab_size
            slot_logits = self.question_model(pred_rep, gold_slot_labels)

            loss = 0.
            for slot_name in self.slot_names:
                slot_loss = F.cross_entropy(slot_logits[slot_name], gold_slot_labels[slot_name], size_average = False)
                loss += slot_loss

            self.metric(slot_logits, gold_slot_labels, loss.item())

            loss_dict = { "loss" : loss }

            return {**loss_dict, **slot_logits}
        else:
            raise ConfigurationError("NoCtxConditionalLM requires gold labels for teacher forcing when running forward. "
                                     "You may wish to run beam_decode_single instead.")

    def beam_decode_single(self,
                           text: Dict[str, torch.LongTensor],
                           predicate_index: torch.LongTensor,
                           max_beam_size: int,
                           min_beam_probability: float):
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = self._get_pred_rep(text, predicate_index)
        return self.question_model.beam_decode_single(pred_rep, max_beam_size, min_beam_probability)

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'NoCtxConditionalLM':
        embedder_params = params.pop("text_field_embedder")
        text_field_embedder = TextFieldEmbedder.from_params(vocab, embedder_params)
        question_model = QuestionModel.from_params(vocab, params.pop("question_model"))
        pred_hidden_dim = params.pop("pred_hidden_dim", 100)
        initializer = InitializerApplicator.from_params(params.pop('initializer', []))
        regularizer = RegularizerApplicator.from_params(params.pop('regularizer', []))
        params.assert_empty(cls.__name__)

        return cls(vocab=vocab,
                   text_field_embedder = text_field_embedder,
                   question_model = question_model,
                   pred_hidden_dim = pred_hidden_dim,
                   initializer = initializer,
                   regularizer = regularizer)
