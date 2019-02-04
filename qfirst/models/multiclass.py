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
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.nn.util import batched_index_select
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.metrics.binary_f1 import BinaryF1

@Model.register("qasrl_multiclass")
class MulticlassModel(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 sentence_encoder: Seq2SeqEncoder,
                 label_name: str,
                 label_namespace: str,
                 predicate_feature_dim: int = 100,
                 prediction_hidden_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(MulticlassModel, self).__init__(vocab, regularizer)
        self._text_field_embedder = text_field_embedder
        self._sentence_encoder = sentence_encoder
        self._predicate_feature_dim = predicate_feature_dim
        self._prediction_hidden_dim = prediction_hidden_dim
        self._embedding_dropout = Dropout(p = embedding_dropout)

        self._label_name = label_name
        self._label_namespace = label_namespace

        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)
        self._final_pred = Sequential(
            Linear(self._sentence_encoder.get_output_dim(), self._prediction_hidden_dim),
            ReLU(),
            Linear(self._prediction_hidden_dim, self.vocab.get_vocab_size(self._label_namespace))
        )

        embedding_dim_with_predicate_feature = self._text_field_embedder.get_output_dim() + self._predicate_feature_dim
        if embedding_dim_with_predicate_feature != self._sentence_encoder.get_input_dim():
            raise ConfigurationError(
                ("Input dimension of sentence encoder (%s) must be " % self._sentence_encoder.get_input_dim()) + \
                ("the sum of predicate feature dim and text embedding dim (%s)." % (embedding_dim_with_predicate_feature)))

        self._metric = BinaryF1()

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                **kwargs):
        # Shape: batch_size, num_tokens, embedding_dim
        embedded_text_input = self._embedding_dropout(self._text_field_embedder(text))
        # Shape: batch_size, num_tokens ?
        text_mask = get_text_field_mask(text)
        # Shape: batch_size, num_tokens, predicate_feature_dim ?
        embedded_predicate_indicator = self._predicate_feature_embedding(predicate_indicator.long())
        # Shape: batch_size, num_tokens, embedding_dim + predicate_feature_dim
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        # Shape: batch_size, num_tokens, encoder_output_dim
        encoded_text = self._sentence_encoder(embedded_text_with_predicate_indicator, text_mask)
        # Shape: batch_size, encoder_output_dim
        pred_rep = batched_index_select(encoded_text, predicate_index).squeeze(1)
        # Shape: batch_size, get_vocab_size("abst-clause-labels")
        logits = self._final_pred(pred_rep)
        probs = torch.sigmoid(logits)
        output_dict = { "logits": logits, "probs": probs }
        if self._label_name in kwargs and kwargs[self._label_name] is not None:
            label_set = kwargs[self._label_name]
            output_dict["loss"] = F.binary_cross_entropy_with_logits(
                logits, label_set, reduction = "sum"
            )
            self._metric(probs, label_set)
        return output_dict

    def get_metrics(self, reset: bool = False):
        return self._metric.get_metric(reset=reset)
