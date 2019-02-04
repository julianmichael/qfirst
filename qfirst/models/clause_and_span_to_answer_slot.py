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
from allennlp.modules.span_extractors import EndpointSpanExtractor
from allennlp.modules.token_embedders import Embedding
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.nn.util import batched_index_select
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.metrics.binary_f1 import BinaryF1

@Model.register("qasrl_clause_and_span_to_answer_slot")
class ClauseAndSpanToAnswerSlotModel(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 sentence_encoder: Seq2SeqEncoder,
                 predicate_feature_dim: int = 100,
                 span_hidden_dim: int = 100,
                 final_input_dim: int = 100,
                 final_hidden_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(ClauseAndSpanToAnswerSlotModel, self).__init__(vocab, regularizer)
        self._text_field_embedder = text_field_embedder
        self._sentence_encoder = sentence_encoder
        self._predicate_feature_dim = predicate_feature_dim
        self._span_hidden_dim = span_hidden_dim
        self._final_input_dim = final_input_dim
        self._final_hidden_dim = final_hidden_dim
        self._embedding_dropout = Dropout(p = embedding_dropout)

        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        self._clause_embedding = Embedding(vocab.get_vocab_size("abst-clause-labels"), self._final_input_dim)

        self._span_extractor = EndpointSpanExtractor(input_dim = self._span_hidden_dim, combination = "x,y")
        self._span_hidden_left = TimeDistributed(Linear(self._sentence_encoder.get_output_dim(), self._span_hidden_dim))
        self._span_hidden_right = TimeDistributed(Linear(self._sentence_encoder.get_output_dim(), self._span_hidden_dim))

        self._predicate_hidden = Linear(self._sentence_encoder.get_output_dim(), self._final_input_dim)

        self._qarg_predictor = Sequential(
            Linear(self._final_input_dim, self._final_hidden_dim),
            ReLU(),
            Linear(self._final_hidden_dim, self.vocab.get_vocab_size("qarg-labels")))

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
                qarg_labeled_clauses,
                qarg_labeled_spans,
                qarg_labels = None,
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

        batch_size, num_labeled_instances, _ = qarg_labeled_spans.size()
        # Shape: batch_size, num_labeled_instances
        qarg_labeled_mask = (qarg_labeled_spans[:, :, 0] >= 0).squeeze(-1).long()
        # max to prevent the padded labels from messing up the embedding module
        # Shape: batch_size, num_labeled_instances, self._clause_embedding_dim
        input_clauses = self._clause_embedding(qarg_labeled_clauses.max(torch.zeros_like(qarg_labeled_clauses)))
        # Shape: batch_size, num_spans, 2 * encoder_output_projected_dim
        span_embeddings = self._span_extractor(encoded_text, qarg_labeled_spans, text_mask, qarg_labeled_mask)
        span_left, span_right = torch.chunk(span_embeddings, 2, dim = -1)
        # Shape: batch_size, num_spans, self._span_hidden_dim
        input_span_hidden = self._span_hidden_left(span_left) + self._span_hidden_right(span_right)

        # Shape: batch_size, 1, self._final_input_dim
        expanded_pred_embedding = self._predicate_hidden(pred_rep) \
                                      .view(   batch_size,                     1, self._final_input_dim)
        # Shape: batch_size, num_labeled_instances, self._final_input_dim
        qarg_inputs = F.relu(expanded_pred_embedding + input_clauses + F.relu(input_span_hidden))
        # Shape: batch_size, num_labeled_instances, get_vocab_size("qarg-labels")
        qarg_logits = self._qarg_predictor(qarg_inputs)
        final_mask = qarg_labeled_mask \
            .unsqueeze(-1) \
            .expand(batch_size, num_labeled_instances, self.vocab.get_vocab_size("qarg-labels")) \
            .float()
        qarg_probs = torch.sigmoid(qarg_logits).squeeze(-1) * final_mask

        output_dict = {"logits": qarg_logits, "probs": qarg_probs}
        if qarg_labels is not None:
            output_dict["loss"] = F.binary_cross_entropy_with_logits(
                qarg_logits, qarg_labels, weight = final_mask, reduction = "sum"
            )
            self._metric(qarg_probs, qarg_labels)
        return output_dict

    def get_metrics(self, reset: bool = False):
        return self._metric.get_metric(reset=reset)
