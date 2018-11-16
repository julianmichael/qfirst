from typing import Dict, List, TextIO, Optional, Union

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout
import torch.nn.functional as F
from torch.nn import Parameter
import math

from allennlp.common import Params
from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, Seq2VecEncoder, TimeDistributed, TextFieldEmbedder, SpanPruner
from allennlp.modules.input_variational_dropout import InputVariationalDropout
from allennlp.modules.matrix_attention.linear_matrix_attention import LinearMatrixAttention
from allennlp.modules.token_embedders import Embedding
from allennlp.modules.span_extractors.endpoint_span_extractor import EndpointSpanExtractor
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn import util
from allennlp.training.metrics import SpanBasedF1Measure

from nrl.modules.span_rep_assembly import SpanRepAssembly
from nrl.common.span import Span

from qfirst.modules.question_encoder import QuestionEncoder
from qfirst.modules.span_selector import SpanSelector
from qfirst.metrics.answer_metric import AnswerMetric

from qfirst.data.util import get_slot_label_namespace

@Model.register("question_text_answerer")
class QuestionTextAnswerer(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 passage_encoder: Seq2SeqEncoder,
                 question_encoder: Seq2SeqEncoder,
                 residual_encoder: Seq2SeqEncoder,
                 span_selector: SpanSelector,
                 embedding_dropout: float = 0.0,
                 variational_dropout: float = 0.2,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionTextAnswerer, self).__init__(vocab, regularizer)

        self._vocab = vocab
        self._text_field_embedder = text_field_embedder
        self._passage_encoder = passage_encoder
        self._question_encoder = question_encoder
        self._residual_encoder = residual_encoder
        self._span_selector = span_selector
        self._embedding_dropout = Dropout(p = embedding_dropout)
        self._variational_dropout = InputVariationalDropout(variational_dropout)

        if question_encoder.get_output_dim() != passage_encoder.get_output_dim():
            raise ConfigurationError("Question and passage encoder output dimensions must match; were: %d, %d" % (question_encoder.get_output_dim(), passage_encoder.get_output_dim()))

        self._encoding_dim = question_encoder.get_output_dim()
        self._matrix_attention = LinearMatrixAttention(self._encoding_dim, self._encoding_dim, 'x,y,x*y')
        self._merge_atten = TimeDistributed(torch.nn.Linear(self._encoding_dim * 4, self._encoding_dim))
        self._self_attention = LinearMatrixAttention(self._encoding_dim, self._encoding_dim, 'x,y,x*y')
        self._merge_self_attention = TimeDistributed(torch.nn.Linear(self._encoding_dim * 3,
                                                                     self._encoding_dim))

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                question_text: Dict[str, torch.LongTensor],
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None,
                **kwargs):

        embedded_passage = self._embedding_dropout(self._text_field_embedder(text))
        passage_mask = util.get_text_field_mask(text).float()
        encoded_passage = self._passage_encoder(embedded_passage, passage_mask)

        embedded_question = self._text_field_embedder(question_text)
        question_mask = util.get_text_field_mask(question_text)
        encoded_question = self._question_encoder(embedded_question, question_mask)

        batch_size, passage_length, _ = embedded_passage.size()

        # Shape: (batch_size, passage_length, question_length)
        passage_question_similarity = self._matrix_attention(encoded_passage, encoded_question)
        # Shape: (batch_size, passage_length, question_length)
        passage_question_attention = util.masked_softmax(passage_question_similarity, question_mask)
        # Shape: (batch_size, passage_length, encoding_dim)
        passage_question_vectors = util.weighted_sum(encoded_question, passage_question_attention)

        # We replace masked values with something really negative here, so they don't affect the
        # max below.
        masked_similarity = util.replace_masked_values(passage_question_similarity,
                                                       question_mask.unsqueeze(1),
                                                       -1e7)

        # Shape: (batch_size, passage_length)
        question_passage_similarity = masked_similarity.max(dim=-1)[0].squeeze(-1)
        # Shape: (batch_size, passage_length)
        question_passage_attention = util.masked_softmax(question_passage_similarity, passage_mask)
        # Shape: (batch_size, encoding_dim)
        question_passage_vector = util.weighted_sum(encoded_passage, question_passage_attention)
        tiled_question_passage_vector = question_passage_vector.unsqueeze(1).expand(batch_size,
                                                                                    passage_length,
                                                                                    self._encoding_dim)

        # Shape: (batch_size, passage_length, encoding_dim * 4)
        final_merged_passage = torch.cat([encoded_passage,
                                          passage_question_vectors,
                                          encoded_passage * passage_question_vectors,
                                          encoded_passage * tiled_question_passage_vector],
                                         dim=-1)

        final_merged_passage = F.relu(self._merge_atten(final_merged_passage))

        residual_layer = self._variational_dropout(self._residual_encoder(final_merged_passage,
                                                                          passage_mask))
        self_attention_matrix = self._self_attention(residual_layer, residual_layer)

        mask = passage_mask.reshape(batch_size, passage_length, 1) \
               * passage_mask.reshape(batch_size, 1, passage_length)
        self_mask = torch.eye(passage_length, passage_length, device=self_attention_matrix.device)
        self_mask = self_mask.reshape(1, passage_length, passage_length)
        mask = mask * (1 - self_mask)

        self_attention_probs = util.masked_softmax(self_attention_matrix, mask)

        # (batch, passage_len, passage_len) * (batch, passage_len, dim) -> (batch, passage_len, dim)
        self_attention_vecs = torch.matmul(self_attention_probs, residual_layer)
        self_attention_vecs = torch.cat([self_attention_vecs, residual_layer,
                                         residual_layer * self_attention_vecs],
                                        dim=-1)
        residual_layer = F.relu(self._merge_self_attention(self_attention_vecs))

        final_merged_passage = final_merged_passage + residual_layer
        # batch_size * max_passage_len * 200? but why 200?
        final_merged_passage = self._variational_dropout(final_merged_passage)
        print("final_merged_passage: " + str(final_merged_passage.size()))

        return self._span_selector.forward(
            final_merged_passage, passage_mask,
            answer_spans,
            num_answers, num_invalids,
            metadata)

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        o = output_dict
        spans = self.span_selector.to_scored_spans(o["span_mask"], o["top_span_indices"], o["top_span_mask"], o["top_span_probs"])
        output_dict['spans'] = spans
        return output_dict

    def get_metrics(self, reset: bool = False):
        return self._span_selector.metric.get_metric(reset = reset)
