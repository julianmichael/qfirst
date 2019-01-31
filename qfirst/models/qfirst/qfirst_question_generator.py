from typing import Dict, List, TextIO, Optional, Set, Tuple

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout
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
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.modules.slot_sequence_generator import SlotSequenceGenerator
from qfirst.metrics.question_metric import QuestionMetric

@Model.register("qfirst_question_generator")
class QfirstQuestionGenerator(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 question_generator: SlotSequenceGenerator,
                 sentence_encoder: Seq2SeqEncoder,
                 predicate_feature_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QfirstQuestionGenerator, self).__init__(vocab, regularizer)
        self._text_field_embedder = text_field_embedder
        self._question_generator = question_generator
        self._sentence_encoder = sentence_encoder
        self._predicate_feature_dim = predicate_feature_dim
        self._embedding_dropout = Dropout(p = embedding_dropout)

        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)
        self._encoder_output_projection = TimeDistributed(
            Linear(sentence_encoder.get_output_dim(), self._question_generator.get_input_dim())
        )

        embedding_dim_with_predicate_feature = self._text_field_embedder.get_output_dim() + self._predicate_feature_dim
        if embedding_dim_with_predicate_feature != self._sentence_encoder.get_input_dim():
            raise ConfigurationError("Input dimension of sentence encoder must be the sum of predicate feature dim and text embedding dim.")
        self.metric = QuestionMetric(vocab, self._question_generator.get_slot_names())

    def get_slot_names(self):
        return self._question_generator.get_slot_names()

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                **kwargs):
        # kwargs: Dict[str, torch.LongTensor]
        #     str: slot_name
        #     shape: batch_size

        batch_size, _ = predicate_indicator.size() # other dim: num_tokens

        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = self._get_pred_rep(text, predicate_indicator, predicate_index)

        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        gold_slot_labels = self._get_gold_slot_labels(kwargs)

        if gold_slot_labels is not None:
            # Shape: slot_name -> batch_size, slot_name_vocab_size
            slot_logits = self._question_generator(pred_rep, **gold_slot_labels)
            neg_log_likelihood = self._get_cross_entropy(slot_logits, gold_slot_labels)
            self.metric(slot_logits, gold_slot_labels, torch.ones([batch_size]), neg_log_likelihood)
            return {**slot_logits, "loss": neg_log_likelihood}
        else:
            raise ConfigurationError("QfirstQuestionGenerator requires gold labels for teacher forcing when running forward. "
                                     "You may wish to run beam_decode instead.")

    def beam_decode(self,
                    text: Dict[str, torch.LongTensor],
                    predicate_indicator: torch.LongTensor,
                    predicate_index: torch.LongTensor,
                    max_beam_size: int,
                    min_beam_probability: float):
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = self._get_pred_rep(text, predicate_indicator, predicate_index)
        return self._question_generator.beam_decode(pred_rep, max_beam_size, min_beam_probability)

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    def _get_cross_entropy(self, slot_logits, gold_slot_labels):
            xe = torch.tensor(0.)
            for slot_name in self.get_slot_names():
                slot_xe = F.cross_entropy(slot_logits[slot_name], gold_slot_labels[slot_name].squeeze(-1), reduction = "sum")
                xe += slot_xe
            return xe

    def _get_gold_slot_labels(self, instance_slot_labels_dict):
        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        gold_slot_labels = {}
        for slot_name in self.get_slot_names():
            if slot_name in instance_slot_labels_dict and instance_slot_labels_dict[slot_name] is not None:
                gold_slot_labels[slot_name] = instance_slot_labels_dict[slot_name].unsqueeze(-1)
        for slot_name in self.get_slot_names():
            if slot_name not in instance_slot_labels_dict or instance_slot_labels_dict[slot_name] is None:
                gold_slot_labels = None
        return gold_slot_labels

    def _get_pred_rep(self,
                      text: Dict[str, torch.LongTensor],
                      predicate_indicator: torch.LongTensor,
                      predicate_index: torch.LongTensor):
        # Shape: batch_size, num_tokens, embedding_dim
        embedded_text_input = self._embedding_dropout(self._text_field_embedder(text))
        # Shape: batch_size, num_tokens ?
        text_mask = get_text_field_mask(text)
        # Shape: batch_size, num_tokens, predicate_feature_dim ?
        embedded_predicate_indicator = self._predicate_feature_embedding(predicate_indicator.long())
        # Shape: batch_size, num_tokens, embedding_dim + predicate_feature_dim
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        batch_size, num_tokens, embedding_dim_with_predicate_feature = embedded_text_with_predicate_indicator.size()
        # Shape: batch_size, num_tokens, encoder_output_dim
        encoded_text = self._sentence_encoder(embedded_text_with_predicate_indicator, text_mask)
        projected_encoded_text = self._encoder_output_projection(encoded_text)
        # TODO do a batched_index_select with predicate_index instead
        # get predicate
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = projected_encoded_text.float() \
                                         .transpose(1, 2) \
                                         .matmul(predicate_indicator.view(batch_size, num_tokens, 1).float()) \
                                         .view(batch_size, self._question_generator.get_input_dim()) \
                                         .float()
        return pred_rep
