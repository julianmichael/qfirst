from typing import Dict, List, TextIO, Optional, Union

from overrides import overrides

import torch
from torch.nn.modules import Dropout, Sequential, Linear, ReLU
import torch.nn.functional as F

from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, TextFieldEmbedder
from allennlp.modules.token_embedders import Embedding
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask
from allennlp.nn.util import batched_index_select

from qfirst.metrics.binary_f1 import BinaryF1

from qfirst.modules.slot_sequence_encoder import SlotSequenceEncoder
from qfirst.modules.span_selector import SpanSelector

question_injection_values = ["top", "bottom"]
@Model.register("qasrl_question_to_span")
class QuestionToSpanModel(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 sentence_encoder: Seq2SeqEncoder,
                 question_encoder: SlotSequenceEncoder,
                 span_selector: SpanSelector,
                 question_injection: str = "top",
                 classify_invalids: bool = True,
                 predicate_feature_dim: int = 100,
                 invalid_hidden_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionToSpanModel, self).__init__(vocab, regularizer)

        self._text_field_embedder = text_field_embedder
        self._sentence_encoder = sentence_encoder
        self._question_encoder = question_encoder
        self._span_selector = span_selector
        self._question_injection = question_injection
        self._classify_invalids = classify_invalids
        self._predicate_feature_dim = predicate_feature_dim
        self._invalid_hidden_dim = invalid_hidden_dim
        self._embedding_dropout = Dropout(p = embedding_dropout)

        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        if question_injection not in question_injection_values:
            raise ConfigurationError("Question injection must be one of the following: " + str(question_injection_values))

        if self._question_injection == "top":
            token_embedding_dim = self._text_field_embedder.get_output_dim() + self._predicate_feature_embedding.get_output_dim()
            encoder_input_dim = self._sentence_encoder.get_input_dim()
            if token_embedding_dim != encoder_input_dim:
                raise ConfigurationError("Combined token embedding dim %s did not match encoder input dim %s" % (token_embedding_dim, encoder_input_dim))
            injected_embedding_dim = self._sentence_encoder.get_output_dim() + self._question_encoder.get_output_dim()
            top_injection_dim = self._span_selector.get_top_injection_dim()
            if injected_embedding_dim != top_injection_dim:
                raise ConfigurationError("Sum of pred rep and question embedding dim %s did not match span selector top injection dim of %s" % (injected_embedding_dim, top_injection_dim))
            invalid_input_dim = injected_embedding_dim
        else:
            assert self._question_injection == "bottom"
            token_embedding_dim = self._text_field_embedder.get_output_dim() + \
                                  self._predicate_feature_embedding.get_output_dim() + \
                                  self._question_encoder.get_output_dim()
            encoder_input_dim = self._sentence_encoder.get_input_dim()
            if token_embedding_dim != encoder_input_dim:
                raise ConfigurationError("Combined token embedding dim %s did not match encoder input dim %s" % (token_embedding_dim, encoder_input_dim))
            top_injection_dim = self._span_selector.get_top_injection_dim()
            if top_injection_dim > 0:
                raise ConfigurationError("Span selector top injection dim (%s) must be zero when doing bottom injection" % top_injection_dim)
            text_embedder_dim = self._text_field_embedder.get_output_dim()
            question_encoder_input_dim = self._question_encoder.get_output_dim()
            if token_embedding_dim != encoder_input_dim:
                raise ConfigurationError("Text embedder dim %s did not match question encoder input dim %s" % (text_embedder_dim, question_encoder_input_dim))
            invalid_input_dim = self._sentence_encoder.get_output_dim()

        if self._classify_invalids:
            self._invalid_pred = Sequential(
                Linear(invalid_input_dim, self._invalid_hidden_dim),
                ReLU(),
                Linear(self._invalid_hidden_dim, 1))
            self._invalid_metric = BinaryF1()

    def classifies_invalids(self):
        return self._classify_invalids

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                predicate_index: torch.LongTensor,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None,
                **kwargs):
        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        slot_labels = self._get_slot_labels(**kwargs)
        if slot_labels is None:
            raise ConfigurationError("QuestionAnswerer must receive question slots as input.")

        embedded_text_input = self._embedding_dropout(self._text_field_embedder(text))
        batch_size, num_tokens, _ = embedded_text_input.size()
        text_mask = get_text_field_mask(text)
        embedded_predicate_indicator = self._predicate_feature_embedding(predicate_indicator.long())

        if self._question_injection == "top":
            full_embedded_text = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
            encoded_text = self._sentence_encoder(full_embedded_text, text_mask)
            pred_embedding = batched_index_select(encoded_text, predicate_index).squeeze(1)
            encoded_question = self._question_encoder(pred_embedding, slot_labels)
            # used below
            top_injection = torch.cat([pred_embedding,encoded_question], -1)
            invalid_input = top_injection
        else:
            assert self._question_injection == "bottom"
            pred_input_embedding = batched_index_select(embedded_text_input, predicate_index).squeeze(1)
            question_encoding = self._question_encoder(pred_input_embedding, slot_labels)
            question_encoding_expanded = question_encoding.view(batch_size, 1, -1).expand(-1, num_tokens, -1)
            full_embedded_text = torch.cat([embedded_text_input, embedded_predicate_indicator, question_encoding_expanded], -1)
            encoded_text = self._sentence_encoder(full_embedded_text, text_mask)
            # used below
            top_injection = None
            invalid_input = batched_index_select(encoded_text, predicate_index).squeeze(1)

        output_dict = self._span_selector(
            encoded_text, text_mask,
            top_injection_embedding = top_injection,
            answer_spans = answer_spans,
            num_answers = num_answers,
            metadata = metadata)

        if self._classify_invalids:
            invalid_logits = self._invalid_pred(invalid_input).squeeze(-1)
            invalid_probs = torch.sigmoid(invalid_logits)
            output_dict["invalid_prob"] = invalid_probs
            if num_invalids is not None:
                invalid_labels = (num_invalids > 0.0).float()
                invalid_loss = F.binary_cross_entropy_with_logits(invalid_logits, invalid_labels, reduction = "sum")
                output_dict["loss"] += invalid_loss
                self._invalid_metric(invalid_probs, invalid_labels)

        return output_dict

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        return self._span_selector.decode(output_dict)

    def get_metrics(self, reset: bool = False):
        span_metrics = self._span_selector.get_metrics(reset = reset)
        if not self._classify_invalids:
            return span_metrics
        else:
            invalid_metrics = self._invalid_metric.get_metric(reset = reset)
            return {
                **{ ("span-%s" % k): v for k, v in span_metrics.items() },
                **{ ("invalid-%s" % k): v for k, v in invalid_metrics.items() }
            }

    def get_slot_names(self):
        return self._question_encoder.get_slot_names()

    def _get_slot_labels(self, **kwargs):
        slot_labels = {}
        for slot_name in self.get_slot_names():
            if slot_name in kwargs and kwargs[slot_name] is not None:
                slot_labels[slot_name] = kwargs[slot_name]
        for slot_name in self.get_slot_names():
            if slot_name not in kwargs or kwargs[slot_name] is None:
                slot_labels = None
        return slot_labels
