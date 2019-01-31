from typing import Dict, Optional

import torch
from torch.nn.modules import Dropout

from overrides import overrides

from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, TextFieldEmbedder
from allennlp.modules.token_embedders import Embedding
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask

from qfirst.modules.span_selector import SpanSelector

# Slightly modified re-implementation of Nicholas's span detector
@Model.register("afirst_span_detector")
class AfirstSpanDetector(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 sentence_encoder: Seq2SeqEncoder,
                 span_selector: SpanSelector,
                 predicate_feature_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(AfirstSpanDetector, self).__init__(vocab, regularizer)

        self._vocab = vocab
        self._text_field_embedder = text_field_embedder
        self._sentence_encoder = sentence_encoder
        self._span_selector = span_selector
        self._predicate_feature_dim = predicate_feature_dim
        self._embedding_dropout = Dropout(p = embedding_dropout)

        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        token_embedding_dim = self._text_field_embedder.get_output_dim() + self._predicate_feature_dim
        if token_embedding_dim != self._sentence_encoder.get_input_dim():
            raise ConfigurationError("Combined token embedding dim %s did not match encoder input dim %s" % (token_embedding_dim, encoder_input_dim))
        if self._span_selector.get_top_injection_dim() > 0:
            raise ConfigurationError("No top-injected content is allowed in the answer-first span detection model")

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor = None,
                answer_spans: torch.LongTensor = None,
                metadata = None,
                **kwargs):
        embedded_text_input = self._embedding_dropout(self._text_field_embedder(text))
        batch_size, num_tokens, _ = embedded_text_input.size()
        text_mask = get_text_field_mask(text)
        # text_size = mask.view(batch_size, -1).sum(1)
        embedded_predicate_indicator = self._predicate_feature_embedding(predicate_indicator.long())
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        encoded_text = self._sentence_encoder(embedded_text_with_predicate_indicator, text_mask)
        return self._span_selector(
            encoded_text, text_mask,
            top_injection_embedding = None,
            answer_spans = answer_spans,
            num_answers = None,
            metadata = metadata)

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        return self._span_selector.decode(output_dict)

    def get_metrics(self, reset: bool = False):
        return self._span_selector.metric.get_metric(reset = reset)
