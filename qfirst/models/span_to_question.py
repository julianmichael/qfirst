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
from allennlp.modules.span_extractors.endpoint_span_extractor import EndpointSpanExtractor
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.metrics.question_metric import QuestionMetric
from qfirst.modules.slot_sequence_generator import SlotSequenceGenerator
from qfirst.modules.time_distributed_dict import TimeDistributedDict

@Model.register("qasrl_span_to_question")
class SpanToQuestionModel(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 sentence_encoder: Seq2SeqEncoder,
                 question_generator: SlotSequenceGenerator,
                 predicate_feature_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(SpanToQuestionModel, self).__init__(vocab, regularizer)
        self._text_field_embedder = text_field_embedder
        self._sentence_encoder = sentence_encoder
        self._question_generator = question_generator
        self._time_distributed_question_generator = TimeDistributedDict(self._question_generator, output_is_dict = True)
        self._predicate_feature_dim = predicate_feature_dim
        self._predicate_feature_embedding = Embedding(2, predicate_feature_dim)
        self._embedding_dropout = Dropout(p=embedding_dropout)

        self._span_extractor = EndpointSpanExtractor(self._sentence_encoder.get_output_dim(), combination="x,y")

        embedding_dim_with_predicate_feature = self._text_field_embedder.get_output_dim() + self._predicate_feature_dim
        if embedding_dim_with_predicate_feature != self._sentence_encoder.get_input_dim():
            raise ConfigurationError(
                ("Input dimension of sentence encoder (%s) must be " % self._sentence_encoder.get_input_dim()) + \
                ("the sum of predicate feature dim and text embedding dim (%s)." % (embedding_dim_with_predicate_feature)))
        if (2 * self._sentence_encoder.get_output_dim()) != self._question_generator.get_input_dim():
            raise ConfigurationError(
                ("Input dimension of question generator (%s) must be " % self._question_generator.get_input_dim()) + \
                ("double the output dimension of the sentence encoder (%s)." % (2 * self._sentence_encoder.get_output_dim())))
        self.metric = QuestionMetric(vocab, self._question_generator.get_slot_names())

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                answer_spans: torch.LongTensor,
                **kwargs):
        span_reps, span_mask = self._get_span_reps(text, predicate_indicator, answer_spans)
        gold_slot_labels = self._get_gold_slot_labels(span_mask, **kwargs)
        if gold_slot_labels is not None:
            slot_logits = self._time_distributed_question_generator(**{"inputs": span_reps, **gold_slot_labels})
            neg_log_likelihood = self._get_total_cross_entropy(slot_logits, gold_slot_labels, span_mask)
            self.metric(slot_logits, gold_slot_labels, span_mask, neg_log_likelihood)
            return {**slot_logits, "span_mask": span_mask, "loss": neg_log_likelihood}
        else:
            raise ConfigurationError("AfirstQuestionGenerator requires gold labels for teacher forcing when running forward. "
                                     "You may wish to run beam_decode instead.")

    def beam_decode(self,
                    text: Dict[str, torch.LongTensor],
                    predicate_indicator: torch.LongTensor,
                    answer_spans: torch.LongTensor,
                    max_beam_size: int,
                    min_beam_probability: float):
        # Shape: 1, num_spans, question generator input dim
        span_reps, _ = self._get_span_reps(text, predicate_indicator, answer_spans)
        batch_size, num_spans, _ = span_reps.size()
        if batch_size > 1:
            raise ConfigurationError("Must have a batch size of 1 for beam decoding (had batch size %s)" % (num_spans, batch_size))
        span_reps = span_reps.squeeze(0)
        return [self._question_generator.beam_decode(
            span_reps[i].unsqueeze(0), max_beam_size, min_beam_probability
        ) for i in range(num_spans)]

    def get_slot_names(self):
        return self._question_generator.get_slot_names()

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    def _get_total_cross_entropy(self, slot_logits, gold_slot_labels, span_mask):
        loss = 0.
        for n in self._question_generator.get_slot_names():
            slot_loss = sequence_cross_entropy_with_logits(
                slot_logits[n], gold_slot_labels[n], span_mask.unsqueeze(-1).float(),
                average = None
            ).sum()
            loss += slot_loss
        return loss

    def _get_gold_slot_labels(self, mask, **kwargs):
        slot_labels = {}
        for slot_name in self._question_generator.get_slot_names():
            if slot_name in kwargs and kwargs[slot_name] is not None:
                slot_labels[slot_name] = (kwargs[slot_name] * mask).unsqueeze(-1)
        if len(slot_labels) == 0:
            slot_labels = None
        return slot_labels

    def _get_span_reps(self, text, predicate_indicator, answer_spans):
        embedded_text_input = self._embedding_dropout(self._text_field_embedder(text))
        text_mask = get_text_field_mask(text)
        embedded_predicate_indicator = self._predicate_feature_embedding(predicate_indicator.long())
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        encoded_text = self._sentence_encoder(embedded_text_with_predicate_indicator, text_mask)
        span_mask = (answer_spans[:, :, 0] >= 0).long()
        span_reps = self._span_extractor(encoded_text, answer_spans, sequence_mask = text_mask, span_indices_mask = span_mask)
        return span_reps, span_mask
