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

from qfirst.modules.question_model import QuestionModel
# from qfirst.modules.vector_question_generator import SequentialQuestionGenerator
from qfirst.metrics.question_metric import QuestionMetric

@Model.register("question_conditional_lm")
class QuestionConditionalLM(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 question_generator: QuestionModel,
                 stacked_encoder: Seq2SeqEncoder,
                 encoder_output_projected_dim: int = 100,
                 predicate_feature_dim: int = 100,
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionConditionalLM, self).__init__(vocab, regularizer)

        self.text_field_embedder = text_field_embedder
        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.stacked_encoder = stacked_encoder
        self.encoder_output_projected_dim = encoder_output_projected_dim
        self.encoder_output_projection = TimeDistributed(Linear(stacked_encoder.get_output_dim(), encoder_output_projected_dim))

        self.question_generator = question_generator
        self.slot_names = question_generator.get_slot_names()

        self.metric = QuestionMetric(vocab, question_generator.get_slot_names())

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                **kwargs):
        # predicate_indicator: batch_size, num_tokens
        # kwargs: Dict[str, torch.LongTensor]
        #     str: slot_name
        #     shape: batch_size

        # print("predicate_indicator: " + str(predicate_indicator.size()))

        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        gold_slot_labels = {}
        for slot_name in self.slot_names:
            if slot_name in kwargs and kwargs[slot_name] is not None:
                gold_slot_labels[slot_name] = kwargs[slot_name]
        for slot_name in self.slot_names:
            if slot_name not in kwargs or kwargs[slot_name] is None:
                gold_slot_labels = None

        # print("slot (wh): " + str(gold_slot_labels["wh"].size()))
        # print("slot (aux): " + str(gold_slot_labels["aux"].size()))

        # Shape: batch_size, num_tokens, embedding_dim
        embedded_text_input = self.embedding_dropout(self.text_field_embedder(text))
        # print("embedded text: " + str(embedded_text_input.size()))

        # Shape: batch_size, num_tokens ?
        text_mask = get_text_field_mask(text)
        # Shape: batch_size, num_tokens, predicate_feature_dim ?
        embedded_predicate_indicator = self.predicate_feature_embedding(predicate_indicator.long())
        # print("embedded predicate indicator: " + str(embedded_predicate_indicator.size()))
 
        # Shape: batch_size, num_tokens, embedding_dim + predicate_feature_dim
        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        # print("embedded text with predicate indicator: " + str(embedded_text_with_predicate_indicator.size()))

        batch_size, num_tokens, embedding_dim_with_predicate_feature = embedded_text_with_predicate_indicator.size()

        if self.stacked_encoder.get_input_dim() != embedding_dim_with_predicate_feature:
            raise ConfigurationError("The SRL model uses an indicator feature, which makes "
                                     "the embedding dimension one larger than the value "
                                     "specified. Therefore, the 'input_dim' of the stacked_encoder "
                                     "must be equal to total_embedding_dim + 1.")

        # Shape: batch_size, num_tokens, encoder_output_dim
        encoded_text = self.stacked_encoder(embedded_text_with_predicate_indicator, text_mask)
        projected_encoded_text = self.encoder_output_projection(encoded_text)

        # encoder_output_dim = self.stacked_encoder.get_output_dim()
        # TODO check encoder output dim matches generator input dim. is this right?
        # if encoder_output_dim() != self.question_generator.input_dim
        #    raise ConfigurationError("todo")

        # get predicate
        # Shape: batch_size, encoder_output_projected_dim
        pred_rep = projected_encoded_text.float() \
                                         .transpose(1, 2) \
                                         .matmul(predicate_indicator.view(batch_size, num_tokens, 1).float()) \
                                         .view(batch_size, self.encoder_output_projected_dim) \
                                         .float()

        if gold_slot_labels is not None:

            # Shape: slot_name -> batch_size, slot_name_vocab_size
            slot_logits = self.question_generator(pred_rep, gold_slot_labels)

            loss = 0.
            for slot_name in self.slot_names:
                slot_loss = F.cross_entropy(slot_logits[slot_name], gold_slot_labels[slot_name], size_average = False)
                loss += slot_loss

            self.metric(slot_logits, gold_slot_labels, loss.item())

            loss_dict = { "loss" : loss }

            return {**loss_dict, **slot_logits}

        # TODO: fwd for decoding / prediction setting: n-best list
        return {}

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QuestionConditionalLM':
        embedder_params = params.pop("text_field_embedder")
        text_field_embedder = TextFieldEmbedder.from_params(vocab, embedder_params)
        stacked_encoder = Seq2SeqEncoder.from_params(params.pop("stacked_encoder"))
        encoder_output_projected_dim = params.pop("encoder_output_projected_dim", 100)
        predicate_feature_dim = params.pop("predicate_feature_dim", 100)

        question_generator = QuestionModel.from_params(vocab, params.pop("question_generator"))

        initializer = InitializerApplicator.from_params(params.pop('initializer', []))
        regularizer = RegularizerApplicator.from_params(params.pop('regularizer', []))

        params.assert_empty(cls.__name__)

        return cls(vocab=vocab,
                   text_field_embedder=text_field_embedder,
                   question_generator=question_generator,
                   stacked_encoder=stacked_encoder,
                   encoder_output_projected_dim=encoder_output_projected_dim,
                   predicate_feature_dim=predicate_feature_dim,
                   initializer=initializer,
                   regularizer=regularizer)
