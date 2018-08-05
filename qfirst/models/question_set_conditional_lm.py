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

from qfirst.modules.question_set_model import QuestionSetModel
from qfirst.metrics.question_set_metric import QuestionSetMetric

@Model.register("question_set_conditional_lm")
class QuestionSetConditionalLM(Model):
    def __init__(self, vocab: Vocabulary,
                text_field_embedder: TextFieldEmbedder,
                question_generator: QuestionSetModel,
                stacked_encoder: Seq2SeqEncoder = None,
                predicate_feature_dim: int = 100,
                embedding_dropout: float = 0.0,
                initializer: InitializerApplicator = InitializerApplicator(),
                regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionSetConditionalLM, self).__init__(vocab, regularizer)

        self.text_field_embedder = text_field_embedder
        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.stacked_encoder = stacked_encoder

        self.question_generator = question_generator
        self.slot_names = question_generator.get_slot_names()

        self.metric = QuestionSetMetric(vocab, question_generator.get_slot_names())

    @overrides
    def forward(self,
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                **kwargs):
        # predicate_indicator: batch_size, num_tokens
        # kwargs: Dict[str, torch.LongTensor]
        #     str: slot_name
        #     shape: batch_size, num_questions

        # print("predicate_indicator: " + str(predicate_indicator.size()))

        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size, num_questions
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
        # print("encoded text: " + str(encoded_text.size()))

        encoder_output_dim = self.stacked_encoder.get_output_dim()
        # TODO check encoder output dim matches generator input dim. is this right?
        # if encoder_output_dim() != self.question_generator.input_dim
        #    raise ConfigurationError("todo")

        # get predicate
        # Shape: batch_size, encoder_output_dim
        pred_rep = encoded_text.long().transpose(1, 2).matmul(predicate_indicator.view(batch_size, num_tokens, 1))
        # print("pred rep: " + str(pred_rep.size()))

        if gold_slot_labels is not None:
            num_questions_max = gold_slot_labels[self.slot_names[0]].size(1)
            # print("max num questions: " + str(num_questions_max))

            # Shape: batch_size, num_questions, encoder_output_dim
            pred_rep_expanded = pred_rep.view(batch_size, 1, encoder_output_dim).expand(-1, num_questions_max, -1)
            # print("pred rep expanded: " + str(pred_rep_expanded.size()))

            # Shape: batch_size, num_questions, 1? ??? TODO
            num_questions_mask = (gold_slot_labels[self.slot_names[0]][:, :] >= 0).long()
            # print("num_questions_mask: " + str(num_questions_mask.size()))

            # Shape: slot_name -> batch_size, num_questions_max, slot_name_vocab_size
            slot_logits = self.question_generator(pred_rep_expanded, gold_slot_labels, num_questions_mask)
            # print("slot_logits: " + str(slot_logits))
            # print("slot_logits[wh]: " + str(slot_logits["wh"].size()))
            # print("gold_slot_labels[wh]: " + str(gold_slot_labels["wh"].size()))
            # print("num_questions_mask: " + str(num_questions_mask.size()))

            loss = 0.
            for slot_name in self.slot_names:
                # XXX set padding to 0 token, shouldn't affect loss since masked out anyway
                these_gold_slot_labels = gold_slot_labels[slot_name]
                these_gold_slot_labels_pad_to_0 = these_gold_slot_labels + (these_gold_slot_labels[:,:] < 0).long()
                slot_loss = sequence_cross_entropy_with_logits(
                    slot_logits[slot_name],
                    these_gold_slot_labels_pad_to_0,
                    num_questions_mask.float(),
                    batch_average = False)
                loss += slot_loss.sum()

            self.metric(slot_logits, gold_slot_labels, num_questions_mask, loss.item())

            slot_logits_dict = { slot_name : slot_logits[slot_name] for slot_name in self.slot_names }
            loss_dict = { "loss" : loss }

            return {**loss_dict, **slot_logits_dict}

        # TODO: fwd for decoding / prediction setting: n-best list
        return {}

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset=reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QuestionSetConditionalLM':
        embedder_params = params.pop("text_field_embedder")
        text_field_embedder = TextFieldEmbedder.from_params(vocab, embedder_params)
        stacked_encoder = Seq2SeqEncoder.from_params(params.pop("stacked_encoder"))
        predicate_feature_dim = params.pop("predicate_feature_dim", 100)

        question_generator = QuestionSetModel.from_params(vocab, params.pop("question_generator"))

        initializer = InitializerApplicator.from_params(params.pop('initializer', []))
        regularizer = RegularizerApplicator.from_params(params.pop('regularizer', []))

        params.assert_empty(cls.__name__)

        return cls(vocab=vocab,
                   text_field_embedder=text_field_embedder,
                   stacked_encoder=stacked_encoder,
                   question_generator=question_generator,
                   predicate_feature_dim=predicate_feature_dim,
                   initializer=initializer,
                   regularizer=regularizer)
