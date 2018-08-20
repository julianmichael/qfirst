from typing import Dict, List, TextIO, Optional

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout
import torch.nn.functional as F
from torch.nn import Parameter
import math

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

from nrl.modules.span_rep_assembly import SpanRepAssembly
from nrl.common.span import Span

from qfirst.modules.question_encoder import QuestionEncoder
from qfirst.metrics.answer_metric import AnswerMetric


@Model.register("question_answerer")
class QuestionAnswerer(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 stacked_encoder: Seq2SeqEncoder,
                 question_encoder: QuestionEncoder,
                 predicate_feature_dim: int,
                 span_hidden_dim: int,
                 union_gold_spans: bool = False,
                 span_thresholds: List[float] = [0.33],
                 invalid_thresholds: List[float] = [0.11],
                 embedding_dropout: float = 0.0,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionAnswerer, self).__init__(vocab, regularizer)

        self.span_hidden_dim = span_hidden_dim

        self.text_field_embedder = text_field_embedder
        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.metric = AnswerMetric(
            span_thresholds = span_thresholds,
            invalid_thresholds = invalid_thresholds)

        self.stacked_encoder = stacked_encoder
        self.invalid_embedding = Parameter(torch.randn(span_hidden_dim))

        self.question_encoder = question_encoder
        self.slot_names = self.question_encoder.get_slot_names()

        self.span_hidden = SpanRepAssembly(self.stacked_encoder.get_output_dim(), self.stacked_encoder.get_output_dim(), self.span_hidden_dim)
        self.span_pred = TimeDistributed(Linear(self.span_hidden_dim + self.question_encoder.get_output_dim() + self.stacked_encoder.get_output_dim(), 1))
        self.invalid_pred = Linear(self.span_hidden_dim + self.question_encoder.get_output_dim(), 1)

        self.union_gold_spans = union_gold_spans

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                predicate_indicator: torch.LongTensor,
                answer_spans: torch.LongTensor,
                num_answers: torch.LongTensor,
                num_invalids: torch.LongTensor,
                metadata,
                **kwargs):

        # each of gold_slot_labels[slot_name] is of
        # Shape: batch_size
        question_slot_labels = {}
        for slot_name in self.slot_names:
            if slot_name in kwargs and kwargs[slot_name] is not None:
                question_slot_labels[slot_name] = kwargs[slot_name]
        for slot_name in self.slot_names:
            if slot_name not in kwargs or kwargs[slot_name] is None:
                question_slot_labels = None

        embedded_text_input = self.embedding_dropout(self.text_field_embedder(text))
        mask = get_text_field_mask(text)
        embedded_predicate_indicator = self.predicate_feature_embedding(predicate_indicator.long())

        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        batch_size, num_tokens, embedding_dim_with_predicate_feature = embedded_text_with_predicate_indicator.size()

        # TODO fix error message
        if self.stacked_encoder.get_input_dim() != embedding_dim_with_predicate_feature:
            raise ConfigurationError("The SRL model uses an indicator feature, which makes "
                                     "the embedding dimension one larger than the value "
                                     "specified. Therefore, the 'input_dim' of the stacked_encoder "
                                     "must be equal to total_embedding_dim + 1.")

        encoded_text = self.stacked_encoder(embedded_text_with_predicate_indicator, mask)
        # print("encoded_text: " + str(encoded_text.size()))

        pred_rep = encoded_text.float() \
                               .transpose(1, 2) \
                               .matmul(predicate_indicator.view(batch_size, num_tokens, 1).float()) \
                               .view(batch_size, self.stacked_encoder.get_output_dim()) \
                               .float()

        question_embedding = self.question_encoder(pred_rep, question_slot_labels)
        # print("question_embedding: " + str(question_embedding.size()))

        span_hidden, span_mask = self.span_hidden(encoded_text, encoded_text, mask, mask)
        # print("span_hidden: " + str(span_hidden.size()))

        num_spans = span_hidden.size(1)

        question_embedding_expanded = question_embedding.view(batch_size, 1, self.question_encoder.get_output_dim()) \
                                                        .expand(-1, num_spans, -1)

        pred_rep_expanded = pred_rep.view(batch_size, 1, -1) \
                                    .expand(-1, num_spans, -1)

        span_with_question_hidden = torch.cat([span_hidden, question_embedding_expanded, pred_rep_expanded], -1)
        # print("span_with_question_hidden: " + str(span_with_question_hidden.size()))

        span_logits = self.span_pred(F.relu(span_with_question_hidden)).squeeze()
        # print("span_logits: " + str(span_logits.size()))

        # uses answer_spans
        # print("answer_spans: " + str(answer_spans.size()))
        span_label_mask = (answer_spans[:, :, 0] >= 0).squeeze(-1).long()
        # print("span_label_mask: " + str(span_label_mask.size()))
        prediction_mask = self.get_prediction_map(answer_spans, span_label_mask, num_tokens, num_answers)
        # print("prediction_mask: " + str(prediction_mask.size()))
        span_loss = F.binary_cross_entropy_with_logits(span_logits, prediction_mask, weight=span_mask.float(), size_average=False)

        # print("self.invalid_embedding: " + str(self.invalid_embedding.size()))
        expanded_invalid_embedding = self.invalid_embedding.view(1, -1).expand(batch_size, -1)
        # print("expanded_invalid_embedding: " + str(expanded_invalid_embedding.size()))
        invalid_with_question_hidden = torch.cat([expanded_invalid_embedding, question_embedding], -1)
        # print("invalid_with_question_hidden: " + str(invalid_with_question_hidden.size()))
        invalidity_logit = self.invalid_pred(F.relu(invalid_with_question_hidden)).squeeze()
        # print("invalidity_logit: " + str(invalidity_logit.size()))

        invalidity_label = num_invalids.float() / num_answers.float()
        invalidity_loss = F.binary_cross_entropy_with_logits(invalidity_logit, invalidity_label, size_average = False)

        loss = span_loss + invalidity_loss

        span_probs = F.sigmoid(span_logits) * span_mask.float()
        scored_spans = self.to_scored_spans(span_probs, span_mask)
        invalidity_prob = F.sigmoid(invalidity_logit)

        self.metric(
            scored_spans, [m["question_label"] for m in metadata],
            invalidity_prob.cpu(), num_invalids.cpu(), num_answers.cpu())

        # if not self.training:
        #     spans = self.to_scored_spans(probs, span_mask)
        #     self.threshold_metric(spans, annotations)

        # TODO add this back in when necessary
        # We need to retain the mask in the output dictionary
        # so that we can crop the sequences to remove padding
        # when we do viterbi inference in self.decode.
        # output_dict["mask"] = mask

        return {
            "span_logits": span_logits,
            "invalidity_logit": invalidity_logit,
            "span_probs": span_probs,
            "span_mask": span_mask,
            "loss": loss
        }

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        probs = output_dict['span_probs']
        mask = output_dict['span_mask']
        spans = self.to_scored_spans(probs, mask)
        output_dict['spans'] = spans
        return output_dict

    def to_scored_spans(self, probs, score_mask):
        probs = probs.data.cpu()
        score_mask = score_mask.data.cpu()
        batch_size, num_spans = probs.size()
        spans = []
        for b in range(batch_size):
            batch_spans = []
            for start, end, i in self.start_end_range(num_spans):
                if score_mask[b, i] == 1 and probs[b, i] > 0:
                    batch_spans.append((Span(start, end), probs[b, i]))
            spans.append(batch_spans)
        return spans

    def start_end_range(self, num_spans):
        n = int(.5 * (math.sqrt(8 * num_spans + 1) -1))

        result = []
        i = 0
        for start in range(n):
            for end in range(start, n):
                result.append((start, end, i))
                i += 1

        return result

    def get_prediction_map(self, spans, span_mask, seq_length, num_answerers):
        batchsize, num_spans, _ = spans.size()
        num_labels = int((seq_length * (seq_length+1))/2)
        labels = spans.data.new().resize_(batchsize, num_labels).zero_().float()
        spans = spans.data
        arg_indexes = (2 * spans[:,:,0] * seq_length - spans[:,:,0].float().pow(2).long() + spans[:,:,0]) / 2 + (spans[:,:,1] - spans[:,:,0])
        arg_indexes = arg_indexes * span_mask.data

        for b in range(batchsize):
            for s in range(num_spans):
                if span_mask.data[b, s] > 0:
                    if self.union_gold_spans:
                        labels[b, arg_indexes[b, s]] = 1
                    else:
                        labels[b, arg_indexes[b, s]] += 1

        if self.union_gold_spans:
            return torch.autograd.Variable(labels.float())
        else:
            num_answerers_expanded_to_spans = num_answerers.view(-1, 1).expand(-1, num_labels).float()
            return torch.autograd.Variable(labels.float() / num_answerers_expanded_to_spans)


    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset = reset)

    @classmethod
    def from_params(cls, vocab: Vocabulary, params: Params) -> 'QuestionAnswerer':
        embedder_params = params.pop("text_field_embedder")
        text_field_embedder = TextFieldEmbedder.from_params(vocab, embedder_params)
        stacked_encoder = Seq2SeqEncoder.from_params(params.pop("stacked_encoder"))
        question_encoder = QuestionEncoder.from_params(vocab, params.pop("question_encoder"))
        predicate_feature_dim = params.pop("predicate_feature_dim")
        span_hidden_dim = params.pop("span_hidden_dim")
        union_gold_spans = params.pop("union_gold_spans", False)
        span_thresholds = params.pop("span_thresholds", [0.33])
        invalid_thresholds = params.pop("invalid_thresholds", [0.11])

        initializer = InitializerApplicator.from_params(params.pop('initializer', []))
        regularizer = RegularizerApplicator.from_params(params.pop('regularizer', []))

        params.assert_empty(cls.__name__)

        return cls(vocab=vocab,
                   text_field_embedder=text_field_embedder,
                   stacked_encoder=stacked_encoder,
                   question_encoder = question_encoder,
                   predicate_feature_dim=predicate_feature_dim,
                   span_hidden_dim = span_hidden_dim,
                   union_gold_spans = union_gold_spans,
                   initializer=initializer,
                   regularizer=regularizer)
