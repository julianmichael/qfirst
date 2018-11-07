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
from allennlp.modules import Seq2SeqEncoder, TimeDistributed, TextFieldEmbedder, SpanPruner
from allennlp.modules.token_embedders import Embedding
from allennlp.modules.span_extractors.endpoint_span_extractor import EndpointSpanExtractor
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn import util
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.nn.util import batched_index_select
from allennlp.nn.util import last_dim_log_softmax
from allennlp.training.metrics import SpanBasedF1Measure

from nrl.modules.span_rep_assembly import SpanRepAssembly
from nrl.common.span import Span

from qfirst.modules.question_encoder import QuestionEncoder
from qfirst.metrics.answer_metric import AnswerMetric

from qfirst.data.util import get_slot_label_namespace

qa_objective_values = ["binary", "multinomial"]
qa_span_selection_policy_values = ["union", "majority", "weighted"]
# multinomial cannot be used with weighted
@Model.register("question_answerer")
class QuestionAnswerer(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 stacked_encoder: Seq2SeqEncoder,
                 question_encoder: QuestionEncoder,
                 predicate_feature_dim: int,
                 span_hidden_dim: int,
                 objective: str = "binary",
                 span_selection_policy: str = "weighted",
                 embedding_dropout: float = 0.0,
                 metric: AnswerMetric = AnswerMetric(),
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionAnswerer, self).__init__(vocab, regularizer)

        self._vocab = vocab

        self.span_hidden_dim = span_hidden_dim

        self.text_field_embedder = text_field_embedder
        self.predicate_feature_embedding = Embedding(2, predicate_feature_dim)

        self.embedding_dropout = Dropout(p=embedding_dropout)

        self.metric = metric

        self.stacked_encoder = stacked_encoder

        self.question_encoder = question_encoder
        self.slot_names = self.question_encoder.get_slot_names()

        self.question_lin = Linear(self.question_encoder.get_output_dim(), self.span_hidden_dim)
        self.pred_lin = Linear(self.stacked_encoder.get_output_dim(), self.span_hidden_dim)

        self.span_hidden = SpanRepAssembly(self.stacked_encoder.get_output_dim(), self.stacked_encoder.get_output_dim(), self.span_hidden_dim)
        self.span_scorer = torch.nn.Sequential(
            TimeDistributed(Linear(self.span_hidden_dim, self.span_hidden_dim)),
            TimeDistributed(torch.nn.ReLU()),
            TimeDistributed(Linear(self.span_hidden_dim, 1)))
        self.span_pruner = SpanPruner(self.span_scorer)
        self.answer_lin = TimeDistributed(Linear(self.span_hidden_dim, self.span_hidden_dim))
        self.span_pred = TimeDistributed(Linear(self.span_hidden_dim, 1))

        if objective not in qa_objective_values:
            raise ConfigurationError("QA objective must be one of the following: " + str(qa_objective_values))
        self.objective = objective
        if span_selection_policy not in qa_span_selection_policy_values:
            raise ConfigurationError("QA span selection policy must be one of the following: " + str(qa_objective_values))
        self.span_selection_policy = span_selection_policy
        if objective == "multinomial" and span_selection_policy == "weighted":
            raise ConfigurationError("Cannot use weighted span selection policy with multinomial objective.")

        if self.objective == "binary":
            self.invalid_embedding = Parameter(torch.randn(span_hidden_dim))
            self.invalid_pred = Linear(self.span_hidden_dim, 1)

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                predicate_index: torch.LongTensor,
                predicate_indicator: torch.LongTensor,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None,
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
        if question_slot_labels is None:
            raise ConfigurationError("QuestionAnswerer must receive a question as input.")

        # XXX debuggy
        # for i in range(question_slot_labels[self.slot_names[0]].size(0)):
        #     question = ""
        #     for slot_name in self.slot_names:
        #         question = question + self._vocab.get_token_from_index(question_slot_labels[slot_name][i].item(), namespace = get_slot_label_namespace(slot_name)) + " "
        #     print(question)

        embedded_text_input = self.embedding_dropout(self.text_field_embedder(text))
        mask = get_text_field_mask(text)
        embedded_predicate_indicator = self.predicate_feature_embedding(predicate_indicator.long())

        embedded_text_with_predicate_indicator = torch.cat([embedded_text_input, embedded_predicate_indicator], -1)
        batch_size, num_tokens, embedding_dim_with_predicate_feature = embedded_text_with_predicate_indicator.size()

        encoded_text = self.stacked_encoder(embedded_text_with_predicate_indicator, mask)

        span_hidden, span_mask = self.span_hidden(encoded_text, encoded_text, mask, mask)
        (top_span_hidden, top_span_mask,
         top_span_indices, top_span_scores) = self.span_pruner(span_hidden, span_mask.float(), 2 * num_tokens)
        # workaround for https://github.com/allenai/allennlp/issues/1696
        if (top_span_scores == float("-inf")).any():
            top_span_scores[top_span_scores == float("-inf")] = -1.

        text_size = mask.view(batch_size, -1).sum(1)
        pred_embedding = batched_index_select(encoded_text, predicate_index).squeeze(1)
        pred_hidden = self.pred_lin(pred_embedding).view(batch_size, 1, -1) # view is for broadcasting to spans
        question_embedding = self.question_encoder(pred_embedding, question_slot_labels)
        question_hidden = self.question_lin(question_embedding).view(batch_size, 1, -1) # view is for broadcasting to spans
        answer_hidden = self.answer_lin(top_span_hidden)

        consolidated_hidden = (pred_hidden + question_hidden) + answer_hidden
        top_span_logits = self.span_pred(F.relu(consolidated_hidden)).squeeze(-1) + top_span_scores.squeeze(-1)

        if answer_spans is not None:
            gold_span_labels = self.get_prediction_map(answer_spans,
                                                       num_tokens, num_answers,
                                                       self.span_selection_policy)
            prediction_mask = batched_index_select(gold_span_labels.unsqueeze(-1),
                                                   top_span_indices).squeeze(-1)
            if self.span_selection_policy == "union":
                gold_invalid_labels = (num_invalids > 0.0).float()
            elif self.span_selection_policy == "majority":
                gold_invalid_labels = (num_invalids >= (num_answers / 2.0)).float()
            else:
                assert self.span_selection_policy == "weighted" and self.objective == "binary"
                gold_invalid_labels = (num_invalids.float() / num_answers.float())

        if self.objective == "binary":
            top_span_probs = F.sigmoid(top_span_logits) * top_span_mask.float()
            scored_spans = self.to_scored_spans(span_mask, top_span_indices, top_span_mask, top_span_probs)

            consolidated_invalid_hidden = self.invalid_embedding + question_hidden
            invalidity_logit = self.invalid_pred(F.relu(consolidated_invalid_hidden)).squeeze(1).squeeze(1)
            invalid_prob = F.sigmoid(invalidity_logit)

            output_dict = {
                "span_mask": span_mask,
                "top_span_indices": top_span_indices,
                "top_span_mask": top_span_mask,
                "top_span_probs": top_span_probs,
                "invalid_prob": invalid_prob
            }
            if answer_spans is not None:
                span_loss = F.binary_cross_entropy_with_logits(top_span_logits, prediction_mask,
                                                               weight = top_span_mask.float(), size_average = False)
                invalidity_loss = F.binary_cross_entropy_with_logits(invalidity_logit, gold_invalid_labels, size_average = False)
                loss = span_loss + invalidity_loss

                self.metric(
                    scored_spans, [m["question_label"] for m in metadata],
                    invalid_prob.cpu(), num_invalids.cpu(), num_answers.cpu())

                output_dict["loss"] = loss
            return output_dict
        else:
            assert self.objective == "multinomial"
            batch_size = top_span_logits.size(0)
            invalidity_scores = predicate_indicator.new_zeros([batch_size]).float()
            masked_span_logits = top_span_logits + top_span_mask.float().log() # "masks out" bad spans by setting them to -Inf
            scores_with_dummy = torch.cat([invalidity_scores.unsqueeze(-1), top_span_logits], -1)
            pred_log_probs = last_dim_log_softmax(scores_with_dummy) # don't need a mask; already did it above
            pred_probs = pred_log_probs.exp()
            top_span_probs = pred_probs[..., 1:]
            invalid_prob = pred_probs[..., 0]
            output_dict = {
                "span_mask": span_mask,
                "top_span_indices": top_span_indices,
                "top_span_mask": top_span_mask,
                "top_span_probs": top_span_probs,
                "invalid_prob": invalid_prob
            }
            if answer_spans is not None:
                gold_dummy_labels = None
                gold_dummy_standin = prediction_mask.view(batch_size, -1).sum(1) == 0
                gold_dummy_labels = torch.max(gold_invalid_labels, gold_dummy_standin.float())
                gold_labels_with_dummy = torch.cat([gold_dummy_labels.unsqueeze(-1).float(), prediction_mask], -1)
                correct_log_probs = pred_log_probs + gold_labels_with_dummy.log()
                logsumexp_intermediate = -util.logsumexp(correct_log_probs)
                negative_marginal_log_likelihood = -util.logsumexp(correct_log_probs).sum()

                scored_spans = self.to_scored_spans(span_mask, top_span_indices, top_span_mask, top_span_probs)
                self.metric(
                    scored_spans, [m["question_label"] for m in metadata],
                    invalid_prob.cpu(), num_invalids.cpu(), num_answers.cpu())
                output_dict["loss"] = negative_marginal_log_likelihood

            return output_dict

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        o = output_dict
        spans = self.to_scored_spans(o["span_mask"], o["top_span_indices"], o["top_span_mask"], o["top_span_probs"])
        output_dict['spans'] = spans
        return output_dict

    def to_scored_spans(self, span_mask, top_span_indices, top_span_mask, top_span_probs):
        span_mask = span_mask.data.cpu()
        top_span_indices = top_span_indices.data.cpu()
        top_span_mask = top_span_mask.data.cpu()
        top_span_probs = top_span_probs.data.cpu()
        batch_size, num_spans = span_mask.size()
        top_spans = []
        for b in range(batch_size):
            batch_spans = []
            for start, end, i in self.start_end_range(num_spans):
                batch_spans.append(Span(start, end))
            batch_top_spans = []
            for i in range(top_span_indices.size(1)):
                if top_span_mask[b, i].item() == 1:
                    batch_top_spans.append((batch_spans[top_span_indices[b, i]], top_span_probs[b, i].item()))
            top_spans.append(batch_top_spans)
        return top_spans

    def start_end_range(self, num_spans):
        n = int(.5 * (math.sqrt(8 * num_spans + 1) -1))

        result = []
        i = 0
        for start in range(n):
            for end in range(start, n):
                result.append((start, end, i))
                i += 1

        return result

    def get_prediction_map(self, spans, seq_length, num_answerers, span_selection_policy):
        batchsize, num_spans, _ = spans.size()
        span_mask = (spans[:, :, 0] >= 0).view(batchsize, num_spans).long()
        num_labels = int((seq_length * (seq_length+1))/2)
        labels = spans.data.new().resize_(batchsize, num_labels).zero_().float()
        spans = spans.data
        arg_indexes = (2 * spans[:,:,0] * seq_length - spans[:,:,0].float().pow(2).long() + spans[:,:,0]) / 2 + (spans[:,:,1] - spans[:,:,0])
        arg_indexes = arg_indexes * span_mask.data

        for b in range(batchsize):
            for s in range(num_spans):
                if span_mask.data[b, s] > 0:
                    if span_selection_policy == "union":
                        labels[b, arg_indexes[b, s]] = 1
                    else:
                        assert span_selection_policy == "weighted" or span_selection_policy == "majority"
                        labels[b, arg_indexes[b, s]] += 1

        if span_selection_policy == "union":
            return torch.autograd.Variable(labels.float())
        else: # weighted or majority
            num_answerers_expanded_to_spans = num_answerers.view(-1, 1).expand(-1, num_labels).float()
            if span_selection_policy == "weighted":
                return torch.autograd.Variable(labels.float() / num_answerers_expanded_to_spans)
            else: # majority
                assert span_selection_policy == "majority"
                return torch.autograd.Variable((labels.float() / num_answerers_expanded_to_spans) >= 0.5).float()

        if union_gold_spans:
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
        objective = params.pop("objective", "binary")
        span_selection_policy = params.pop("span_selection_policy", "weighted")

        # absorb the parameter if it exists, but we don't use it anymore
        union_gold_spans = params.pop("union_gold_spans", False)

        metric_params = params.pop("metric", None)
        metric = AnswerMetric.from_params(metric_params) if metric_params is not None else AnswerMetric()

        initializer = InitializerApplicator.from_params(params.pop('initializer', []))
        regularizer = RegularizerApplicator.from_params(params.pop('regularizer', []))

        params.assert_empty(cls.__name__)

        return cls(vocab=vocab,
                   text_field_embedder=text_field_embedder,
                   stacked_encoder=stacked_encoder,
                   question_encoder = question_encoder,
                   predicate_feature_dim=predicate_feature_dim,
                   span_hidden_dim = span_hidden_dim,
                   objective = objective,
                   span_selection_policy = span_selection_policy,
                   metric = metric,
                   initializer = initializer,
                   regularizer = regularizer)
