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

from qfirst.modules.span_selector import SpanSelector
from qfirst.modules.question_encoder import QuestionEncoder
from qfirst.metrics.answer_metric import AnswerMetric

from qfirst.data.util import get_slot_label_namespace

objective_values = ["binary", "multinomial"]
gold_span_selection_policy_values = ["union", "majority", "weighted"]
invalid_repr_values = ["independent", "textual"]
@SpanSelector.register("simple")
class SimpleSpanSelector(SpanSelector):
    def __init__(self,
                 input_dim: int,
                 span_hidden_dim: int,
                 pruning_ratio: float = 2.0,
                 objective: str = "binary",
                 gold_span_selection_policy: str = "union",
                 invalid_repr: str = "independent",
                 metric: AnswerMetric = AnswerMetric(),
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None) -> None:
        super(SimpleSpanSelector, self).__init__()

        if objective not in objective_values:
            raise ConfigurationError("Span selection objective must be one of the following: " + str(objective_values))
        self.objective = objective

        if gold_span_selection_policy not in gold_span_selection_policy_values:
            raise ConfigurationError("Gold span selection policy must be one of the following: " + str(gold_span_selection_policy_values))
        self.gold_span_selection_policy = gold_span_selection_policy

        if invalid_repr not in invalid_repr_values:
            raise ConfigurationError("Invalid repr must be one of the following: " + str(invalid_repr_values))

        if objective == "multinomial" and gold_span_selection_policy == "weighted":
            raise ConfigurationError("Cannot use weighted gold span selection policy with multinomial objective.")

        if objective == "multinomial" and invalid_repr == "textual":
            raise ConfigurationError("Cannot use textual invalid repr with multinomial objective.")

        if self.objective == "binary":
            self.invalid_embedding = Parameter(torch.randn(span_hidden_dim))
            self.invalid_pred = Linear(self.span_hidden_dim, 1)

        self.metric = metric


        self.input_dim = input_dim
        self.span_hidden_dim = span_hidden_dim
        self.pruning_ratio = pruning_ratio

        self.span_hidden = SpanRepAssembly(self.input_dim, self.input_dim, self.span_hidden_dim)
        self.span_scorer = torch.nn.Sequential(
            TimeDistributed(Linear(self.span_hidden_dim, self.span_hidden_dim)),
            TimeDistributed(torch.nn.ReLU()),
            TimeDistributed(Linear(self.span_hidden_dim, 1)))
        self.span_pruner = SpanPruner(self.span_scorer)
        self.span_pred = TimeDistributed(Linear(self.span_hidden_dim, 1))

    def get_input_dim(self):
        return self.input_dim

    def forward(self,
                encoded_text: torch.LongTensor,
                text_mask: torch.LongTensor,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                num_invalids: torch.LongTensor = None,
                metadata = None):
        batch_size, num_tokens, _ = encoded_text.size()
        span_hidden, span_mask = self.span_hidden(encoded_text, encoded_text, text_mask, text_mask)
        (top_span_hidden, top_span_mask,
        top_span_indices, top_span_scores) = self.span_pruner(span_hidden, span_mask.float(), int(self.pruning_ratio * num_tokens))
        # workaround for https://github.com/allenai/allennlp/issues/1696
        # TODO: use replace_masked_values instead?
        if (top_span_scores == float("-inf")).any():
            top_span_scores[top_span_scores == float("-inf")] = -1.
        top_span_logits = top_span_scores.squeeze(-1)

        if answer_spans is not None:
            gold_span_labels = self.get_prediction_map(answer_spans,
                                                       num_tokens,
                                                       num_answers,
                                                       self.gold_span_selection_policy)
            prediction_mask = batched_index_select(gold_span_labels.unsqueeze(-1),
                                                   top_span_indices).squeeze(-1)
            if self.gold_span_selection_policy == "union":
                gold_invalid_labels = (num_invalids > 0.0).float()
            elif self.gold_span_selection_policy == "majority":
                gold_invalid_labels = (num_invalids >= (num_answers / 2.0)).float()
            else:
                assert self.gold_span_selection_policy == "weighted" and self.objective == "binary"
                gold_invalid_labels = (num_invalids.float() / num_answers.float())

        if self.objective == "binary":
            raise NotImplementedError()
            # TODO: here we must enforce that we have a CANNOTANSWER token at the end
            top_span_probs = F.sigmoid(top_span_logits) * top_span_mask.float()

            # if self.question_injection == "top" or self.question_input_type == "text":
            #     consolidated_invalid_hidden = self.invalid_embedding + question_hidden
            # else:
            #     assert self.question_injection == "bottom" and self.question_input_type == "slots"
            #     pred_embedding = batched_index_select(encoded_text, predicate_index).squeeze(1)
            #     pred_hidden = self.pred_lin(pred_embedding).view(batch_size, 1, -1) # view is for broadcasting to spans
            #     consolidated_invalid_hidden = self.invalid_embedding + pred_hidden
            # invalidity_logit = self.invalid_pred(F.relu(consolidated_invalid_hidden)).squeeze(1).squeeze(1)
            # invalid_prob = F.sigmoid(invalidity_logit)

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

                scored_spans = self.to_scored_spans(span_mask, top_span_indices, top_span_mask, top_span_probs)
                self.metric(
                    scored_spans, [m["question_label"] for m in metadata],
                    invalid_prob.cpu(), num_invalids.cpu(), num_answers.cpu())

                output_dict["loss"] = loss
            return output_dict
        else:
            assert self.objective == "multinomial"
            invalidity_scores = encoded_text.new_zeros([batch_size]).float()
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

    def start_end_range(self, num_spans):
        n = int(.5 * (math.sqrt(8 * num_spans + 1) -1))

        result = []
        i = 0
        for start in range(n):
            for end in range(start, n):
                result.append((start, end, i))
                i += 1

        return result

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

    def get_prediction_map(self, spans, seq_length, num_answerers, gold_span_selection_policy):
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
                    if gold_span_selection_policy == "union":
                        labels[b, arg_indexes[b, s]] = 1
                    else:
                        assert gold_span_selection_policy == "weighted" or gold_span_selection_policy == "majority"
                        labels[b, arg_indexes[b, s]] += 1

        if gold_span_selection_policy == "union":
            return torch.autograd.Variable(labels.float())
        else: # weighted or majority
            num_answerers_expanded_to_spans = num_answerers.view(-1, 1).expand(-1, num_labels).float()
            if gold_span_selection_policy == "weighted":
                return torch.autograd.Variable(labels.float() / num_answerers_expanded_to_spans)
            else: # majority
                assert gold_span_selection_policy == "majority"
                return torch.autograd.Variable((labels.float() / num_answerers_expanded_to_spans) >= 0.5).float()

        if union_gold_spans:
            return torch.autograd.Variable(labels.float())
        else:
            num_answerers_expanded_to_spans = num_answerers.view(-1, 1).expand(-1, num_labels).float()
            return torch.autograd.Variable(labels.float() / num_answerers_expanded_to_spans)
