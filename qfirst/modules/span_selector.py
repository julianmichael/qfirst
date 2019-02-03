
from typing import Dict, List, TextIO, Optional, Union

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout
import torch.nn.functional as F
from torch.nn import Parameter
import math

from allennlp.common import Params, Registrable
from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, Seq2VecEncoder, TimeDistributed, TextFieldEmbedder, SpanPruner
from allennlp.modules.token_embedders import Embedding
from allennlp.modules.span_extractors.endpoint_span_extractor import EndpointSpanExtractor
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn import util
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.nn.util import batched_index_select
from allennlp.nn.util import masked_log_softmax
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.modules.span_rep_assembly import SpanRepAssembly
from qfirst.common.span import Span

from qfirst.metrics.span_metric import SpanMetric

# TODO!!!: fix weighted span selection policy.
# right now it determines the targets. instead it should determine the loss weights.
objective_values = ["binary", "multinomial"]
gold_span_selection_policy_values = ["union", "majority", "weighted"]
# multinomial cannot be used with weighted
class SpanSelector(torch.nn.Module, Registrable):
    def __init__(self,
                 input_dim: int,
                 span_hidden_dim: int,
                 top_injection_dim: int = 0,
                 objective: str = "binary",
                 gold_span_selection_policy: str = "union",
                 pruning_ratio: float = 2.0,
                 # add_invalid_span: bool = True,
                 metric: SpanMetric = SpanMetric(),
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(SpanSelector, self).__init__()

        self.input_dim = input_dim
        self.span_hidden_dim = span_hidden_dim
        self.top_injection_dim = top_injection_dim
        self._pruning_ratio = pruning_ratio
        # self.add_invalid_span = add_invalid_span

        if objective not in objective_values:
            raise ConfigurationError("QA objective must be one of the following: " + str(qa_objective_values))
        self.objective = objective

        if gold_span_selection_policy not in gold_span_selection_policy_values:
            raise ConfigurationError("QA span selection policy must be one of the following: " + str(qa_objective_values))
        self.gold_span_selection_policy = gold_span_selection_policy

        if objective == "multinomial" and gold_span_selection_policy == "weighted":
            raise ConfigurationError("Cannot use weighted span selection policy with multinomial objective.")

        # if objective == "multinomial" and add_invalid_span:
        #     raise ConfigurationError("Cannot use an explicit invalid span with multinomial objective.")

        # if self.add_invalid_span:
        #     self.invalid_embedding = Parameter(torch.randn(span_hidden_dim))
        #     self.invalid_pred = Linear(self.span_hidden_dim, 1)

        self.metric = metric

        self.span_hidden = SpanRepAssembly(input_dim, input_dim, self.span_hidden_dim)
        self.span_scorer = torch.nn.Sequential(
            TimeDistributed(torch.nn.ReLU()),
            TimeDistributed(Linear(self.span_hidden_dim, self.span_hidden_dim)),
            TimeDistributed(torch.nn.ReLU()),
            TimeDistributed(Linear(self.span_hidden_dim, 1)))
        self.span_pruner = SpanPruner(self.span_scorer)

        if self.top_injection_dim > 0:
            self.top_injection_lin = Linear(self.top_injection_dim, self.span_hidden_dim)
            self.span_secondary_lin = TimeDistributed(Linear(self.span_hidden_dim, self.span_hidden_dim))
            self.span_pred = TimeDistributed(Linear(self.span_hidden_dim, 1))

    def get_top_injection_dim(self):
        return self.top_injection_dim

    def forward(self,  # type: ignore
                inputs: torch.LongTensor,
                input_mask: torch.LongTensor,
                top_injection_embedding: torch.LongTensor = None,
                answer_spans: torch.LongTensor = None,
                num_answers: torch.LongTensor = None, # only needs to be non-None when training with weighted or majority gold span selection policy
                metadata = None,
                **kwargs):

        if self.top_injection_dim > 0 and top_injection_embedding is None:
            raise ConfigurationError("SpanSelector with top-injection must receive top-injected embeddings.")

        batch_size, num_tokens, _ = inputs.size()

        span_hidden, span_mask = self.span_hidden(inputs, inputs, input_mask, input_mask)

        (top_span_hidden, top_span_mask,
         top_span_indices, top_span_scores) = self.span_pruner(span_hidden, span_mask.float(), int(self._pruning_ratio * num_tokens))
        top_span_mask = top_span_mask.unsqueeze(-1).float()

        # workaround for https://github.com/allenai/allennlp/issues/1696
        if (top_span_scores == float("-inf")).any():
            top_span_scores[top_span_scores == float("-inf")] = -1.

        if self.top_injection_dim > 0:
            top_injection_hidden = self.top_injection_lin(top_injection_embedding).view(batch_size, 1, -1) # broadcast to spans
            top_span_secondary_hidden = self.span_secondary_lin(top_span_hidden)
            top_span_consolidated_hidden = top_injection_hidden + top_span_secondary_hidden
            top_span_logits = self.span_pred(F.relu(top_span_consolidated_hidden)) + top_span_scores
        else:
            top_span_logits = top_span_scores

        if answer_spans is not None:
            gold_span_labels = self.get_prediction_map(answer_spans,
                                                       num_tokens, num_answers,
                                                       self.gold_span_selection_policy)
            prediction_mask = batched_index_select(gold_span_labels.unsqueeze(-1),
                                                   top_span_indices)

        if self.objective == "binary":
            top_span_probs = torch.sigmoid(top_span_logits) * top_span_mask
            output_dict = {
                "span_mask": span_mask,
                "top_span_indices": top_span_indices,
                "top_span_mask": top_span_mask,
                "top_span_logits": top_span_logits,
                "top_span_probs": top_span_probs
            }
            if answer_spans is not None:
                loss = F.binary_cross_entropy_with_logits(top_span_logits, prediction_mask,
                                                          weight = top_span_mask, reduction = "sum")
                scored_spans = self.to_scored_spans(span_mask, top_span_indices, top_span_mask, top_span_probs)
                output_dict["spans"] = scored_spans
                self.metric(scored_spans, [m["gold_spans"] for m in metadata])
                output_dict["loss"] = loss
            return output_dict
        else:
            # shouldn't be too hard to fix this up, but it's not a priority
            raise NotImplementedError
            # batch_size = top_span_logits.size(0)
            # null_scores = predicate_indicator.new_zeros([batch_size]).float()
            # masked_span_logits = top_span_logits + top_span_mask.float().log() # "masks out" bad spans by setting them to -Inf
            # scores_with_null = torch.cat([null_scores.unsqueeze(-1), top_span_logits], -1)
            # pred_log_probs = masked_log_softmax(scores_with_null) # don't need a mask; already did it above
            # pred_probs = pred_log_probs.exp()
            # top_span_probs = pred_probs[..., 1:]
            # null_prob = pred_probs[..., 0]
            # output_dict = {
            #     "span_mask": span_mask,
            #     "top_span_indices": top_span_indices,
            #     "top_span_mask": top_span_mask,
            #     "top_span_scores": top_span_scores,
            #     "top_span_probs": top_span_probs,
            #     "null_prob": null_prob
            # }

            # if answer_spans is not None:
            #     gold_dummy_labels = None
            #     gold_dummy_standin = prediction_mask.view(batch_size, -1).sum(1) == 0
            #     gold_dummy_labels = torch.max(gold_invalid_labels, gold_dummy_standin.float())
            #     gold_labels_with_dummy = torch.cat([gold_dummy_labels.unsqueeze(-1).float(), prediction_mask], -1)
            #     correct_log_probs = pred_log_probs + gold_labels_with_dummy.log()
            #     logsumexp_intermediate = -util.logsumexp(correct_log_probs)
            #     negative_marginal_log_likelihood = -util.logsumexp(correct_log_probs).sum()

            #     scored_spans = self.to_scored_spans(span_mask, top_span_indices, top_span_mask, top_span_probs)
            #     self.metric.with_explicit_invalids(
            #         self.decode(output_dict)
            #         [m["question_label"] for m in metadata],
            #         num_invalids.cpu(), num_answers.cpu())
            #     output_dict["loss"] = negative_marginal_log_likelihood

            # return output_dict

    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        if "spans" not in output_dict:
            o = output_dict
            spans = self.to_scored_spans(
                o["span_mask"], o["top_span_indices"], o["top_span_mask"], o["top_span_probs"]
            )
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
            if num_answerers is None:
                raise ConfigurationError("Number of answerers must be provided for training the weighted or majority span selection metrics.")
            num_answerers_expanded_to_spans = num_answerers.view(-1, 1).expand(-1, num_labels).float()
            if span_selection_policy == "weighted":
                return torch.autograd.Variable(labels.float() / num_answerers_expanded_to_spans)
            else: # majority
                assert span_selection_policy == "majority"
                return torch.autograd.Variable((labels.float() / num_answerers_expanded_to_spans) >= 0.5).float()

    def get_metrics(self, reset: bool = False):
        return self.metric.get_metric(reset = reset)
