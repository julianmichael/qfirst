
from typing import Dict, List, TextIO, Optional, Union

from overrides import overrides
import torch
from torch.nn.modules import Linear, Dropout, ReLU
import torch.nn.functional as F
from torch.nn import Parameter
import math

from allennlp.common import Params, Registrable
from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, Seq2VecEncoder, TimeDistributed, TextFieldEmbedder, Pruner, FeedForward
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

objective_values = ["binary", "density_mle"]
class SpanSelector(torch.nn.Module, Registrable):
    def __init__(self,
                 input_dim: int,
                 extra_input_dim: int = 0,
                 span_hidden_dim: int = 100,
                 span_ffnn: FeedForward = None,
                 objective: str = "binary",
                 uncertainty_factor: float = 2.0,
                 span_probability_threshold: float = 0.05,
                 skip_metrics_during_training: bool = False,
                 metric: SpanMetric = SpanMetric(),
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(SpanSelector, self).__init__()

        self._input_dim = input_dim
        self._extra_input_dim = extra_input_dim
        self._span_hidden_dim = span_hidden_dim
        self._span_ffnn = span_ffnn
        self._objective = objective
        self._uncertainty_factor = uncertainty_factor
        self._span_probability_threshold = span_probability_threshold
        self._skip_metrics_during_training = skip_metrics_during_training

        if objective not in objective_values:
            raise ConfigurationError("QA objective must be one of the following: " + str(qa_objective_values))

        self._metric = metric

        self._span_hidden = SpanRepAssembly(self._input_dim, self._input_dim, self._span_hidden_dim)
        if self._span_ffnn is not None:
            if self._span_ffnn.get_input_dim() != self._span_hidden_dim:
                raise ConfigurationError(
                    "Span hidden dim %s must match span classifier FFNN input dim %s" % (
                        self._span_hidden_dim, self._span_ffnn.get_input_dim()
                    )
                )
            self._span_scorer = TimeDistributed(
                torch.nn.Sequential(
                    ReLU(),
                    self._span_ffnn,
                    Linear(self._span_ffnn.get_output_dim(), 1)))
        else:
            self._span_scorer = TimeDistributed(
                torch.nn.Sequential(
                    ReLU(),
                    Linear(self._span_hidden_dim, 1)))
        self._span_pruner = Pruner(self._span_scorer)

        if self._extra_input_dim > 0:
            self._extra_input_lin = Linear(self._extra_input_dim, self._span_hidden_dim)

    def get_extra_input_dim(self):
        return self._extra_input_dim

    def forward(self,  # type: ignore
                inputs: torch.LongTensor,
                input_mask: torch.LongTensor,
                extra_input_embedding: torch.LongTensor = None,
                answer_spans: torch.LongTensor = None,
                span_counts: torch.LongTensor = None,
                num_answers: torch.LongTensor = None,
                metadata = None,
                **kwargs):

        if self._extra_input_dim > 0 and extra_input_embedding is None:
            raise ConfigurationError("SpanSelector with extra input configured must receive extra input embeddings.")

        batch_size, num_tokens, _ = inputs.size()
        span_hidden, span_mask = self._span_hidden(inputs, inputs, input_mask, input_mask)

        if self._extra_input_dim > 0:
            full_hidden = self._extra_input_lin(extra_input_embedding).unsqueeze(1) + span_hidden
        else:
            full_hidden = span_hidden

        span_logits = self._span_scorer(span_hidden).squeeze(-1)

        output_dict = {
            "span_mask": span_mask,
            "span_logits": span_logits
        }

        if answer_spans is not None:
            if self._objective == "binary":
                gold_span_mask = (answer_spans[:, :, 0] >= 0).squeeze(-1).long()
                prediction_mask = self._get_prediction_map(answer_spans, gold_span_mask, num_tokens)
                output_dict["span_probs"] = torch.sigmoid(span_logits) * span_mask.float()
                output_dict["loss"] = F.binary_cross_entropy_with_logits(span_logits, prediction_mask, weight = span_mask.float(), size_average = False)
            else:
                assert self._objective == "density_mle"
                print("span_logits: " + str(span_logits.size()))
                # gold_span_mask = (answer_spans[:, :, 0] >= 0).squeeze(-1).long()
                gold_span_indices = self._get_span_indices(answer_spans, num_tokens)

                null_logits = span_logits.data.new().resize_(batch_size, 1).zero_()
                null_mask = torch.ones_like(span_mask).resize_(batch_size, 1)
                null_indices = gold_span_indices.data.new().resize_(batch_size, 1).zero_() # long
                null_counts = num_answers.float() / self._uncertainty_factor

                span_logits_with_null = torch.cat([null_logits, span_logits], -1)
                span_mask_with_null = torch.cat([null_mask, span_mask], -1)
                span_indices_with_null = torch.cat([null_indices, gold_span_indices + 1], -1) # shift gold indices +1 accting for 0 baseline
                span_counts_with_null = torch.cat([null_counts.unsqueeze(-1), span_counts.float()], -1)

                span_probs_with_null = util.masked_log_softmax(span_logits_with_null, span_mask_with_null)

                # print("span_probs_with_null: " + str(span_probs_with_null.size()))
                # print("gold_span_indices: " + str(gold_span_indices))
                # print("gold_span_indices + 1: " + str(gold_span_indices + 1))
                # print("null_indices: " + str(null_indices))
                print("span_indices_with_null: " + str(span_indices_with_null))
                print("span_counts_with_null: " + str(span_counts_with_null))

                loss = None
                num_gold_spans = span_indices_with_null.size(1)
                for span_num in range(num_gold_spans):
                    loss_for_span_num = F.nll_loss(span_probs_with_null, span_indices_with_null[:,span_num], reduce = False)
                    print("### SPAN NUM %s ###" % span_num)
                    print("loss_for_span_num: " + str(loss_for_span_num))
                    padded_loss_for_span_num = loss_for_span_num * span_counts_with_null[:,span_num] * span_mask_with_null[:,span_num].float()
                    print("padded_loss_for_span_num: " + str(padded_loss_for_span_num))
                    if loss is None:
                        loss = padded_loss_for_span_num.sum()
                    else:
                        loss += padded_loss_for_span_num.sum()
                output_dict["span_probs"] = span_probs_with_null[:,1:]
                output_dict["null_prob"] = span_probs_with_null[:,0]
                output_dict["loss"] = loss
            if not (self.training and self._skip_metrics_during_training):
                output_dict = self.decode(output_dict)
                self._metric(output_dict["spans"], output_dict.get("null_prob"), [m["gold_spans"] for m in metadata])

        return output_dict

    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        if "spans" not in output_dict:
            o = output_dict
            spans = self._to_scored_spans(o["span_probs"], o["span_mask"])
            output_dict["spans"] = spans
        return output_dict

    def _to_scored_spans(self, probs, score_mask):
        probs = probs.data.cpu()
        score_mask = score_mask.data.cpu()
        batch_size, num_spans = probs.size()
        spans = []
        for b in range(batch_size):
            batch_spans = []
            for start, end, i in self._start_end_range(num_spans):
                if score_mask[b, i] == 1 and probs[b, i] > self._span_probability_threshold:
                    batch_spans.append((Span(start, end), probs[b, i].item()))
            spans.append(batch_spans)
        return spans

    # def _to_scored_spans(self, span_mask, top_span_indices, top_span_mask, top_span_probs):
    #     span_mask = span_mask.data.cpu()
    #     top_span_indices = top_span_indices.data.cpu()
    #     top_span_mask = top_span_mask.data.cpu()
    #     top_span_probs = top_span_probs.data.cpu()
    #     batch_size, num_spans = span_mask.size()
    #     top_spans = []
    #     for b in range(batch_size):
    #         batch_spans = []
    #         for start, end, i in self._start_end_range(num_spans):
    #             batch_spans.append(Span(start, end))
    #         batch_top_spans = []
    #         for i in range(top_span_indices.size(1)):
    #             if top_span_mask[b, i].item() == 1:
    #                 batch_top_spans.append((batch_spans[top_span_indices[b, i]], top_span_probs[b, i].item()))
    #         top_spans.append(batch_top_spans)
    #     return top_spans

    def _start_end_range(self, num_spans):
        n = int(.5 * (math.sqrt(8 * num_spans + 1) -1))

        result = []
        i = 0
        for start in range(n):
            for end in range(start, n):
                result.append((start, end, i))
                i += 1

        return result

    def _get_prediction_map(self, spans, span_mask, seq_length):
        batchsize, num_spans, _ = spans.size()
        num_labels = int((seq_length * (seq_length+1))/2)
        labels = spans.data.new().resize_(batchsize, num_labels).zero_().float()
        spans = spans.data
        arg_indexes = (2 * spans[:,:,0] * seq_length - spans[:,:,0].float().pow(2).long() + spans[:,:,0]) / 2 + (spans[:,:,1] - spans[:,:,0])
        arg_indexes = arg_indexes * span_mask.data

        for b in range(batchsize):
            for s in range(num_spans):
                if span_mask.data[b, s] > 0:
                    labels[b, arg_indexes[b, s]] = 1

        return torch.autograd.Variable(labels)

    def _get_span_indices(self, spans, seq_length):
        batch_size, num_gold_spans, _ = spans.size()
        spans = spans.data
        labels = spans.new().resize_(batch_size, num_gold_spans).zero_()
        for b in range(batch_size):
            for s in range(num_gold_spans):
                span = spans[b, s]
                if span[0] == -1:
                    span_index = 0
                else:
                    span_index = (2 * span[0] * seq_length - span[0].float().pow(2).long() + span[0]) / 2 + (span[1] - span[0])
                labels[b, s] = span_index

        return torch.autograd.Variable(labels).long()

    def get_metrics(self, reset: bool = False):
        return self._metric.get_metric(reset = reset)
