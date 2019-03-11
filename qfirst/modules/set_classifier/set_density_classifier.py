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
from allennlp.models.model import Model
from allennlp.nn import util
from allennlp.nn.util import get_text_field_mask, sequence_cross_entropy_with_logits
from allennlp.nn.util import get_lengths_from_binary_sequence_mask, viterbi_decode
from allennlp.nn.util import batched_index_select
from allennlp.nn.util import masked_log_softmax
from allennlp.training.metrics import SpanBasedF1Measure

from qfirst.common.span import Span
from qfirst.metrics.binary_f1 import BinaryF1
from qfirst.metrics.moments_metric import MomentsMetric
from qfirst.modules.span_rep_assembly import SpanRepAssembly
from qfirst.modules.set_classifier.set_classifier import SetClassifier

label_selection_policy_values = ["union", "majority", "weighted"]

@SetClassifier.register("density")
class SetDensityClassifier(torch.nn.Module, Registrable):
    def __init__(self,
                 # decoding_threshold: float = 0.05,
                 use_null_calibrator: bool = True,
                 uncertainty_factor: float = 1.0,
                 skip_metrics_during_training: bool = False,
                 prob_metric: BinaryF1 = BinaryF1(),
                 score_metric: BinaryF1 = BinaryF1([-4, -3, -2, -1, 0, 1, 2])):
        super(SetDensityClassifier, self).__init__()

        # if uncertainty_factor < 1.0:
        #     raise ConfigurationError("Uncertainty factor must be >= 1.")

        self._use_null_calibrator = use_null_calibrator
        self._uncertainty_factor = uncertainty_factor
        self._skip_metrics_during_training = skip_metrics_during_training

        self._prob_metric = prob_metric
        self._score_metric = score_metric
        # self._zero_score_metric = BinaryF1([0])
        self._gold_recall_metric = MomentsMetric()
        self._kl_divergence_metric = MomentsMetric()
        self._null_prob_metric = MomentsMetric()

    def forward(self,  # type: ignore
                logits: torch.LongTensor, # batch_size, set_size, 1
                mask: torch.LongTensor = None, # batch_size, set_size
                label_counts: torch.LongTensor = None, # batch_size, set_size
                num_labelers: torch.LongTensor = None, # batch_size
                metadata = None,
                **kwargs):

        batch_size, set_size = logits.size()

        if self._use_null_calibrator:
            null_logits = logits.data.new().resize_(batch_size, 1).zero_()
            null_mask = torch.ones_like(mask).resize_(batch_size, 1) if mask is not None else None

            full_logits = torch.cat([null_logits, logits], -1)
            full_mask = torch.cat([null_mask, mask], -1) if mask is not None else None

            if mask is not None:
                full_log_probs = util.masked_log_softmax(full_logits, full_mask)
            else:
                full_log_probs = F.log_softmax(full_logits, dim = 1)

            full_probs = full_log_probs.exp()
            probs = full_probs[:,1:]
            null_prob = full_probs[:,0]

            output_dict = {
                "logits": logits,
                "mask": mask,
                "probs": probs,
                "null_prob": null_prob
            }

            if label_counts is not None:
                null_counts = num_labelers.float() / self._uncertainty_factor
                full_gold_counts = torch.cat([null_counts.unsqueeze(-1), label_counts], -1)
                full_gold_probs = F.normalize(full_gold_counts, p = 1, dim = 1)
                cross_entropy = torch.sum(-full_gold_probs * full_log_probs, 1)
                output_dict["loss"] = torch.mean(cross_entropy)
                if not (self.training and self._skip_metrics_during_training):
                    labels = label_counts > 0.0
                    self._prob_metric(probs, labels, mask)
                    self._score_metric(logits, labels, mask)
                    # self._zero_score_metric(logits, labels, mask)
                    gold_items_per_labeler = label_counts.sum(dim = 1) / num_labelers
                    self._gold_recall_metric(gold_items_per_labeler)
                    gold_entropy = torch.distributions.Categorical(probs = full_gold_probs).entropy()
                    kl_divergence = (cross_entropy - gold_entropy)
                    self._kl_divergence_metric(kl_divergence)
                    self._null_prob_metric(null_prob)
            return output_dict

        else:
            raise NotImplementedError


    def get_metrics(self, reset: bool = False):
        prob_metrics = self._prob_metric.get_metric(reset = reset)
        score_metrics = self._score_metric.get_metric(reset = reset)
        # zero_score_metrics = self._zero_score_metric.get_metric(reset = reset)
        kl_divergence_metrics = self._kl_divergence_metric.get_metric(reset = reset)
        null_prob_metrics = self._null_prob_metric.get_metric(reset = reset)
        gold_recall_metrics = self._gold_recall_metric.get_metric(reset = reset)
        return {
            **{ ("p-%s" % k): v for k, v in prob_metrics.items() },
            **{ ("s-%s" % k): v for k, v in score_metrics.items() },
            # **{ ("0-%s" % k): v for k, v in zero_score_metrics.items() },
            "gold-items-avg": gold_recall_metrics["mean"],
            "null-prob-avg": null_prob_metrics["mean"],
            "null-prob-stdev": null_prob_metrics["stdev"],
            "KL": kl_divergence_metrics["mean"]
        }
