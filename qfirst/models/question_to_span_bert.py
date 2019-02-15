from typing import Dict, List, TextIO, Optional, Union

from overrides import overrides

import torch
from torch.nn.modules import Dropout, Sequential, Linear, ReLU
import torch.nn.functional as F

from allennlp.common.checks import ConfigurationError
from allennlp.data import Vocabulary
from allennlp.modules import Seq2SeqEncoder, TimeDistributed, TextFieldEmbedder, Pruner
from allennlp.modules.token_embedders import Embedding
from allennlp.models.model import Model
from allennlp.nn import InitializerApplicator, RegularizerApplicator
from allennlp.nn.util import get_text_field_mask
from allennlp.nn.util import batched_index_select

from qfirst.common.span import Span

from qfirst.metrics.span_metric import SpanMetric

from qfirst.modules.slot_sequence_encoder import SlotSequenceEncoder
from qfirst.modules.span_selector import SpanSelector
from qfirst.modules.sentence_encoder import SentenceEncoder

@Model.register("qasrl_question_to_span_bert")
class QuestionToSpanModelBert(Model):
    def __init__(self, vocab: Vocabulary,
                 text_field_embedder: TextFieldEmbedder,
                 skip_metrics_during_training: bool = True,
                 initializer: InitializerApplicator = InitializerApplicator(),
                 regularizer: Optional[RegularizerApplicator] = None):
        super(QuestionToSpanModelBert, self).__init__(vocab, regularizer)
        self._text_field_embedder = text_field_embedder
        self._qa_outputs = TimeDistributed(Linear(self._text_field_embedder.get_output_dim(), 2))
        self._pruner = Pruner(lambda x: x)
        self._skip_metrics_during_training = skip_metrics_during_training
        self._metric = SpanMetric()

    def forward(self,  # type: ignore
                text: Dict[str, torch.LongTensor],
                start_index: torch.LongTensor = None,
                end_index: torch.LongTensor = None,
                metadata = None,
                **kwargs):

        # text should have question in it as well
        embedded_text = self._text_field_embedder(text)
        text_mask = get_text_field_mask(text)
        text_log_mask = text_mask.float().log()

        logit_pairs = self._qa_outputs(embedded_text)
        start_logits, end_logits = logit_pairs.split(1, dim = -1)
        start_logits = start_logits.squeeze(-1) + text_log_mask
        end_logits = end_logits.squeeze(-1) + text_log_mask

        output_dict = {
            "text_mask": text_mask,
            "start_logits": start_logits,
            "end_logits": end_logits
        }
        if start_index is not None and end_index is not None:
            # ignored_index = start_logits.size(1)
            # start_index.clamp_(0, ignored_index)
            # end_index.clamp_(0, ignored_index)
            start_loss = F.cross_entropy(start_logits, start_index.squeeze(-1))
            end_loss = F.cross_entropy(end_logits, end_index.squeeze(-1))
            loss = start_loss + end_loss
            output_dict["loss"] = loss

            # metrics; TODO only if not training?
            if not (self._raining and self._skip_metrics_during_training):
                output_dict = self.decode(output_dict)
                self._metric(output_dict["spans"], [m["gold_spans"] for m in metadata])

        return output_dict

    @overrides
    def decode(self, output_dict: Dict[str, torch.Tensor]) -> Dict[str, torch.Tensor]:
        start_probs = F.softmax(output_dict["start_logits"], dim = 1)
        end_probs = F.softmax(output_dict["end_logits"], dim = 1)

        batch_size, num_tokens = output_dict["start_logits"].size()
        num_pruned_indices = min(num_tokens, 5)
        (top_start_probs, top_start_mask,
            top_start_indices, _) = self._pruner(start_probs.unsqueeze(-1), output_dict["text_mask"].float(), num_pruned_indices)
        (top_end_probs, top_end_mask,
            top_end_indices, _) = self._pruner(end_probs.unsqueeze(-1), output_dict["text_mask"].float(), num_pruned_indices)

        scored_spans = []
        for bi in range(batch_size):
            # span_index_offset = metadata[bi]["span_index_offset"]
            item_scored_spans = []
            top_span_probs = torch.ger(top_start_probs[bi].squeeze(-1), top_end_probs[bi].squeeze(-1))
            for si in range(num_pruned_indices):
                start = top_start_indices[bi][si].item()
                for ei in range(num_pruned_indices):
                    end = top_end_indices[bi][ei].item()
                    if end >= start:
                        prob = top_span_probs[si, ei].item()
                        item_scored_spans.append((Span(start, end), prob))
            scored_spans.append(item_scored_spans)
        output_dict["spans"] = scored_spans
        return output_dict

    def get_metrics(self, reset: bool = False):
        return self._metric.get_metric(reset = reset)
