from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.data.vocabulary import Vocabulary
from allennlp.training.metrics.metric import Metric

import math

from qfirst.data.util import get_slot_label_namespace

class QuestionMetric(Metric):
    def __init__(self,
            vocabulary: Vocabulary,
            slot_names: List[str]):
        self._vocabulary = vocabulary
        self._slot_names = slot_names

        self.reset()

    def reset(self):
        self._total_questions = 0
        self._questions_correct = 0
        self._slot_correct = { l: 0 for l in self._slot_names }
        self._negative_log_likelihood = 0.

    def __call__(self,
                 slot_logits: Dict[str, torch.Tensor],
                 slot_labels: Dict[str, torch.Tensor],
                 mask: torch.Tensor,
                 negative_log_likelihood: float):
        mask, negative_log_likelihood = self.unwrap_to_tensors(mask.long(), negative_log_likelihood)
        batch_size, num_questions = mask.size()
        # slot_logits[slot_name] Shape: batch_size, num_questions, slot name vocab size
        # slot_labels[slot_name] Shape: batch_size, num_questions, 1?

        num_total_questions = mask.sum().item()

        self._total_questions += num_total_questions
        self._negative_log_likelihood += negative_log_likelihood

        # we'll mask out questions as we miss slots to get the full question accuracy
        correct_questions = mask.clone()

        for slot_name in self._slot_names:
            # logits Shape: batch_size, slot_name_vocab_size
            # gold_labels Shape: batch_size, 1?
            logits, gold_labels = self.unwrap_to_tensors(slot_logits[slot_name], slot_labels[slot_name])
            # Shape: batch_size, question_length, 1?
            argmax_predictions = logits.argmax(-1)
            for bi in range(batch_size):
                for qi in range(num_questions):
                    if mask[bi, qi].item() > 0:
                        if argmax_predictions[bi, qi] == gold_labels[bi, qi]:
                            self._slot_correct[slot_name] += 1
                        else:
                            correct_questions[bi, qi] = 0

        self._questions_correct = correct_questions.sum().item()

    def get_metric(self, reset=False):

        def get_slot_accuracy(slot_name):
            return self._slot_correct[slot_name] /  self._total_questions
        slot_wise_metrics = { "%s-accuracy" % l : get_slot_accuracy(l) for l in self._slot_names }

        avg_slot_accuracy = sum([v for k, v in slot_wise_metrics.items()]) / len(self._slot_names)
        full_question_accuracy = self._questions_correct / self._total_questions
        perplexity_per_question = math.exp(self._negative_log_likelihood / self._total_questions)

        other_metrics = {
            "avg-slot-accuracy": avg_slot_accuracy,
            "full-question-accuracy": full_question_accuracy,
            "perplexity-per-question": perplexity_per_question
        }

        if reset:
            self.reset()
        return {**slot_wise_metrics, **other_metrics}

