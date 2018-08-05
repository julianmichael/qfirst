from typing import Dict, List, Optional, Set, Tuple

import torch

from allennlp.nn.util import get_lengths_from_binary_sequence_mask
from allennlp.data.vocabulary import Vocabulary
from allennlp.training.metrics.metric import Metric

import math

from qfirst.data.util import get_slot_label_namespace

class QuestionSetMetric(Metric):
    def __init__(self,
            vocabulary: Vocabulary,
            slot_names: List[str]):
        self._vocabulary = vocabulary
        self._slot_names = slot_names

        self.reset()

    def reset(self):
        self._total_verbs = 0
        self._total_questions = 0
        self._questions_correct = 0
        self._slot_correct = { l: 0 for l in self._slot_names }
        self._negative_log_likelihood = 0.

    def __call__(self,
                 slot_logits: Dict[str, torch.Tensor],
                 slot_labels: Dict[str, torch.Tensor],
                 num_questions_mask: torch.LongTensor,
                 negative_log_likelihood: float):
        # slot_logits[slot_name] Shape: batch_size, num_questions, slot_name_vocab_size
        # slot_labels[slot_name] Shape: batch_size, num_questions, 1?

        self._total_verbs += num_questions_mask.size(0)
        self._total_questions += num_questions_mask.sum().item()
        self._negative_log_likelihood += negative_log_likelihood

        # we'll mask out questions as we miss slots to get the full question accuracy
        num_questions_correct_mask = num_questions_mask.clone()

        for slot_name in self._slot_names:
            # logits Shape: batch_size, num_questions, slot_name_vocab_size
            # gold_labels Shape: batch_size, num_questions, 1?
            logits, gold_labels = self.unwrap_to_tensors(slot_logits[slot_name], slot_labels[slot_name])
            # Shape: batch_size, question_length, 1?
            argmax_predictions = logits.argmax(-1)
            for b in range(num_questions_mask.size(0)):
                for i in range(num_questions_mask.size(1)):
                    if num_questions_mask[b][i] == 1:
                        if argmax_predictions[b][i] == gold_labels[b][i]:
                            self._slot_correct[slot_name] += 1
                        else:
                            num_questions_correct_mask[b][i] = 0

        self._questions_correct = num_questions_correct_mask.sum().item()


    def get_metric(self, reset=False):

        def get_slot_accuracy(slot_name):
            return self._slot_correct[slot_name] /  self._total_questions
        slot_wise_metrics = { "%s-accuracy" % l : get_slot_accuracy(l) for l in self._slot_names }

        avg_slot_accuracy = sum([v for k, v in slot_wise_metrics.items()]) / len(self._slot_names)
        full_question_accuracy = self._questions_correct / self._total_questions
        perplexity_per_question = math.exp(self._negative_log_likelihood / self._total_verbs)

        other_metrics = {
            "avg-slot-accuracy": avg_slot_accuracy,
            "full-question-accuracy": full_question_accuracy,
            "negative-log-likelihood": self._negative_log_likelihood,
            "perplexity-per-question": perplexity_per_question
        }

        if reset:
            self.reset()
        return {**slot_wise_metrics, **other_metrics}
