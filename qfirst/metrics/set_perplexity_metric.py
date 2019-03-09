# from typing import Dict, List, Optional, Set, Tuple

# import torch

# from allennlp.common import Registrable
# from allennlp.training.metrics.metric import Metric

# import math

# from qfirst.common.span import Span

# class SetPerplexityMetric(Metric, Registrable):
#     def __init__(self):
#         self.reset()

#     def reset(self):
#         # def make_confs(thresholds):
#         #     return [{
#         #         "threshold": t,
#         #         "instances": 0,
#         #         "tp": 0,
#         #         "tn": 0,
#         #         "fp": 0,
#         #         "fn": 0
#         #     } for t in thresholds]
#         # self._confs = make_confs(self._thresholds)

#     def __call__(self,
#                  cross_entropy,
#                  gold_entropy
#                  probs,
#                  gold_probs,
#                  mask = None):
#         probs, gold_probs = Metric.unwrap_to_tensors(probs, gold_probs)
#         if mask is not None:
#             mask, = Metric.unwrap_to_tensors(mask)

#         # for conf in self._confs:
#         #     conf["instances"] += scores.size(0)
#         # if mask is None:
#         #     num_true = labels.sum().item()
#         #     for conf in self._confs:
#         #         preds = (scores >= conf["threshold"]).long()
#         #         num_tp = torch.min(preds, labels).sum().item()
#         #         conf["tp"] += num_tp
#         #         conf["fn"] += num_true - num_tp
#         #         conf["fp"] += preds.sum().item() - num_tp
#         #         conf["tn"] += (1 - torch.max(preds, labels)).sum().item()
#         # else:
#         #     num_true = (labels * mask).sum().item()
#         #     for conf in self._confs:
#         #         preds = (scores >= conf["threshold"]).long()
#         #         num_tp = (torch.min(preds, labels) * mask).sum().item()
#         #         conf["tp"] += num_tp
#         #         conf["fn"] += num_true - num_tp
#         #         conf["fp"] += (preds * mask).sum().item() - num_tp
#         #         conf["tn"] += ((1 - torch.max(preds, labels)) * mask).sum().item()

#     def get_metric(self, reset = False):
#         # def stats(conf):
#         #     tp = conf["tp"]
#         #     fp = conf["fp"]
#         #     tn = conf["tn"]
#         #     fn = conf["fn"]
#         #     num_predicted = tp + fp
#         #     precision = 0.
#         #     if num_predicted > 0.0:
#         #         precision = tp / num_predicted
#         #     num_gold = tp + fn
#         #     recall = 0.
#         #     if num_gold > 0.0:
#         #         recall = tp / num_gold
#         #     f1 = 0.
#         #     if precision + recall > 0.0:
#         #         f1 = 2 * (precision * recall) / (precision + recall)
#         #     mccNum = (tp * tn) - (fp * fn)
#         #     mccDenom = math.sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
#         #     mcc = 0.
#         #     if abs(mccDenom) > 0.0:
#         #         mcc = mccNum / mccDenom
#         #     return {
#         #         "threshold": conf["threshold"],
#         #         "avg-predicted": num_predicted / conf["instances"],
#         #         "avg-gold": num_gold / conf["instances"],
#         #         "precision": precision,
#         #         "recall": recall,
#         #         "f1": f1,
#         #         "mcc": mcc
#         #     }
#         # stats_dict = max([stats(conf) for conf in self._confs], key = lambda d: d["f1"])
#         stats_dict = {}

#         if reset:
#             self.reset()

#         return stats_dict
