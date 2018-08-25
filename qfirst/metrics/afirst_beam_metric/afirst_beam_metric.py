from allennlp.common import Params, Registrable
from allennlp.training.metrics.metric import Metric

class AfirstBeamMetric(Metric, Registrable):
    @classmethod
    def from_params(cls, params: Params) -> 'AfirstBeamMetric':
        choice = params.pop_choice('type', cls.list_available())
        return cls.by_name(choice).from_params(params)
