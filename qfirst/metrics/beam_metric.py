from allennlp.common import Params, Registrable
from allennlp.training.metrics.metric import Metric

class BeamMetric(Metric, Registrable):
    def __init__(self):
        super(BeamMetric, self).__init__()

    def __call__(self,
                 gold_qa_pairs,
                 full_beam,
                 metadata):
        raise NotImplementedError()

    @classmethod
    def from_params(cls, params) -> 'BeamMetric':
        choice = params.pop_choice('type', cls.list_available())
        return cls.by_name(choice).from_params(params)
