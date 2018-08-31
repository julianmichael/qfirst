#!/bin/bash

# qfirst model, tuning (qa-level e2e)
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"question_source": "turk", "min_answers": 6, "min_valid_answers": 5}}'
# qfirst model, tuning (dense): all three accuracies.
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"min_answers": 6}, "model": {"metric": {"target": "span-acc-lb", "use_dense_metric": true} } }'
# qfirst model, dense. TODO final tuned thresholds
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"min_answers": 6}, "model": {"metric": {"type": "beam_filtering", "filter": {"question_threshold": 0.1, "span_threshold": 0.5, "invalid_threshold": 0.25}, "use_dense_metric": true} } }'
# qfirst model, eval (non-dense). TODO final tuned thresholds
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "metric": {"type": "beam_filtering", "filter": {"question_threshold": 0.1, "span_threshold": 0.5, "invalid_threshold": 0.25}}'

# qfirst model, tuning (non-dense)
#python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"} }'

# afirst model, tuning on dense
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": { "min_answers": 6 }, "model": {"metric": {"target": "span-acc-lb", "use_dense_metric": true, "recall_constraint": 2.0 } } }'

# afirst model, tuning on turk-question eval (non-dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"} }'
# afirst model, running on turk-question eval (non-dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"type": "threshold", "span_threshold": 0.5} }'

# afirst model, tuning on turk-question eval (dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"question_sources": "turk", "min_answers": 6, "min_valid_answers": 5} }'

# afirst model, running eval with tuned threshold (dense).
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 6}, "model": {"metric": {"type": "threshold", "span_threshold": 0.7, "use_dense_metric": true} } }'
