#!/bin/bash

# qfirst model, tuning (dense)
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"question_source": "turk", "min_answers": 6, "min_valid_answers": 5}}'
# qfirst model, eval (dense). TODO replace with final tuned thresholds
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"question_source": "turk", "min_answers": 6, "min_valid_answers": 5}, "model": {"metric": {"type": "beam_filtering", "filter": {"question_threshold": 0.1, "span_threshold": 0.5, "invalid_threshold": 0.25}}}}'
# qfirst model, tuning (non-dense)
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"} }'
# qfirst model, eval (non-dense). TODO fill in with tuned thresholds
python -m allennlp.run evaluate models/qfirst-model-1.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"min_answers": 6, "min_valid_answers": 5}, "metric": {"type": "beam_filtering", "filter": {"question_threshold": TODO, "span_threshold": TODO, "invalid_threshold": TODO}}'

# afirst model, tuning on turk-question eval (non-dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"} }'
# afirst model, running on turk-question eval (non-dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"type": "threshold", "span_threshold": 0.5} }'

# afirst model, tuning on turk-question eval (dense)
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"question_sources": "turk", "min_answers": 6, "min_valid_answers": 5} }'

# afirst model, running eval with tuned threshold (dense).
python -m allennlp.run evaluate models/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device 0 --include-package qfirst --overrides '{ "dataset_reader": {"question_sources": "turk", "min_answers": 6, "min_valid_answers": 5}, "metric": {"type": "threshold", "span_threshold": 0.7} }'
