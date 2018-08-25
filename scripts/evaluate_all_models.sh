#!/bin/bash

# qfirst model, turk-question eval (like intermediate dense dev from paper)
# here recall is most important
# TODO tuning run and run with results?
python -m allennlp.run evaluate models/full/qfirst-model-2.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{"dataset_reader": {"question_source": "turk", "min_answers": 6, "min_valid_answers": 5}}'

# afirst model, tuning on turk-question eval (non-dense)
python -m allennlp.run evaluate models/full/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"} }'

# afirst model, running on turk-question eval (non-dense)
python -m allennlp.run evaluate models/full/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": {"min_answers": 3, "min_valid_answers": 3}, "metric": {"target": "e2e-f1"}, "metric": {"type": "threshold", "span_threshold": 0.5} }'

# afirst model, tuning on turk-question eval (dense)
python -m allennlp.run evaluate models/full/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": {"question_sources": "turk", "min_answers": 6, "min_valid_answers": 5} }'

# afirst model, running eval with tuned threshold (dense). # TODO replace with final tuned threshold
python -m allennlp.run evaluate models/full/afirst-model.tar.gz --evaluation-data-file http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz --cuda-device -1 --include-package qfirst --overrides '{ "dataset_reader": {"question_sources": "turk", "min_answers": 6, "min_valid_answers": 5}, "metric": {"type": "threshold", "span_threshold": 0.5} }'
