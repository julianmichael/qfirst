#!/bin/bash

set -e
python qfirst/scripts/convert_propbank_to_qasrl_input.py --in=./data/development/dev.gold_conll --out=data/dev.propbank.qasrl.jsonl
python qfirst/scripts/convert_propbank_to_qasrl_input.py --in=./data/train/train.gold_conll --out=data/train.propbank.qasrl.jsonl
python qfirst/scripts/convert_propbank_to_qasrl_input.py --in=./data/test/test.gold_conll --out=data/test.propbank.qasrl.jsonl
