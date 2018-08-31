#!/bin/bash

# python scripts/assemble_qfirst_model.py \
#        --qg models/qg/cond-lm-1-passthrough \
#        --qa models/qa/simple-1-weighted \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-1.tar.gz \
#        --target_device cpu

python scripts/assemble_qfirst_model.py \
       --qg save/qg/cond-lm-2-bilstm \
       --qa save/qa/binary-1-union \
       --config_base models/qfirst-parser-base.json \
       --out models/qfirst-model-1.tar.gz \
       --target_device cpu

# python scripts/prepare_afirst_model.py \
#        --model_dir data/qasrl_parser_elmo \
#        --out models/afirst-model.tar.gz \
#        --target_device cpu
