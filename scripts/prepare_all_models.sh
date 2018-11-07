#!/bin/bash

#        --target_device cpu

python scripts/assemble_qfirst_model.py \
       --qg save/qg/clause-cond-lm-1 \
       --qa save/qa/binary-union-1 \
       --config_base models/qfirst-clause-parser-base.json \
       --out models/qfirst-clause-model-2.tar.gz \
       --target_device cpu

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/clause-cond-lm-1 \
#        --qa save/qa/clause-binary-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-clause-model-1.tar.gz \

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-bilstm-1 \
#        --qa save/qa/binary-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-1.tar.gz \

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-2-bilstm \
#        --qa save/qa/multinomial-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-2.tar.gz \

# python scripts/prepare_afirst_model.py \
#        --model_dir data/qasrl_parser_elmo \
#        --out models/afirst-model.tar.gz \
