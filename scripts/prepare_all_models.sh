#!/bin/bash

#        --target_device cpu

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-2-bilstm-fixed \
#        --qa save/qa/binary-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-1-fixed.tar.gz \

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-bilstm-tqa \
#        --qa save/qa/binary-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-1-tqa.tar.gz \

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-bilstm-wikipedia \
#        --qa save/qa/binary-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-1-wikipedia.tar.gz \

python scripts/assemble_qfirst_model.py \
       --qg save/qg/cond-lm-bilstm-wikinews \
       --qa save/qa/binary-union-1 \
       --config_base models/qfirst-parser-base.json \
       --out models/qfirst-model-1-wikinews.tar.gz \

# python scripts/assemble_qfirst_model.py \
#        --qg save/qg/cond-lm-2-bilstm \
#        --qa save/qa/multinomial-union-1 \
#        --config_base models/qfirst-parser-base.json \
#        --out models/qfirst-model-2.tar.gz \

# python scripts/prepare_afirst_model.py \
#        --model_dir data/qasrl_parser_elmo \
#        --out models/afirst-model.tar.gz \
