#!/bin/bash

# python scripts/combine_models.py \
#        --qg models/qg/cond-lm-1-passthrough \
#        --qa models/qa/simple-1-weighted \
#        --config_base models/full/qfirst-parser-base.json \
#        --out models/full/qfirst-model-1.tar.gz \
#        --target_device cpu

python scripts/combine_models.py \
       --qg models/qg/cond-lm-2-bilstm \
       --qa models/qa/simple-1-weighted \
       --config_base models/full/qfirst-parser-base.json \
       --out models/full/qfirst-model-2.tar.gz \
       --target_device cpu
