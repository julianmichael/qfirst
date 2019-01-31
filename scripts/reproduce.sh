#!/bin/bash

$SPAN_DETECTOR_DIR=save/afirst/span_detector
$QUESTION_GENERATOR_DIR=save/afirst/question_generator
$AFIRST_PARSER_DIR=save/afirst
$CUDA_DEVICE=-1

# train span detector
python -m allennlp.run train configs/afirst/span_detection.json --include-package qfirst -s $SPAN_DETECTOR_DIR
# train question generator
python -m allennlp.run train configs/afirst/question_generation.json --include-package qfirst -s $QUESTION_GENERATOR_DIR
# assemble into full model
mkdir -p $AFIRST_PARSER_DIR
python scripts/assemble_afirst_model.py \
       --config_base configs/afirst/parser_base.json \
       --span_detector $SPAN_DETECTOR_DIR \
       --question_generator $QUESTION_GENERATOR_DIR \
       --out $AFIRST_PARSER_DIR/model.tar.gz

# run predictions
mkdir -p $AFIRST_PARSER_DIR/predictions
allennlp predict \
         --predictor afirst \
         --cuda-device $CUDA_DEVICE \
         --include-package qfirst \
         --output-file $AFIRST_PARSER_DIR/predictions/predictions-dense.jsonl \
         $AFIRST_PARSER_DIR/model.tar.gz \
         qasrl-v2_1/dense/dev.jsonl
