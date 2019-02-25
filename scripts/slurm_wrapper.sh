#!/bin/bash
PATH=$PATH source /public/apps/anaconda3/5.0.1/bin/activate /private/home/jmichael/qfirst/env
export CUDA_VISIBLE_DEVICES=$SLURM_LOCALID
MODEL_VARIANT=$(($INDEX_OFFSET + $SLURM_LOCALID))
THIS_MODEL_BRANCH=$MODELS_BRANCH/$MODEL_VARIANT.json
echo $SLURMD_NODENAME $SLURM_JOB_ID $SLURM_LOCALID $THIS_MODEL_BRANCH $INIT_BATCH_SIZE
/private/home/jmichael/qfirst/env/bin/python /private/home/jmichael/qfirst/qfirst/training/run_slurm.py \
  $MODELS_ROOT $THIS_MODEL_BRANCH $INIT_BATCH_SIZE
