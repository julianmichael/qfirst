#!/bin/bash

#SBATCH --job-name=qasrl-tuning
#SBATCH --output=/checkpoint/%u/jobs/%j.out
#SBATCH --error=/checkpoint/%u/jobs/%j.err
#SBATCH --partition=uninterrupted
#SBATCH --cpus-per-task=2
#SBATCH --signal=USR1@180
#SBATCH --open-mode=append
#SBATCH --time=2:00:00
###SBATCH --comment="ACL deadline Mar 4"

# training
module load anaconda3
srun --label /private/home/jmichael/qfirst/scripts/slurm_wrapper.sh