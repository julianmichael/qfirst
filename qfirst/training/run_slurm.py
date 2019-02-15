#!/usr/bin/env python3

#SBATCH --job-name=qasrl-devel
#SBATCH --output=checkpoint/%u/jobs/sample-%j.out
#SBATCH --error=checkpoint/%u/jobs/sample-%j.err
#SBATCH --partition=dev
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=2
#SBATCH --signal=B:USR1@120
#SBATCH --open-mode=append
#SBATCH --time=3:00

import importlib
from allennlp.common.util import import_submodules
importlib.invalidate_caches()
import sys
sys.path.append(".")
import_submodules("qfirst")

import signal
import sys
import os
import time
import argparse
import socket
import shutil

import torch
from allennlp.common import Params
from allennlp.commands.train import train_model

def sig_handler(signum, frame):
    print("Caught signal", signum)
    print(socket.gethostname(), "USR1 signal caught.")
    print("requeueing job " + os.environ["SLURM_JOB_ID"])
    os.system("scontrol requeue " + os.environ["SLURM_JOB_ID"])
    raise KeyboardInterrupt # trigger AllenNLP's built-in interrupt handler
    # sys.exit(-1)

def term_handler(signum, frame):
    print("bypassing sigterm", flush = True)

def main():
    signal.signal(signal.SIGUSR1, sig_handler)
    signal.signal(signal.SIGTERM, term_handler)
    print("signal handlers installed", flush = True)

    parser = argparse.ArgumentParser(description = "Train QA-SRL model variants.")
    parser.add_argument("models_root", metavar = "path", type = str, help = "Path to root of model variants")
    parser.add_argument("models_branch", metavar = "path", type = str, help = "Path to config file")
    parser.add_argument("initial_batch_size", metavar = "n", type = int, help = "Batch size to start with before cutting as necessary")
    parser.add_argument("serialization_dir", metavar = "path", type = str, help = "Override to default serialization dir for testing", default = None)
    args = parser.parse_args()

    if args.serialization_dir is None:
        serialization_directory = "/checkpoint/jmichael/" + args.models_branch
    else:
        serialization_directory = args.serialization_dir

    current_batch_size = args.initial_batch_size

    done = False
    while not done:
        try:
            if not os.path.exists(serialization_directory):
                print("Starting new training round")
                os.makedirs(serialization_directory)
                config_path = args.models_root + "/" + args.models_branch
                params = Params.from_file(config_path, "")
                params["iterator"]["biggest_batch_first"] = True
                params["iterator"]["batch_size"] = current_batch_size
                train_model(params,
                            serialization_directory,
                            file_friendly_logging = True)
                done = True
            else:
                print("Recovering from a previously preempted run")
                config_path = serialization_directory + "/config.json"
                params = Params.from_file(config_path, "")
                current_batch_size = params["iterator"]["batch_size"]
                train_model(params,
                            serialization_directory,
                            file_friendly_logging = True,
                            recover = True)
                done = True
        except RuntimeError as e:
            if 'out of memory' in str(e):
                current_batch_size = current_batch_size / 2
                print('Ran out of memory. Reducing batch size to %s and retrying' % current_batch_size)
                shutil.rmtree(serialization_directory)
                torch.cuda.empty_cache()
                # for p in self.model.parameters():
                #     if p.grad is not None:
                #         del p.grad  # free some memory
                # return self.valid_step(sample, raise_oom=True)
            else:
                raise e

if __name__ == "__main__":
    main()
