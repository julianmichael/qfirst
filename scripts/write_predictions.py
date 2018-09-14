from typing import Optional
import torch, os, json, tarfile, argparse, uuid, shutil, sys

def main(model_path: str,
         max_beam_size: int,
         question_minimum_prob: float,
         span_minimum_prob: float,
         out_path: str,
         cuda_device: int):
    if model_path is None or out_path is None or cuda_device is None:
        sys.exit("Arguments (model, out, device) must be present.")

    # eval_data_file = "http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz"
    eval_data_file = "dev-mini.jsonl"
    overrides = {
        "dataset_reader": {
            "min_answers": 0,
            "min_valid_answers": 0
        },
        "model": {
            "metric": {
                "type": "prediction_saving",
                "file_path": out_path
            }
        }
    }
    if max_beam_size is not None:
        overrides["model"]["max_beam_size"] = max_beam_size
    if question_minimum_prob is not None:
        overrides["model"]["question_minimum_prob"] = question_minimum_prob
    if span_minimum_prob is not None:
        overrides["model"]["span_minimum_prob"] = span_minimum_prob

    from subprocess import run
    run(["python", "-m", "allennlp.run", "evaluate",
         model_path,
         "--evaluation-data-file", eval_data_file,
         "--cuda-device", str(cuda_device),
         "--include-package", "qfirst",
         "--overrides", json.dumps(overrides)])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Output a model's full (unfiltered, scored) predictions to a file.")
    parser.add_argument('--model', type=str)
    parser.add_argument('--beam', type=float)
    parser.add_argument('--question_threshold', type=float)
    parser.add_argument('--span_threshold', type=float)
    parser.add_argument('--out', type=str)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.beam, args.question_threshold, args.span_threshold, args.out, args.device)
