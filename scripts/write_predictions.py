from typing import Optional
import torch, os, json, tarfile, argparse, uuid, shutil, sys

def main(model_path: str,
         span_minimum_prob: float,
         out_path: str,
         cuda_device: int):
    if model_path is None or span_minimum_prob is None or out_path is None or cuda_device is None:
        print("All arguments (model, threshold, out, device) must be present.")

    # eval_data_file = "http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz"
    eval_data_file = "dev-mini.jsonl"
    dataset_reader = '{ "min_answers": 3, "min_valid_answers": 0 }'
    metric = '{"type": "prediction_saving", "span_minimum_prob": %.4f, "file_path": "%s" }' % (span_minimum_prob, out_path)

    overrides = '{ "dataset_reader": %s, "model": {"metric": %s } }' % (dataset_reader, metric)

    from subprocess import run
    run(["python", "-m", "allennlp.run", "evaluate",
         model_path,
         "--evaluation-data-file", eval_data_file,
         "--cuda-device", str(cuda_device),
         "--include-package", "qfirst",
         "--overrides", overrides])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Output a model's full (unfiltered, scored) predictions to a file.")
    parser.add_argument('--model', type=str)
    parser.add_argument('--threshold', type=float)
    parser.add_argument('--out', type=str)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.threshold, args.out, args.device)
