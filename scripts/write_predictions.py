from typing import Optional
import torch, os, json, tarfile, argparse, uuid, shutil, sys

def main(model_path: str,
         max_beam_size: int,
         question_minimum_prob: float,
         span_minimum_prob: float,
         out_path: str,
         cuda_device: int):
    if model_path is None or span_minimum_prob is None or out_path is None or cuda_device is None:
        print("Arguments (model, span_threshold, out, device) must be present.")

    # eval_data_file = "http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz"
    eval_data_file = "dev-mini.jsonl"
    dataset_reader = '{ "min_answers": 0, "min_valid_answers": 0 }'
    metric = '{"type": "prediction_saving", "span_minimum_prob": %.4f, "file_path": "%s" }' % (span_minimum_prob, out_path)

    if max_beam_size is not None and question_minimum_prob is not None:
        model = '{"max_beam_size": %d, "min_beam_probability": %.3f, "metric": %s}' % (max_beam_size, question_minimum_prob, metric)
    elif max_beam_size is not None:
        model = '{"max_beam_size": %d, "metric": %s}' % (max_beam_size, metric)
    elif question_minimum_prob is not None:
        model = '{"min_beam_probability": %.3f, "metric": %s}' % (question_minimum_prob, metric)
    else:
        model = '{"metric": %s}' % metric

    overrides = '{ "dataset_reader": %s, "model": %s }' % (dataset_reader, model)

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
    parser.add_argument('--beam', type=float)
    parser.add_argument('--question_threshold', type=float)
    parser.add_argument('--span_threshold', type=float)
    parser.add_argument('--out', type=str)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.beam, args.question_threshold, args.span_threshold, args.out, args.device)
