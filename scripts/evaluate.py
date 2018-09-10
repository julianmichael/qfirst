from typing import Optional
import torch, os, json, tarfile, argparse, uuid, shutil, sys

def main(model_path: str,
         threshold: Optional[str], # if absent, do tuning
         dense_eval: bool,
         out_path: str,
         cuda_device: int):
    if dense_eval and out_path is None:
        print("Must specify results out path with --out for dense eval tuning.")
        sys.exit()
    if dense_eval and threshold is not None:
        print("Cannot specify thresholds for dense eval, which requires tuning.")
        sys.exit()

    if dense_eval:
        eval_data_file = "http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz"
        dataset_reader = '{ "min_answers": 6, "min_valid_answers": 0 }'
        metric = '{"target": "question-answer-acc-lb", "use_dense_metric": true, "recall_pegs": [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0], "save_filepath": %s }' % out_path
    else:
        eval_data_file = "http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz"
        dataset_reader = '{ "min_answers": 3, "min_valid_answers": 3 }'
        if threshold_params is not None:
            if "question_threshold" in threshold_params:
                metric = '{"type": "beam_filtering", "filter": %s, "use_dense_metric": false}' % threshold
            else:
                metric = '{"type": "threshold", "span_threshold": %s, "use_dense_metric": false}' % threshold
        else:
            metric = '{"target": "q-e2e-f1", "use_dense_metric": false, "recall_pegs": [0.0] }'

    overrides = '{ "dataset_reader": %s, "model": {"metric": %s } }' % (dataset_reader, metric)

    from subprocess import run
    run(["python", "-m", "allennlp.run", "evaluate",
         model_path,
         "--evaluation-data-file", eval_data_file,
         "--cuda-device", str(cuda_device),
         "--include-package", "qfirst",
         "--overrides", overrides])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Evaluate a model.")
    parser.add_argument('--model', type=str)
    parser.add_argument('--threshold', type=str)
    parser.add_argument('--dense', type=bool)
    parser.add_argument('--out', type=str)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.threshold, args.dense, args.out, args.device)

