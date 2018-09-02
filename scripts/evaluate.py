import torch, os, json, tarfile, argparse, uuid, shutil

# bool: dense/not dense
# bool: qfirst/afirst
# path: model archive
# int:  cuda-device

def main(model_path: str,
         dense_eval: bool,
         cuda_device: int):

    if dense_eval:
        eval_data_file = "http://qasrl.org/data/qasrl-v2/dense/dev.jsonl.gz",
        overrides = '{ "dataset_reader": { "min_answers": 6 }, "model": {"metric": {"target": "span-acc-lb", "use_dense_metric": true, "recall_constraint": 2.0 } } }'
    else:
        eval_data_file = "http://qasrl.org/data/qasrl-v2/orig/dev.jsonl.gz",
        overrides = '{ "dataset_reader": { "min_answers": 3, "min_valid_answers": 3 }, "model": {"metric": {"target": "e2e-f1", "use_dense_metric": false, "recall_constraint": 0.0 } } }'

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
    parser.add_argument('--dense', type=bool)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.dense, args.device)

