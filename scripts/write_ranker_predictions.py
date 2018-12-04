from typing import Optional
import torch, os, json, tarfile, argparse, uuid, shutil, sys

def main(model_path: str,
         ref_path: str,
         out_path: str,
         cuda_device: int):
    if model_path is None or ref_path is None or out_path is None or cuda_device is None:
        sys.exit("Arguments (model, ref, out, device) must be present.")

    overrides = {
        "dataset_reader": {
            "include_metadata": True
        },
        "model": {
            "prediction_out_path": out_path
        }
    }
    from subprocess import run
    run(["python", "-m", "allennlp.run", "evaluate",
         model_path, ref_path,
         "--cuda-device", str(cuda_device),
         "--include-package", "qfirst",
         "--overrides", json.dumps(overrides)])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Output a model's full (unfiltered, scored) predictions to a file.")
    parser.add_argument('--model', type=str)
    parser.add_argument('--ref', type=str)
    parser.add_argument('--out', type=str)
    parser.add_argument('--device', type=int)
    args = parser.parse_args()
    main(args.model, args.ref, args.out, args.device)
