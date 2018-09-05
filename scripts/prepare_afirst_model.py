import torch, os, json, tarfile, argparse, uuid, shutil

def main(model_dir, outfile, target_device):
    def load(targ):
        if target_device is not None:
            return torch.load(targ, map_location = target_device)
        else:
            return torch.load(targ)

    weights = load(os.path.join(model_dir, "weights.th"))
    config = json.loads(open(os.path.join(model_dir, "config.json"), 'r').read())

    config["model"]["type"] = "afirst_parser"
    config["model"]["metric"] = {
        "type": "threshold_tuning",
        "target": "q-e2e-f1",
        "span_thresholds": [0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.96, 0.97, 0.98, 0.985, 0.99]
    }

    tmpdir = os.path.join("tmp", str(uuid.uuid4()))
    os.makedirs(tmpdir)

    weightfile = os.path.join(tmpdir, "weights.th")
    configfile = os.path.join(tmpdir, "config.json")
    vocabdir = os.path.join(tmpdir, "vocabulary")

    torch.save(weights, weightfile)
    with open(configfile, "w") as f:
        f.write(json.dumps(config))
    shutil.copytree(os.path.join(model_dir, "vocabulary"), vocabdir)

    with tarfile.open(outfile, "w:gz") as tar:
        tar.add(weightfile, arcname="weights.th")
        tar.add(configfile, arcname="config.json")
        tar.add(vocabdir, arcname="vocabulary")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Prepare the pretrained answer-first baseline model for comparable evaluation.")
    parser.add_argument('--model_dir', type=str)
    parser.add_argument('--out', type=str)
    parser.add_argument('--target_device', type=str)

    args = parser.parse_args()
    main(args.model_dir, args.out, args.target_device)
 
