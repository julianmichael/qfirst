import torch, os, json, tarfile, argparse, uuid, shutil

def main(config_base, qg_dir, qa_dir, outfile):
    def load(targ):
        return torch.load(targ, map_location = "cpu")

    config = json.loads(open(config_base).read())
    weights = {}

    qg_weights = load(os.path.join(qg_dir, "best.th"))
    qg_config = json.loads(open(os.path.join(qg_dir, "config.json"), 'r').read())
    for name, w in qg_weights.items():
        weights['_question_generator.' + name] = w
    config["model"]["question_generator"] = qg_config["model"]
    del config["model"]["question_generator"]["type"]

    qa_weights = load(os.path.join(qa_dir, "best.th"))
    qa_config = json.loads(open(os.path.join(qa_dir, "config.json"), 'r').read())
    for name, w in qa_weights.items():
        weights['_question_answerer.' + name] = w
    config["model"]["question_answerer"] = qa_config["model"]
    del config["model"]["question_answerer"]["type"]

    tmpdir = os.path.join("tmp", str(uuid.uuid4()))
    os.makedirs(tmpdir)

    weightfile = os.path.join(tmpdir, "weights.th")
    vocabdir = os.path.join(tmpdir, "vocabulary")
    configfile = os.path.join(tmpdir, "config.json")

    torch.save(weights, weightfile)
    with open(configfile, "w") as f:
        f.write(json.dumps(config))
    shutil.copytree(os.path.join(qa_dir, "vocabulary"), vocabdir)

    with tarfile.open(outfile, "w:gz") as tar:
        tar.add(weightfile, arcname="weights.th")
        tar.add(configfile, arcname="config.json")
        tar.add(vocabdir, arcname="vocabulary")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Combine trained question-generation and answering models"
                                                 "into a question-first QA-SRL parser.")
    parser.add_argument('--config_base', type=str)
    parser.add_argument('--question_generator', type=str)
    parser.add_argument('--question_answerer', type=str)
    parser.add_argument('--out', type=str)

    args = parser.parse_args()
    main(args.config_base, args.question_generator, args.question_answerer, args.out)

