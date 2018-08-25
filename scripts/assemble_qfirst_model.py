import torch, os, json, tarfile, argparse, uuid, shutil

def main(qg_dir, qa_dir, config_base, outfile, target_device):
    def load(targ):
        if target_device is not None:
            return torch.load(targ, map_location = target_device)
        else:
            return torch.load(targ)

    qg_weights = load(os.path.join(qg_dir, "best.th"))
    qg_config = json.loads(open(os.path.join(qg_dir, "config.json"), 'r').read())
    # TODO remove when old model is retrained
    question_model_config_if_old = qg_config["model"].pop("question_generator", None)
    if question_model_config_if_old is not None:
        qg_config["model"]["question_model"] = question_model_config_if_old

    weights = {}
    for name, w in qg_weights.items():
        # TODO remove when old model is retrained
        if name.startswith('question_generator'):
            name = name.replace('question_generator', 'question_model')
        weights['question_generator.' + name] = w

    qa_weights = load(os.path.join(qa_dir, "best.th"))
    qa_config = json.loads(open(os.path.join(qa_dir, "config.json"), 'r').read())

    for name, w in qa_weights.items():
        weights['question_answerer.' + name] = w

    config = json.loads(open(config_base).read())
    config["model"]["question_generator"] = qg_config["model"]
    config["model"]["question_answerer"] = qa_config["model"]

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
    parser.add_argument('--qg', type=str)
    parser.add_argument('--qa', type=str)
    parser.add_argument('--config_base', type=str)
    parser.add_argument('--out', type=str)
    parser.add_argument('--target_device', type=str)

    args = parser.parse_args()
    main(args.qg, args.qa, args.config_base, args.out, args.target_device)

