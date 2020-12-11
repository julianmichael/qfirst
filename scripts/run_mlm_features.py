import logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("run_mlm_features")

import torch

import spacy
nlp = spacy.load('en_core_web_sm')

lemma_overrides = {
    'it': ['it'],
    "they/them": ['they', 'them'],
    "oneself": ['himself', 'herself', 'itself', 'themselves', 'ourselves', 'itself', 'oneself', 'myself', 'yourself']
}
lemma_override_map = {
    token: lemma
    for lemma, tokens in lemma_overrides.items()
    for token in tokens
}

def lemmatize(token):
    return lemma_override_map.get(token) or nlp(token)[0].lemma_

# utility methods

import gzip
import codecs
import json
from tqdm import tqdm
import os

def read_lines(file_path):
    if file_path.endswith('.gz'):
        with gzip.open(file_path, 'r') as f:
            for line in f:
                yield line
    else:
        with codecs.open(file_path, 'r', encoding='utf8') as f:
            for line in f:
                yield line

# def indexOfFirstSatisfying(xs, p):
#     for i, x in enumerate(xs):
#          if p(x):
#              return i
#     return -1

def indexOfFirst(xs, y):
    for i, x in enumerate(xs):
         if x == y:
             return i
    return -1

def print_vector(vec, get_tokens, num_tokens = 15):
    top_probs, top_indices = torch.topk(vec, num_tokens)
    top_tokens = get_tokens(top_indices)
    print(" ".join(["%15s" % x for x in top_tokens]))
    print(" ".join(["%15.10f" % x for x in top_probs]))

class MLMFeatureBuilder:
    def __init__(self):
        pass

# TODO: figure out what to do about n't token.
# This isn't in BERT's vocab, but gets masked out on its own, creating massive confusion.
# maybe I can just not care and fiat it later. it's always gonna be ARGM-NEG or whatever, right?
# perhaps that's all the more reason not to normalize the final distributions later;
# sufficiently spread-out distributions aren't going to do much.
# this could potentially help with other similar issues? maybe?

from transformers import BertTokenizer, BertForMaskedLM
class BertMLMFeatureBuilder(MLMFeatureBuilder):
    """Includes:
    - tokenizer (for vocab indexing conversions), and
    - run(List[token], int) -> torch.Tensor(vocab size) that does MLM.
    """
    def __init__(self, should_lemmatize = True):
        super(MLMFeatureBuilder, self).__init__()
        self.tokenizer = BertTokenizer.from_pretrained('bert-large-uncased', local_files_only = True)
        self.model = BertForMaskedLM.from_pretrained('bert-large-uncased', local_files_only = True)
        self.model.eval()
        self.model.to('cuda')

        self.should_lemmatize = should_lemmatize

        if not should_lemmatize:
            self.vocab_size = len(self.tokenizer.get_vocab())
        else:
            logger.info("Constructing lemmatizer map")
            vocab = self.tokenizer.get_vocab()
            lemmatizer_map = {
                token: lemmatize(token)
                for token, index in tqdm(vocab.items())
                if not token.startswith("##") and not (token.startswith('[') and token.endswith(']'))
            }
            self.lemmas = list({
                lemma for token, lemma in lemmatizer_map.items()
            })
            self.vocab_size = len(self.lemmas)
            lemma_index = { lem: i for i, lem in enumerate(self.lemmas) }

            num_all_tokens = len(vocab)
            num_valid_tokens = len(lemmatizer_map)

            logger.info("Num total tokens: %d" % len(vocab))
            logger.info("Num valid tokens: %d" % num_valid_tokens)
            # print(list(lemmatizer_map))
            logger.info("Num lemmas: %d" % self.vocab_size)
            # print(self.lemmas)

            lemma_indices = []
            token_indices = []
            for token, lemma in lemmatizer_map.items():
                lemma_indices.append(lemma_index[lemma])
                token_indices.append(vocab[token])

            transform_values = torch.ones(num_valid_tokens, dtype = torch.float32, device = torch.device('cuda'))
            transform_indices = torch.LongTensor([lemma_indices, token_indices]).to('cuda')
            self.lemma_transform = torch.sparse.FloatTensor(
                transform_indices, transform_values, torch.Size([self.vocab_size, num_all_tokens])
            )

    def get_tokens(self, indices):
        if self.should_lemmatize:
            return [self.lemmas[i] for i in indices.tolist()]
        else:
            return self.tokenizer.convert_ids_to_tokens(indices)

    def run(self, tokens, index, keep_indices = None):
        # copy tokens, then replace target span with a mask.
        # Special handling for "n't" contractions: make sure to remove the whole thing
        toks = list(tokens)
        if toks[index] == "n't":
            toks.pop(index - 1)
            toks[index - 1] = self.tokenizer.mask_token
            yus = True
        elif index < len(toks) - 1 and toks[index + 1] == "n't":
            toks.pop(index)
            toks[index] = self.tokenizer.mask_token
            yus = True
        else:
            toks[index] = self.tokenizer.mask_token
            yus = False
        text = " ".join(["[CLS]"] + toks + ["[SEP]"])
        text = text.replace(" n't", "n't") # combine "n't" contractions together

        # if yus:
        #     print(tokens)
        #     print(text)

        # Retokenize input for the encoder
        tokenized_text = self.tokenizer.tokenize(text)
        masked_index = indexOfFirst(tokenized_text, self.tokenizer.mask_token)
        assert masked_index > -1

        # Convert token to vocabulary indices
        indexed_tokens = self.tokenizer.convert_tokens_to_ids(tokenized_text)
        # Define sentence A and B indices associated to 1st and 2nd sentences (see paper)
        segment_ids = [0] * len(tokenized_text)

        # Convert inputs to PyTorch tensors
        tokens_tensor = torch.tensor([indexed_tokens]).to('cuda')
        segment_tensors = torch.tensor([segment_ids]).to('cuda')

        # Predict all tokens
        with torch.no_grad():
            outputs = self.model(tokens_tensor, segment_tensors)
            predictions = outputs[0]
            # print(text)
            mask_predictions = torch.nn.functional.softmax(predictions[0, masked_index], dim = 0)
            # print_vector(mask_predictions, self.tokenizer.convert_ids_to_tokens)
            if self.should_lemmatize:
                mask_predictions = torch.matmul(self.lemma_transform, mask_predictions.unsqueeze(1)).squeeze(1)
                # renormalize after lemmatizing to fix cases with high-probability ##partial ##wordpieces
                mask_predictions = mask_predictions / mask_predictions.sum()
                # print_vector(mask_predictions, self.get_tokens)
            if keep_indices is not None:
                # don't necessarily renormalize here; can always do that later.
                mask_predictions = torch.index_select(mask_predictions, 0, keep_indices)
                # print_vector(mask_predictions, lambda indices: self.get_tokens([keep_indices[i] for i in indices]))
            # mask_predictions = mask_predictions / mask_predictions.sum()

        # top_probs, top_indices = torch.topk(mask_predictions, 10)
        # if keep_indices is not None:
        #     top_indices = torch.index_select(keep_indices, 0, top_indices)
        # top_tokens = self.get_tokens(top_indices)
        # print(text)
        # print(" ".join(["%15s" % x for x in top_tokens]))
        # print(" ".join(["%15.5f" % x for x in top_probs]))

        return mask_predictions

# TODO perhaps add some other features in..
# - animacy?
# - number?
# - reflexivity?
# I _could_ just do those directly on the original text...but maybe BERT gives stronger signal?
# Idk.

# from transformers import RobertaTokenizer, RobertaForMaskedLM
# tokenizer = RobertaTokenizer.from_pretrained('roberta-large')
# model = RobertaForMaskedLM.from_pretrained('roberta-large')

model = BertMLMFeatureBuilder(should_lemmatize = True)
data_sources = {
    "dev": {
        "path": "frame-induction/conll08/out/mlm-inputs/dev.jsonl.gz",
        "size": 1333
    },
    "train": {
        "path": "frame-induction/conll08/out/mlm-inputs/train.jsonl.gz",
        "size": 39278
    },
    "test": {
        "path": "frame-induction/conll08/out/mlm-inputs/test.jsonl.gz",
        "size": 2398
    }
}
final_vocab_size = 1024
output_dir = "frame-induction/conll08/input/mlm"

targets = {
    "devel:359:plunge:18",
    "devel:359:press",
    "devel:1025:close",
    "devel:637:purchase",
    "devel:487:trigger",
    "devel:1009:trade",
    "devel:1053:believe:3",
}
# just for debugging
def should_include(sentence_id, verb_lemma, index):
    # string = "%s:%s:%s" % (sentence_id, verb_lemma, index)
    # if any([string.startswith(t) for t in targets]):
    #     print(string)
    #     return True
    # else:
    #     return False
    return True

import itertools
def get_lines(split):
    source = data_sources[split]
    return tqdm(read_lines(source["path"]), total = source["size"])
    # return itertools.islice(read_lines(data_sources[split]), 5)

def run_symm_left(tokens, index, keep_indices):
    new_tokens = list(tokens)
    new_tokens.insert(index, "and")
    new_tokens.insert(index, model.tokenizer.mask_token)
    return model.run(new_tokens, index, keep_indices)

def run_symm_right(tokens, index, keep_indices):
    new_tokens = list(tokens)
    new_tokens.insert(index + 1, "and")
    new_tokens.insert(index + 2, model.tokenizer.mask_token)
    return model.run(new_tokens, index + 2, keep_indices)

def run_symm_both(tokens, index, keep_indices):
    left = run_symm_left(tokens, index, keep_indices)
    right = run_symm_right(tokens, index, keep_indices)
    product = torch.nn.functional.softmax(left.log() + right.log(), dim = 0)
    return product

all_verb_data = {
    "masked": {
        "run": model.run,
        "verbs": {},
        "args": {}
    },
    # "symm_left": {
    #     "run": run_symm_left,
    #     "verbs": {},
    #     "args": {}
    # },
    # "symm_right": {
    #     "run": run_symm_right,
    #     "verbs": {},
    #     "args": {}
    # },
    "symm_both": {
        "run": run_symm_both,
        "verbs": {},
        "args": {}
    },
}

base_vec = torch.zeros(model.vocab_size, dtype = torch.float32, device = torch.device('cuda'))
def make_dist_data():
    return {
        "total": 0,
        "pcounts": torch.zeros_like(base_vec),
        "maxes": torch.zeros_like(base_vec),
    }

def get_dist_data(mode_verb_data, verb):
    if verb not in mode_verb_data:
        mode_verb_data[verb] = make_dist_data()
    return mode_verb_data[verb]


def print_prior(verb, verb_data):
    probs = verb_data["pcounts"] / verb_data["total"]
    top_probs, top_indices = torch.topk(probs, 30)
    top_tokens = model.get_tokens(top_indices)

    print(verb)
    print("Instances: %s" % verb_data["total"])
    print(" ".join(["%15s" % x for x in top_tokens]))
    print(" ".join(["%15.5f" % x for x in top_probs]))

# === MAIN ===

# give it a read through and construct verb-wise priors
logger.info("Constructing priors")
for split in data_sources:
    logger.info("Reading %s" % split)
    for line in get_lines(split):
        input_json = json.loads(line)
        tokens = input_json["sentenceTokens"]
        for verb, verb_indices in input_json["verbs"].items():
            for mode, mode_data in all_verb_data.items():
                verb_data = get_dist_data(mode_data["verbs"], verb)
                for index in verb_indices:
                    if should_include(input_json["sentenceId"], verb, index):
                        verb_data["total"] += 1
                        probs = mode_data["run"](tokens, index, None)
                        verb_data["pcounts"].add_(probs)
                        verb_data["maxes"] = torch.max(verb_data["maxes"], probs)
        for verb, arg_indices in input_json["argsByVerb"].items():
            for mode, mode_data in all_verb_data.items():
                args_data = get_dist_data(mode_data["args"], verb)
                for index in arg_indices:
                    if should_include(input_json["sentenceId"], verb, index):
                        args_data["total"] += 1
                        probs = mode_data["run"](tokens, index, None)
                        args_data["pcounts"].add_(probs)
                        args_data["maxes"] = torch.max(args_data["maxes"], probs)
                        # if keep_indices is not None:
                        #     top_indices = torch.index_select(keep_indices, 0, top_indices)
                        # top_tokens = self.get_tokens(top_indices)
                        # print(text)


# debug printing
# print("==== PRIORS ====")
# sorted_verb_data = sorted(list(all_verb_data["masked"]["verbs"].items()), key = lambda t: t[1]["total"])
# for verb, verb_data in sorted_verb_data:
#     print_prior(verb, verb_data)

# record final vocabularies
logger.info("Computing vocabularies")
for mode, mode_data in all_verb_data.items():
    for verb, verb_data in mode_data["verbs"].items():
        top_pcounts, top_indices = torch.topk(verb_data["maxes"], final_vocab_size)
        verb_data["keep_indices"] = top_indices
    for verb, arg_data in mode_data["args"].items():
        top_pcounts, top_indices = torch.topk(arg_data["maxes"], final_vocab_size)
        arg_data["keep_indices"] = top_indices

for mode, mode_data in all_verb_data.items():
    logger.info("Writing features for mode: %s" % mode)
    mode_dir = os.path.join(output_dir, mode)
    os.makedirs(mode_dir, exist_ok = True)

    logger.info("Writing vocabularies")
    verb_vocabs = {
        verb: model.get_tokens(verb_data["keep_indices"])
        for verb, verb_data in mode_data["verbs"].items()
    }
    verb_vocab_file = os.path.join(mode_dir, "verb_vocabs.json")
    with open(verb_vocab_file, 'wt') as f:
        f.write(json.dumps(verb_vocabs))
    arg_vocabs = {
        verb: model.get_tokens(arg_data["keep_indices"])
        for verb, arg_data in mode_data["args"].items()
    }
    arg_vocab_file = os.path.join(mode_dir, "arg_vocabs.json")
    with open(arg_vocab_file, 'wt') as f:
        f.write(json.dumps(arg_vocabs))

    logger.info("Writing data")
    # write verb IDs and vectors
    for split in data_sources:
        logger.info("Processing %s" % split)
        verb_ids_file = os.path.join(mode_dir, "%s_verb_ids.jsonl.gz" % split)
        arg_ids_file = os.path.join(mode_dir, "%s_arg_ids.jsonl.gz" % split)
        verb_vecs_file = os.path.join(mode_dir, "%s_verb_vecs.bin" % split)
        arg_vecs_file = os.path.join(mode_dir, "%s_arg_vecs.bin" % split)
        with gzip.open(verb_ids_file, "wt") as f_verb_ids:
            with open(verb_vecs_file, "wb") as f_verb_vecs:
                with gzip.open(arg_ids_file, "wt") as f_arg_ids:
                    with open(arg_vecs_file, "wb") as f_arg_vecs:
                        for line in get_lines(split):
                            input_json = json.loads(line)
                            tokens = input_json["sentenceTokens"]
                            for verb, verb_indices in input_json["verbs"].items():
                                verb_data = mode_data["verbs"][verb]
                                for index in verb_indices:
                                    instance_id = {
                                        "sentenceId": input_json["sentenceId"],
                                        "verbLemma": verb,
                                        "index": index
                                    }
                                    instance_vec = mode_data["run"](tokens, index, verb_data["keep_indices"])
                                    f_verb_ids.write(json.dumps(instance_id) + "\n")
                                    f_verb_vecs.write(instance_vec.cpu().numpy().tobytes())
                            for verb, arg_indices in input_json["argsByVerb"].items():
                                arg_data = mode_data["args"][verb]
                                for index in arg_indices:
                                    instance_id = {
                                        "sentenceId": input_json["sentenceId"],
                                        "verbLemma": verb,
                                        "index": index
                                    }
                                    instance_vec = mode_data["run"](tokens, index, arg_data["keep_indices"])
                                    f_arg_ids.write(json.dumps(instance_id) + "\n")
                                    f_arg_vecs.write(instance_vec.cpu().numpy().tobytes())
