""" Usage:
    <file-name> --in=IN_FILE --out=OUT_FILE [--debug]
"""
# External imports
import logging
import pdb
from pprint import pprint
from pprint import pformat
from docopt import docopt
from collections import defaultdict
from operator import itemgetter
from tqdm import tqdm
import json

# Local imports
from qfirst.data.util import get_propbank_sentences
#=-----


def convert_propbank_to_qasrl(propbank_sent):
    """
    Convert a sentence represented in propbank format
    to the expected qasrl json.
    """
    (sent_tokens, attr_dict) = propbank_sent
    indices = attr_dict["verb_indices"]
    lemmas = attr_dict["verb_lemmas"]
    senses = attr_dict["verb_senses"]
    qasrl_json = {
        "sentenceId": attr_dict["sentence_id"],
        "sentenceTokens": sent_tokens
    }
    verb_entries = {}
    for verb_index, verb_lemma, verb_sense in zip(indices, lemmas, senses):
        verb_entries[str(verb_index)] = {
            "verbIndex": verb_index,
            "verbInflectedForms":{
                "stem": verb_lemma, #TODO?
                "verbLemma": verb_lemma,
                "verbSense": verb_sense,
            },
            "questionLabels":{},
        }

    qasrl_json["verbEntries"] = verb_entries

    return qasrl_json

if __name__ == "__main__":
    # Parse command line arguments
    args = docopt(__doc__)
    inp_fn = args["--in"]
    out_fn = args["--out"]
    debug = args["--debug"]
    if debug:
        logging.basicConfig(level = logging.DEBUG)
    else:
        logging.basicConfig(level = logging.INFO)

    # Read propbank sentences
    propbank_sents = get_propbank_sentences(inp_fn)

    logging.info(f"Writing output to {out_fn}")
    with open(out_fn, "w", encoding = "utf8") as fout:
        for propbank_sent in tqdm(propbank_sents):
            qasrl_json = convert_propbank_to_qasrl(propbank_sent)
            qasrl_json_str = json.dumps(qasrl_json)
            fout.write(f"{qasrl_json_str}\n")

    logging.info("DONE")
