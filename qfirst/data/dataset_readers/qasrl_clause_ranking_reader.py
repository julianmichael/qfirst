import codecs
import os
import logging
from typing import Dict, List, Optional, Tuple, Set
from collections import Counter
import json
import gzip

from overrides import overrides

from allennlp.common import Params
from allennlp.common.file_utils import cached_path
from allennlp.data.dataset_readers.dataset_reader import DatasetReader
from allennlp.data.instance import Instance
from allennlp.data.token_indexers import SingleIdTokenIndexer, TokenIndexer
from allennlp.data.tokenizers import Token, Tokenizer, WordTokenizer
from allennlp.data.fields import Field, IndexField, TextField, SequenceLabelField, LabelField, ListField, MetadataField, SpanField
from allennlp.data.dataset_readers.dataset_utils.span_utils import enumerate_spans

from json.decoder import JSONDecodeError

from nrl.common.span import Span
from nrl.data.util import cleanse_sentence_text

from qfirst.data.util import get_slot_label_namespace

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

@DatasetReader.register("qasrl_clause_ranking")
class QasrlClauseRankingReader(DatasetReader):
    def __init__(self,
                 include_metadata: bool = False,
                 tokenizer: Tokenizer = None,
                 token_indexers: Dict[str, TokenIndexer] = None,
                 lazy: bool = False) -> None:
        super().__init__(lazy)
        self._include_metadata = include_metadata
        self._tokenizer = tokenizer or WordTokenizer()
        self._token_indexers = token_indexers or {'tokens': SingleIdTokenIndexer()}

    @overrides
    def _read(self, file_list: str):
        instances = []
        for file_path in file_list.split(","):
            if file_path.strip() == "":
                continue

            logger.info("Reading QA-SRL clause ranking instances from file at: %s", file_path)
            data = []
            if file_path.endswith('.gz'):
                with gzip.open(cached_path(file_path), 'r') as f:
                    for line in f:
                        try:
                            sentence_json = json.loads(line)
                            for i in self._instances_from_json(sentence_json):
                                yield i
                        except JSONDecodeError as e:
                            print(e)
            elif file_path.endswith(".jsonl"):
                with codecs.open(cached_path(file_path), 'r', encoding='utf8') as f:
                    for line in f:
                        for i in self._instances_from_json(json.loads(line)):
                            yield i

    def _instances_from_json(self, json):
        verb_metadata = {
            "sentenceId": json["sentenceId"],
            "sentenceTokens": json["sentenceTokens"],
            "verbIndex": json["verbIndex"],
            "verbInflectedForms": json["verbInflectedForms"]
        }
        for clause_json in json["clauses"]:
            metadata_field = MetadataField({**verb_metadata, **clause_json})
            sentence_field = TextField([Token(t) for t in json["sentenceTokens"]], self._token_indexers)
            clause_field = TextField(self._tokenizer.tokenize(clause_json["string"]), self._token_indexers)
            label = "entailed" if clause_json["prob"] >= 0.5 else "non-entailed"
            label_field = LabelField(label)
            instance_dict = {
                "premise": sentence_field,
                "hypothesis": clause_field,
                "label": label_field,
            }
            if self._include_metadata:
                instance_dict["metadata"] = metadata_field
            yield Instance(instance_dict)
