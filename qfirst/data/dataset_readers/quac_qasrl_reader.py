import json
import logging
from typing import Any, Dict, List, Tuple

from overrides import overrides

from allennlp.common.file_utils import cached_path
from allennlp.data.dataset_readers.dataset_reader import DatasetReader
from allennlp.data.instance import Instance
from allennlp.data.fields import TextField, SpanField, LabelField, ListField, MetadataField
from allennlp.data.dataset_readers.reading_comprehension import util
from allennlp.data.token_indexers import SingleIdTokenIndexer, TokenIndexer
from allennlp.data.tokenizers import Token, Tokenizer, WordTokenizer

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

@DatasetReader.register("quac_qasrl")
class QuACQasrlReader(DatasetReader):

    def __init__(self,
                 token_indexers: Dict[str, TokenIndexer] = None,
                 lazy: bool = False) -> None:
        super().__init__(lazy)
        self._tokenizer = WordTokenizer()
        self._token_indexers = token_indexers or {'tokens': SingleIdTokenIndexer()}

    @overrides
    def _read(self, file_path: str):
        file_path = cached_path(file_path)
        logger.info("Reading file at %s", file_path)
        with open(file_path) as dataset_file:
            dataset_json = json.load(dataset_file)
            dataset = dataset_json['data']
        logger.info("Reading the dataset")
        for article in dataset[0:2]:
            for paragraph_json in article['paragraphs']:
                paragraph = paragraph_json["context"]
                paragraph_tokens = self._tokenizer.tokenize(paragraph)
                paragraph_text_field = TextField(paragraph_tokens, self._token_indexers)
                paragraph_offsets = [(token.idx, token.idx + len(token.text)) for token in paragraph_tokens]
                qas = paragraph_json['qas']
                metadata = {}
                metadata["instance_id"] = [qa['id'] for qa in qas]
                for qa in qas:
                    question_text = qa["question"].strip().replace("\n", "")
                    question_tokens = self._tokenizer.tokenize(question_text)
                    def get_span(answer):
                        char_span_start = answer["answer_start"]
                        char_span_end = answer["answer_start"] + len(answer["text"])
                        (span_start, span_end), error = util.char_span_to_token_span(
                            paragraph_offsets, (char_span_start, char_span_end))
                        if error:
                            logger.debug("Passage: %s", paragraph)
                            logger.debug("Passage tokens: %s", paragraph_tokens)
                            logger.debug("Answer span: (%d, %d)", char_span_start, char_span_end)
                            logger.debug("Token span: (%d, %d)", span_start, span_end)
                            logger.debug("Tokens in answer: %s", paragraph_tokens[span_start:span_end + 1])
                            logger.debug("Answer: %s", paragraph[char_span_start:char_span_end])
                        return [span_start, span_end + 1]
                    spans = [
                        get_span(answer)
                        for answer in qa["answers"]
                        if answer["text"] != "CANNOTANSWER"
                    ]

                    if len(spans) == 0:
                        spans_field = ListField([SpanField(-1, -1, paragraph_text_field)])
                    else:
                        spans_field = ListField([SpanField(s[0], s[1]-1, paragraph_text_field) for s in spans])

                    answer_judgments = [
                        { "isValid": True, "spans": [get_span(answer)] } if answer["text"] != "CANNOTANSWER" else { "isValid": False }
                        for answer in qa["answers"]
                    ]
                    metadata['question_label'] = {"answerJudgments": answer_judgments}

                    instance_dict = {
                        'text': paragraph_text_field,
                        'question_text': TextField(question_tokens, self._token_indexers),
                        'answer_spans': spans_field,
                        'num_answers': LabelField(label = len(qa["answers"]), skip_indexing = True),
                        'num_invalids': LabelField(label = len([a for a in qa["answers"] if a["text"] == "CANNOTANSWER"]), skip_indexing = True),
                        'metadata': MetadataField(metadata),
                    }
                    yield Instance(instance_dict)
