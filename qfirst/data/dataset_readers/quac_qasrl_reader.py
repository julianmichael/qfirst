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

from allennlp.common.util import get_spacy_model

@DatasetReader.register("quac_qasrl")
class QuACQasrlReader(DatasetReader):

    def __init__(self,
                 max_sentence_length: int = 80,
                 token_indexers: Dict[str, TokenIndexer] = None,
                 lazy: bool = False) -> None:
        super().__init__(lazy)
        self._tokenizer = WordTokenizer()
        self._max_sentence_length = max_sentence_length
        self._token_indexers = token_indexers or {'tokens': SingleIdTokenIndexer()}
        self._spacy = get_spacy_model('en_core_web_sm', parse = True, ner = False, pos_tags = False)

    @overrides
    def _read(self, file_path: str):
        num_qas_returned = 0
        num_qas_skipped = 0
        file_path = cached_path(file_path)
        logger.info("Reading file at %s", file_path)
        with open(file_path) as dataset_file:
            dataset_json = json.load(dataset_file)
            dataset = dataset_json['data']
        logger.info("Reading the dataset")
        for article in dataset:
            for paragraph_json in article['paragraphs']:
                paragraph = paragraph_json["context"]
                paragraph_data = self._spacy(paragraph)
                sentences = paragraph_data.sents
                # paragraph_tokens = self._tokenizer.tokenize(paragraph)
                # paragraph_text_field = TextField(paragraph_tokens, self._token_indexers)
                # paragraph_offsets = [(token.idx, token.idx + len(token.text)) for token in paragraph_tokens]
                qas = paragraph_json['qas']
                metadata = {}
                metadata["instance_id"] = [qa['id'] for qa in qas]
                for qa in qas:
                    question_text = qa["question"].strip().replace("\n", "")
                    question_tokens = [Token(t.text) for t in self._spacy(question_text)]
                    def is_idx_in(i, sent):
                        return i >= sent.start_char and i < sent.end_char
                    def is_ans_in(ans, sent):
                        return is_idx_in(ans["answer_start"], sent) or is_idx_in(ans["answer_start"] + len(ans["text"]), sent)
                    def get_sentences(answers):
                        return [
                            sent for sent in paragraph_data.sents
                            if any([is_ans_in(ans, sent)
                                    for ans in qa["answers"]
                                    if ans["text"] != "CANNOTANSWER"])
                        ]
                    sentences = get_sentences(qa["answers"])
                    if len(sentences) == 0:
                        sent_tokens = [t for t in list(paragraph_data.sents)[0]]
                    else:
                        sent_start_idx = min([s.start_char for s in sentences])
                        sent_end_idx = max([s.end_char for s in sentences])
                        sent_tokens = [
                            t for t in paragraph_data
                            if t.idx >= sent_start_idx and t.idx < sent_end_idx
                        ]
                    sent_text_field = TextField([Token(t.text) for t in sent_tokens], self._token_indexers)
                    def get_span(answer):
                        answer_token_indices = [
                            i for i, t in enumerate(sent_tokens)
                            if t.idx < (answer["answer_start"] + len(answer["text"])) and answer["answer_start"] < (t.idx + len(t.text))
                        ]
                        return [min(answer_token_indices), max(answer_token_indices) + 1]
                    spans = [
                        get_span(answer)
                        for answer in qa["answers"]
                        if answer["text"] != "CANNOTANSWER"
                    ]

                    # sanity check
                    for ans in qa["answers"]:
                        if ans["text"] != "CANNOTANSWER":
                            span = get_span(ans)
                            recovered_text = "".join([t.text_with_ws for t in sent_tokens[span[0]:span[1]]])
                            if recovered_text != ans["text"]:
                                logger.debug("Passage: %s", paragraph)
                                logger.debug("Passage tokens: %s", [t for t in paragraph_data])
                                logger.debug("Sent tokens: %s", sent_tokens)
                                logger.debug("Correct answer: %s", ans["text"])
                                logger.debug("Induced answer: %s", recovered_text)
                                # logger.debug("Answer span: (%d, %d)", char_span_start, char_span_end)
                                # logger.debug("Token span: (%d, %d)", span_start, span_end)
                                # logger.debug("Tokens in answer: %s", paragraph_tokens[span_start:span_end + 1])
                                # logger.debug("Answer: %s", paragraph[char_span_start:char_span_end])

                    if len(spans) == 0:
                        spans_field = ListField([SpanField(-1, -1, sent_text_field)])
                    else:
                        spans_field = ListField([SpanField(s[0], s[1]-1, sent_text_field) for s in spans])

                    answer_judgments = [
                        { "isValid": True, "spans": [get_span(answer)] } if answer["text"] != "CANNOTANSWER" else { "isValid": False }
                        for answer in qa["answers"]
                    ]
                    metadata['question_label'] = {"answerJudgments": answer_judgments}

                    instance_dict = {
                        'text': sent_text_field,
                        'question_text': TextField(question_tokens, self._token_indexers),
                        'answer_spans': spans_field,
                        'num_answers': LabelField(label = len(qa["answers"]), skip_indexing = True),
                        'num_invalids': LabelField(label = len([a for a in qa["answers"] if a["text"] == "CANNOTANSWER"]), skip_indexing = True),
                        'metadata': MetadataField(metadata),
                    }

                    if len(sent_tokens) > self._max_sentence_length:
                        num_qas_skipped += 1
                    else:
                        yield Instance(instance_dict)
                        num_qas_returned += 1
        logger.info("Number of QAs processed: %d" % num_qas_returned)
        logger.info("Number of QAs skipped: %d" % num_qas_skipped)
