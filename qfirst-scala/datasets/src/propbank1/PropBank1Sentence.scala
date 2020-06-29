package qfirst.datasets.propbank1

import qfirst.datasets.ptb2.PTB2SentenceId

case class PropBank1Sentence(
  id: PTB2SentenceId,
  predicateArgumentStructures: List[PredicateArgumentStructure]
)
