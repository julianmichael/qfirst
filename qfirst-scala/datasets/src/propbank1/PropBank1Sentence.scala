package qfirst.datasets.propbank1

import qfirst.datasets.PredArgStructure
import qfirst.datasets.PropBankPredicate
import qfirst.datasets.ptb2.PTB2SentenceId

case class PropBank1Sentence(
  id: PTB2SentenceId,
  predArgStructures: List[PredArgStructure[PropBankPredicate, PropBankArgument]]
)
