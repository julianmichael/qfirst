package qfirst.paraphrase

import qfirst.ClauseResolution.ArgStructure

import io.circe.generic.JsonCodec

@JsonCodec case class QuestionParaphraseLabels(
  correct: Set[String],
  incorrect: Set[String],
  )
object ParaphraseLabels

@JsonCodec case class VerbParaphraseLabels(
  correctClauses: Set[ArgStructure],
  incorrectClauses: Set[ArgStructure],
  questionParaphrases: Map[String, QuestionParaphraseLabels]
)
object VerbParaphraseLabels {
  def empty = VerbParaphraseLabels(Set(), Set(), Map())
}
