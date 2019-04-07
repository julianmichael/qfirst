package qfirst.paraphrase

import qfirst.ClauseResolution.ArgStructure

import io.circe.generic.JsonCodec

import monocle.macros.Lenses

@JsonCodec @Lenses case class QuestionParaphraseLabels(
  correct: Set[String],
  incorrect: Set[String])
object QuestionParaphraseLabels

@JsonCodec @Lenses case class VerbParaphraseLabels(
  correctClauses: Set[ArgStructure],
  incorrectClauses: Set[ArgStructure],
  questionParaphrases: Map[String, QuestionParaphraseLabels]
)
object VerbParaphraseLabels {
  def empty = VerbParaphraseLabels(Set(), Set(), Map())
}
