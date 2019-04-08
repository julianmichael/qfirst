package qfirst.paraphrase

import qfirst.ClauseResolution.ArgStructure
import qfirst.EquivalenceWithApartness

import io.circe.generic.JsonCodec

import monocle.macros.Lenses

import cats.Monoid

import qasrl.ArgumentSlot

// @JsonCodec @Lenses case class QuestionParaphraseLabels(
//   correct: Set[(ArgStructure, ArgumentSlot)],
//   incorrect: Set[(ArgStructure, ArgumentSlot)])
// object QuestionParaphraseLabels {
//   implicit val questionParaphraseLabelsMonoid: Monoid[QuestionParaphraseLabels] =
//     new Monoid[QuestionParaphraseLabels] {
//       def empty: QuestionParaphraseLabels = QuestionParaphraseLabels(Set(), Set())
//       def combine(x: QuestionParaphraseLabels, y: QuestionParaphraseLabels): QuestionParaphraseLabels =
//         QuestionParaphraseLabels(x.correct ++ y.correct, x.incorrect ++ y.incorrect)
//     }
// }

@JsonCodec @Lenses case class VerbParaphraseLabels(
  correctClauses: Set[ArgStructure],
  incorrectClauses: Set[ArgStructure],
  paraphrases: EquivalenceWithApartness[(ArgStructure, ArgumentSlot)]
  // questionParaphrases: Map[String, QuestionParaphraseLabels]
)
object VerbParaphraseLabels {
  def empty = VerbParaphraseLabels(Set(), Set(), EquivalenceWithApartness.empty)
}
