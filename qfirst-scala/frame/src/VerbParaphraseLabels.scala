package qfirst.frame

import qfirst.clause.ArgStructure
import qfirst.frame.math.EquivalenceWithApartness

import io.circe.generic.JsonCodec

import monocle.Lens
import monocle.macros.Lenses

import cats.Monoid

import qasrl.ArgumentSlot

@JsonCodec @Lenses case class VerbParaphraseLabels(
  correctClauses: Set[ArgStructure],
  incorrectClauses: Set[ArgStructure],
  paraphrases: EquivalenceWithApartness[(ArgStructure, ArgumentSlot)]
)
object VerbParaphraseLabels {
  def empty = VerbParaphraseLabels(Set(), Set(), EquivalenceWithApartness.empty)

  def clauseAddingParaphraseLens(x: (ArgStructure, ArgumentSlot), y: (ArgStructure, ArgumentSlot)) = {
    val equal = VerbParaphraseLabels.paraphrases.composeLens(EquivalenceWithApartness.equal(x, y))
    Lens[VerbParaphraseLabels, Boolean](
      vpl => equal.get(vpl))(
      b => vpl => equal.set(b)(if(b) VerbParaphraseLabels.correctClauses.modify(_ + x._1 + y._1)(vpl) else vpl)
    )
  }
}
