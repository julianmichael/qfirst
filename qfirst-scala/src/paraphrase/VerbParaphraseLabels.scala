package qfirst.paraphrase

import qfirst.ClauseResolution.ArgStructure
import qfirst.EquivalenceWithApartness

import io.circe.generic.JsonCodec

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
}
