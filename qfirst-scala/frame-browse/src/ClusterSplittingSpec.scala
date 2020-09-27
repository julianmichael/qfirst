package qfirst.frame.browse

// import qfirst.frame.ArgumentId
// import qfirst.frame.ClausalQuestion
import qfirst.frame.ClusterSplittingCriterion
// import qfirst.frame.MergeTree
// import qfirst.frame.VerbFrame

// import qfirst.clause.ArgStructure

// import qasrl.ArgumentSlot

import io.circe.generic.JsonCodec

import monocle.macros._

import cats.implicits._

@Lenses @JsonCodec case class ClusterSplittingSpec(
  verbCriterion: ClusterSplittingCriterion,
  argumentCriterion: ClusterSplittingCriterion
) {
}
object ClusterSplittingSpec {
}
