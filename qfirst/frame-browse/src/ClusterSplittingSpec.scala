package qfirst.frame.browse

import qfirst.frame.ClusterSplittingCriterion

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses @JsonCodec case class ClusterSplittingSpec(
  verbCriterion: ClusterSplittingCriterion,
  argumentCriterion: ClusterSplittingCriterion
)
object ClusterSplittingSpec

