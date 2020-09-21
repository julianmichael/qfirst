package qfirst.frame

import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class VerbClusterModel[VerbType, Arg](
  verbType: VerbType,
  verbClusterTree: MergeTree[Set[VerbId]],
  argumentClusterTreeOpt: Option[MergeTree[ArgumentId[Arg]]]
) {
  val numVerbInstances = verbClusterTree.size
}
object VerbClusterModel
