package qfirst.frame

import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class VerbClusterModel[VerbType, Arg](
  verbType: VerbType,
  verbClusterTree: MergeTree[Set[VerbId]],
  argumentClustering: ArgumentClustering[Arg]
) {
  val numVerbInstances = verbClusterTree.size
}
object VerbClusterModel

@JsonCodec case class ArgumentClustering[Arg](
  clusterTreeOpt: Option[MergeTree[Set[ArgumentId[Arg]]]],
  extraRoles: Map[String, Set[ArgumentId[Arg]]]
) {
  def split[A](f: ArgumentId[Arg] => A): Map[A, ArgumentClustering[Arg]] = {
    val treesOpt = clusterTreeOpt.map(_.group(_.groupBy(f)))
    val extras = extraRoles
      .mapVals(_.groupBy(f)).toList
      .foldMap { case (label, groups) =>
        groups.mapVals(argSet => Map(label -> argSet))
      }
    val keys = treesOpt.foldMap(_.keySet) ++ extras.keySet

    keys.iterator.map(key =>
      key -> ArgumentClustering(treesOpt.flatMap(_.get(key)), extras.get(key).combineAll)
    ).toMap
  }
}
object ArgumentClustering
