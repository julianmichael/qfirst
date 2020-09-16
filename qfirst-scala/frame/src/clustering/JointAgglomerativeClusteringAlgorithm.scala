package qfirst.frame.clustering

import qfirst.frame.MergeTree

import cats.data.NonEmptyVector
import cats.implicits._

import cats.effect.IO
import freelog.EphemeralTreeLogger

class JointAgglomerativeClusteringAlgorithm[I, InnerIndex, InnerParam](
  val innerAlgorithm: AgglomerativeClusteringAlgorithm { type Index = InnerIndex; type ClusterParam = InnerParam },
  getSubInstances: I => Vector[InnerIndex],
  getLossPenalty: Vector[Int] => Double // should grow monotonically
) extends AgglomerativeClusteringAlgorithm {
  type Index = I
  type ClusterParam = Vector[(MergeTree[InnerIndex], InnerParam)]

  val innerStoppingCondition = (
    trees: Map[Int, (MergeTree[InnerIndex], InnerParam)], i: Int, j: Int, newLoss: Double
  ) => {
    val sizes = trees.mapVals(_._1.size.toInt)
    val newSize = sizes(i) + sizes(j)
    val lossBefore = trees.values.map(_._1.loss).sum + getLossPenalty(sizes.values.toVector)
    val lossAfter = (trees - i - j).values.map(_._1.loss).sum + newLoss + getLossPenalty((sizes - i - j + (i -> newSize)).values.toVector)
    lossAfter > lossBefore
  }

  // don't log stuff from inner clustering
  // implicit val noopLogger = EphemeralTreeLogger.noop[IO, String]

  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    NonEmptyVector.fromVector(getSubInstances(index)).map { innerIndices =>
      val res = innerAlgorithm.runAgglomerativeClustering(
        innerIndices,
        innerStoppingCondition
      )(EphemeralTreeLogger.noop[IO, String])
      // println(s"INIT  - ${innerIndices.size}: ${res.size}")
      res
    }.map(_.toVector).getOrElse(Vector())
  }

  def getInstanceLoss(
    instance: Index,
    param: ClusterParam
  ): Double = {
    param.foldMap(_._1.loss) + getLossPenalty(param.map(_._1.size.toInt).toVector)
  }

  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      // println(s"MERGE - ${left.size + right.size}: ${res.size}")
      (NonEmptyVector.fromVector(left), NonEmptyVector.fromVector(right)).mapN { (l, r) =>
        innerAlgorithm.runPartialAgglomerativeClustering(
          l |+| r, innerStoppingCondition
        )(EphemeralTreeLogger.noop[IO, String]).toVector
      }.getOrElse(left ++ right)
    }
  )

  override val mergeLossEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val mergedParam = mergeParamsEfficient.get(left, right)
      mergedParam.foldMap(_._1.loss) + getLossPenalty(mergedParam.map(_.size.toInt).toVector)
    }
  )
}
