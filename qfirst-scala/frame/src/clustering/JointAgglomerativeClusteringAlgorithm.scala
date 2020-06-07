package qfirst.frame.clustering

import qfirst.frame.MergeTree

import cats.data.NonEmptyVector
import cats.implicits._

import cats.effect.IO
import freelog.EphemeralTreeLogger

class JointAgglomerativeClusteringAlgorithm[I, InnerIndex, InnerParam](
  val innerAlgorithm: AgglomerativeClusteringAlgorithm { type Index = InnerIndex; type ClusterParam = InnerParam },
  getSubInstances: I => NonEmptyVector[InnerIndex],
  getLossPenalty: Int => Double // should grow monotonically
) extends AgglomerativeClusteringAlgorithm {
  type Index = I
  type ClusterParam = NonEmptyVector[(MergeTree[InnerIndex], InnerParam)]

  val innerStoppingCondition = (
    trees: Map[Int, (MergeTree[InnerIndex], InnerParam)], i: Int, j: Int, newLoss: Double
  ) => {
    val lossBefore = trees.values.map(_._1.loss).sum + getLossPenalty(trees.size)
    val lossAfter = (trees - i - j).values.map(_._1.loss).sum + newLoss + getLossPenalty(trees.size - 1)
    lossAfter > lossBefore
  }

  // don't log stuff from inner clustering
  implicit val noopLogger = EphemeralTreeLogger.noop[IO, String]

  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    val innerIndices = getSubInstances(index)
    val res = innerAlgorithm.runAgglomerativeClustering(
      innerIndices,
      innerStoppingCondition
    )
    // println(s"INIT  - ${innerIndices.size}: ${res.size}")
    res
  }

  def getInstanceLoss(
    instance: Index,
    param: ClusterParam
  ): Double = {
    param.foldMap(_._1.loss) + getLossPenalty(param.size.toInt)
  }

  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      // println(s"MERGE - ${left.size + right.size}: ${res.size}")
      innerAlgorithm.runPartialAgglomerativeClustering(
        left |+| right, innerStoppingCondition
      )
    }
  )

  override val mergeLossEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val mergedParam = innerAlgorithm.runPartialAgglomerativeClustering(
        left |+| right, innerStoppingCondition
      )
      mergedParam.foldMap(_._1.loss) + getLossPenalty(mergedParam.size.toInt)
    }
  )
}
