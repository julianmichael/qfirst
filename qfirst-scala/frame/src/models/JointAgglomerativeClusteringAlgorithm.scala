package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.data.NonEmptyVector
import cats.implicits._

class JointAgglomerativeClusteringAlgorithm[I, InnerIndex](
  val innerAlgorithm: ClusteringAlgorithm { type Index = InnerIndex },
  getSubInstances: I => NonEmptyVector[InnerIndex],
  getLossPenalty: Int => Double // should grow monotonically
) extends AgglomerativeClusteringAlgorithm {
  type Index = I
  type ClusterParam = NonEmptyVector[(MergeTree[innerAlgorithm.Index], innerAlgorithm.ClusterParam)]

  val innerStoppingCondition = (
    trees: Map[Int, (MergeTree[InnerIndex], innerAlgorithm.ClusterParam)], i: Int, j: Int, newLoss: Double
  ) => {
    val lossBefore = trees.values.map(_._1.loss).sum + getLossPenalty(trees.size)
    val lossAfter = (trees - i - j).values.map(_._1.loss).sum + newLoss + getLossPenalty(trees.size - 1)
    lossAfter > lossBefore
  }

  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    val innerIndices = getSubInstances(index)
    innerAlgorithm.runAgglomerativeClustering(
      innerIndices,
      innerStoppingCondition
    )
  }

  def getInstanceLoss(
    instance: Index,
    param: ClusterParam
  ): Double = {
    param.foldMap(_._1.loss) + getLossPenalty(param.size.toInt)
  }

  def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    innerAlgorithm.runPartialAgglomerativeClustering(
      leftParam |+| rightParam, innerStoppingCondition
    )
  }

  def mergeLoss(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    val mergedParam = innerAlgorithm.runPartialAgglomerativeClustering(
      leftParam |+| rightParam, innerStoppingCondition
    )
    mergedParam.foldMap(_._1.loss) + getLossPenalty(mergedParam.size.toInt)
  }
}
