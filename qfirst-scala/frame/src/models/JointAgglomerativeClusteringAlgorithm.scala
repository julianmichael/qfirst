package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.data.NonEmptyVector
import cats.implicits._

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

  def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val res = innerAlgorithm.runPartialAgglomerativeClustering(
      leftParam |+| rightParam, innerStoppingCondition
    )
    // println(s"MERGE - ${leftParam.size + rightParam.size}: ${res.size}")
    res
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
