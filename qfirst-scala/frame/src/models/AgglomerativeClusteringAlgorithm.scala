package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector

trait AgglomerativeClusteringAlgorithm {
  type ClusterParam
  type Index

  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam

  // gets loss for an instance wrt a cluster.
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double

  def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam

  def mergeLoss(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double

  // override eg for max
  def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    rightLoss: Double
  ) = newLoss - leftLoss - rightLoss

  def runPartialAgglomerativeClustering(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    val indices = initTrees.toVector.indices
    var currentTrees = initTrees.zipWithIndex.map(_.swap).toVector.toMap

    val distances = Array.ofDim[Double](initTrees.size.toInt, initTrees.size.toInt)

    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      indices.map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(left, leftParam, right, rightParam)
          val delta = getLossChangePriority(loss, left.loss, right.loss)
          distances(leftIndex)(rightIndex) = delta
          distances(rightIndex)(leftIndex) = delta
          queues(leftIndex) += (rightIndex -> loss)
        }
    }

    var done: Boolean = false
    while(!done && currentTrees.size > 1) {
      val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
      val (j, newLoss) = queues(i).head
      done = stoppingCondition(currentTrees, i, j, newLoss)
      if(!done) {
        queues -= j // remove j from consideration
        queues(i).clear() // clear i's queue since we'll refresh it
        val emptyQueues = queues.iterator.flatMap { case (k, q) =>
          q --= List(i, j)  // remove stale entries for i and j from all queues
          if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
        }
        queues --= emptyQueues // and remove other now-empty queues
        val (leftTree, leftParam) = currentTrees(i)
        val (rightTree, rightParam) = currentTrees(j)
        val newTree = MergeTree.Merge(newLoss, leftTree, rightTree)
        val newParam = mergeParams(leftTree, leftParam, rightTree, rightParam)
        currentTrees --= List(i, j) // remove i and j from current trees
                                    // get new merge candidates before adding new cluster back in
        val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
          val loss = mergeLoss(newTree, newParam, right, rightParam)
          val delta = getLossChangePriority(loss, newTree.loss, right.loss)
          // update distances before inserting anything into queues to keep things consistent
          distances(i)(k) = delta
          distances(k)(i) = delta
          k -> loss
        }.toList
        currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
        queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
      }

    }
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }

  def runAgglomerativeClustering(
    indices: NonEmptyVector[Index],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {

    val initTrees = indices.map { index =>
      val param = getSingleInstanceParameter(index)
      val loss = getInstanceLoss(index, param)
      (MergeTree.Leaf(loss, index): MergeTree[Index]) -> param
    }

    runPartialAgglomerativeClustering(initTrees, stoppingCondition)
  }

  def runFullAgglomerativeClustering(
    instances: NonEmptyVector[Index],
  ): (MergeTree[Index], ClusterParam) = {
    runAgglomerativeClustering(
      instances,
      (_, _, _, _) => false
    ).head
  }
}
