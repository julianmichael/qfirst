package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
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
    initTrees: Vector[(MergeTree[Index], ClusterParam)]
  ): Vector[(MergeTree[Index], ClusterParam)] = {
    val indices = trees.indices.toVector
    var currentTrees = trees.zipWithIndex.map(_.swap).toMap

    val distances = Array.ofDim[Double](initTrees.size, initTrees.size)

    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      (0 until instances.size).map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(instances, left, leftParam, right, rightParam)
          val delta = getLossChangePriority(loss, left.loss, right.loss)
          distances(leftIndex)(rightIndex) = delta
          distances(rightIndex)(leftIndex) = delta
          queues(leftIndex) += (rightIndex -> loss)
        }
    }

    var nextRank = 1
    while(!stoppingCondition(currentTrees) && currentTrees.size > 1) {
      val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
      val (j, newLoss) = queues(i).head

      queues -= j // remove j from consideration
      queues(i).clear() // clear i's queue since we'll refresh it
      val emptyQueues = queues.iterator.flatMap { case (k, q) =>
        q --= List(i, j)  // remove stale entries for i and j from all queues
        if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
      }
      queues --= emptyQueues // and remove other now-empty queues
      val (leftTree, leftParam) = currentTrees(i)
      val (rightTree, rightParam) = currentTrees(j)
      val newTree = MergeTree.Merge(nextRank, newLoss, leftTree, rightTree)
      val newParam = mergeParams(instances, leftTree, leftParam, rightTree, rightParam)
      currentTrees --= List(i, j) // remove i and j from current trees
      // get new merge candidates before adding new cluster back in
      val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
        val loss = mergeLoss(instances, newTree, newParam, right, rightParam)
        val delta = getLossChangePriority(loss, newTree.loss, right.loss)
        // update distances before inserting anything into queues to keep things consistent
        distances(i)(k) = delta
        distances(k)(i) = delta
        k -> loss
      }.toList
      currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
      queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
      nextRank = nextRank + 1
    }
    currentTrees.values.toVector
  }

  def runAgglomerativeClustering(
    indices: Vector[Index],
    stoppingCondition: Map[Int, (MergeTree[Index], ClusterParam)] => Boolean = _ => false
  ): Vector[(MergeTree[Index], ClusterParam)] = {
    require(instances.size > 0)

    var currentTrees = instances.zipWithIndex.map { case (instance, index) =>
      val param = getSingleInstanceParameter(index, instance)
      val loss = getInstanceLoss(instances(index), param)
      index -> ((MergeTree.Leaf(loss, index): MergeTree[Int]) -> param)
    }.toMap

    val distances = Array.ofDim[Double](instances.size, instances.size)

    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      (0 until instances.size).map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(instances, left, leftParam, right, rightParam)
          val delta = getLossChangePriority(loss, left.loss, right.loss)
          distances(leftIndex)(rightIndex) = delta
          distances(rightIndex)(leftIndex) = delta
          queues(leftIndex) += (rightIndex -> loss)
        }
    }

    while(!stoppingCondition(currentTrees) && currentTrees.size > 1) {
      val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
      val (j, newLoss) = queues(i).head

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
      val newParam = mergeParams(instances, leftTree, leftParam, rightTree, rightParam)
      currentTrees --= List(i, j) // remove i and j from current trees
      // get new merge candidates before adding new cluster back in
      val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
        val loss = mergeLoss(instances, newTree, newParam, right, rightParam)
        val delta = getLossChangePriority(loss, newTree.loss, right.loss)
        // update distances before inserting anything into queues to keep things consistent
        distances(i)(k) = delta
        distances(k)(i) = delta
        k -> loss
      }.toList
      currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
      queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
    }
    currentTrees.values.toVector
  }

  def runFullAgglomerativeClustering(
    instances: Vector[Instance]
  ): (MergeTree[Int], ClusterParam) = {
    runAgglomerativeClustering(instances).head
  }

}

// trait JointAgglomerativeClusteringAlgorithm extends AgglomerativeClusteringAlgorithm {
//   val innerAlgorithm: ClusteringAlgorithm

//   def getLossPenalty(numClusters: Int): Double

//   type Instance = Vector[innerAlgorithm.Instance]
//   type ClusterParam = Vector[(MergeTree[(Int, Int)], innerAlgorithm.ClusterParam)]

//   def runPartialInnerClustering(
//     trees: ClusterParam
//   ): ClusterParam = {
//     require(trees.size > 0)

//     val distances = Array.ofDim[Double](trees.size, trees.size)

//     var currentTrees = trees.zipWithIndex.map(_.swap).toMap

//     import scala.collection.mutable
//     def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
//     // omit the last instance index because it's covered by all of the other queues
//     val queues = mutable.Map(
//       (0 until trees.size).map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
//     )
//     currentTrees.toList.tails.toVector.foreach {
//       case Nil => ()
//       case (leftIndex, (left, leftParam)) :: rights =>
//         if(rights.isEmpty) queues -= leftIndex // remove empty queue
//         else rights.foreach { case (rightIndex, (right, rightParam)) =>
//           val loss = mergeLoss(instances, left, leftParam, right, rightParam)
//           val delta = getLossChangePriority(loss, left.loss, right.loss)
//           distances(leftIndex)(rightIndex) = delta
//           distances(rightIndex)(leftIndex) = delta
//           queues(leftIndex) += (rightIndex -> loss)
//         }
//     }

//     var nextRank = 1
//     while(!stoppingCondition(currentTrees) && currentTrees.size > 1) {
//       val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
//       val (j, newLoss) = queues(i).head

//       queues -= j // remove j from consideration
//       queues(i).clear() // clear i's queue since we'll refresh it
//       val emptyQueues = queues.iterator.flatMap { case (k, q) =>
//         q --= List(i, j)  // remove stale entries for i and j from all queues
//         if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
//       }
//       queues --= emptyQueues // and remove other now-empty queues
//       val (leftTree, leftParam) = currentTrees(i)
//       val (rightTree, rightParam) = currentTrees(j)
//       val newTree = MergeTree.Merge(nextRank, newLoss, leftTree, rightTree)
//       val newParam = mergeParams(instances, leftTree, leftParam, rightTree, rightParam)
//       currentTrees --= List(i, j) // remove i and j from current trees
//       // get new merge candidates before adding new cluster back in
//       val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
//         val loss = mergeLoss(instances, newTree, newParam, right, rightParam)
//         val delta = getLossChangePriority(loss, newTree.loss, right.loss)
//         // update distances before inserting anything into queues to keep things consistent
//         distances(i)(k) = delta
//         distances(k)(i) = delta
//         k -> loss
//       }.toList
//       currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
//       queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
//       nextRank = nextRank + 1
//     }
//     currentTrees.values.toVector
//   }

//   def getSingleInstanceParameter(
//     index: Int,
//     instance: Instance
//   ): ClusterParam = {
//     // TODO: this has an issue: indices in inner clustering may get messed up.
//     // probably we want to generalize the indexing of instances all over, to improve this situation.
//     val leaves = instance.zipWithIndex.map { case (innerInstance, innerIndex) =>
//       val param = innerAlgorithm.getSingleInstanceParameter(innerIndex, innerInstance)
//       val loss = innerAlgorithm.getInstanceLoss(innerInstance, param)
//       MergeTree.Leaf(loss, (index, innerIndex)) -> param
//     }
//     runPartialInnerClustering(leaves)
//   }

//   def getInstanceLoss(
//     instance: Instance,
//     param: ClusterParam
//   ): Double = {
//     param.map(_._1.loss).sum + getLossPenalty(param.size)
//   }

//   def mergeParams(
//     instances: Vector[Instance],
//     left: MergeTree[Int],
//     leftParam: ClusterParam,
//     right: MergeTree[Int],
//     rightParam: ClusterParam
//   ): ClusterParam = {
//     runPartialInnerClustering(leftParam ++ rightParam)
//   }

//   def mergeLoss(
//     instances: Vector[Instance],
//     left: MergeTree[Int],
//     leftParam: ClusterParam,
//     right: MergeTree[Int],
//     rightParam: ClusterParam
//   ): Double = {
//     val mergedParam = runPartialInnerClustering(
//       leftParam ++ rightParam
//     )
//     mergedParam.map(_._1.loss).sum + getLossPenalty(param.size)
//   }
// }

trait CompositeAgglomerativeClusteringAlgorithm extends AgglomerativeClusteringAlgorithm {
  val _1: AgglomerativeClusteringAlgorithm
  val _1Lambda: Double
  val _2: AgglomerativeClusteringAlgorithm
  val _2Lambda: Double
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Instance = (_1.Instance, _2.Instance)

  def getSingleInstanceParameter(
    index: Int,
    instance: Instance
  ): ClusterParam = {
    _1.getSingleInstanceParameter(index, instance._1) ->
      _2.getSingleInstanceParameter(index, instance._2)
  }

  def getInstanceLoss(
    instance: Instance,
    param: ClusterParam
  ): Double = {
    (_1Lambda * _1.getInstanceLoss(instance._1, param._1)) +
      (_2Lambda * _2.getInstanceLoss(instance._2, param._2))
  }

  def mergeParams(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): ClusterParam = {
    _1.mergeParams(instances.map(_._1), left, leftParam._1, right, rightParam._1) ->
      _2.mergeParams(instances.map(_._2), left, leftParam._2, right, rightParam._2)
  }

  def mergeLoss(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): Double = {
    (_1Lambda * _1.mergeLoss(instances.map(_._1), left, leftParam._1, right, rightParam._1)) +
      (_2Lambda * _2.mergeLoss(instances.map(_._2), left, leftParam._2, right, rightParam._2))
  }
}
