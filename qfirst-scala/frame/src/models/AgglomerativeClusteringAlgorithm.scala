package qfirst.frame.models

import qfirst.math.TarjanUnionFind
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

  // more efficient implementation! possibly MUCH more efficient for good distance functions
  // easily adaptable into an n^2 logn algorithm for single and complete linkage.
  def runPartialAgglomerativeClustering(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)],
    stoppingCondition: (Map[Int, (MergeTree[Index], ClusterParam)], Int, Int, Double) => Boolean
  ): NonEmptyVector[(MergeTree[Index], ClusterParam)] = {
    val indices = initTrees.toVector.indices
    val initNumTrees = initTrees.size.toInt

    case class MergeCandidate(
      i: Int, j: Int, delta: Double, loss: Double
    )
    object MergeCandidate {
      implicit val mergeCandidateOrdering = Ordering.by[MergeCandidate, Double](_.delta)
    }

    // Variables for algorithm:

    // current clusters: full dendrograms/trees
    var currentTrees = initTrees.zipWithIndex.map(_.swap).toVector.toMap

    // queue of candidate merges in priority order
    var orderedMerges = currentTrees.toList.tails.flatMap {
      case Nil => Nil
      case (leftIndex, (left, leftParam)) :: rights =>
        rights.iterator.map { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(left, leftParam, right, rightParam)
          val delta = getLossChangePriority(loss, left.loss, right.loss)
          MergeCandidate(leftIndex, rightIndex, delta, loss)
        }
    }.toList.sorted

    // counter for number of stale merge candidates remaining for each pair.
    // CAVEAT: a value of 0 means there IS an ACTIVE merge candidate in the queue,
    // at least for remaining values.
    val staleCounts = Array.ofDim[Int](initNumTrees, initNumTrees)

    // union-find structure to identify cluster representatives
    val clusterSets = TarjanUnionFind.empty[Int]
    indices.foreach(clusterSets.add)

    var done: Boolean = false
    while(!done && currentTrees.size > 1) {
      orderedMerges match {
        case Nil => ??? // should not happen; or, can use this instead of currentTrees.size > 1 condition above?
        case MergeCandidate(origI, origJ, delta, newLoss) :: rest =>
          val i = clusterSets.find(origI).get
          val j = clusterSets.find(origJ).get
          // if(i == j) { // already merged implicitly; skip
          //   orderedMerges = rest
          // } else
          require(i != j)
          staleCounts(i)(j) match {
            case 0 => // merge
              done = stoppingCondition(currentTrees, i, j, newLoss)
              if(!done) {
                // merge indices in disjoint sets repr, and update staleness counts
                val newRep = clusterSets.union(i, j).get
                val oldRep = if(newRep == i) j else i
                require(Set(newRep, oldRep) == Set(i, j))
                indices.foreach { k =>
                  // three cases:
                  // 1) k == newRep. then we're on the diagonal; it's meaningless.
                  // 2) k == oldRep. then we're on the just-now-stale 0 we took to get here. meaningless.
                  // 3) k is neither. then we're doing the right thing.
                  // TODO could use a running set of active indices so this update gets shorter each time.
                  val newStaleCounts = {
                    // if there are 0 stales, that means there is an active one,
                    // which is now becoming stale.
                    // otherwise, if there are >0 stales, then there must be none active,
                    // so we only count the stales.
                    math.max(1, staleCounts(newRep)(k)) +
                      math.max(1, staleCounts(oldRep)(k))
                  }
                  staleCounts(newRep)(k) = newStaleCounts
                  staleCounts(k)(newRep) = newStaleCounts
                }
                // perform the actual merge
                val (leftTree, leftParam) = currentTrees(i)
                val (rightTree, rightParam) = currentTrees(j)
                val newTree = MergeTree.Merge(newLoss, leftTree, rightTree)
                sanityCheck(newLoss, leftTree, leftParam, rightTree, rightParam)
                val newParam = mergeParams(leftTree, leftParam, rightTree, rightParam)
                // update the current set of trees
                currentTrees --= List(i, j) // remove i and j from current trees
                currentTrees += (newRep -> (newTree -> newParam))
                orderedMerges = rest
              }
            case 1 => // insert a new merge into the queue
              staleCounts(i)(j) -= 1
              staleCounts(j)(i) -= 1
              // now there are no stale precursor merges left, so this merge can happen
              val (leftTree, leftParam) = currentTrees(i)
              val (rightTree, rightParam) = currentTrees(j)
              val loss = mergeLoss(leftTree, leftParam, rightTree, rightParam)
              val delta = getLossChangePriority(loss, leftTree.loss, rightTree.loss)
              val candidate = MergeCandidate(i, j, delta, loss)
              // insert it into the appropriate location in the sequence
              orderedMerges = {
                val (prefix, suffix) = rest.span(_.delta < delta)
                prefix ++ (candidate :: suffix)
              }
            case _ => // skip
              staleCounts(i)(j) -= 1
              staleCounts(j)(i) -= 1
              orderedMerges = rest
          }
      }
    }
    // guaranteed to have at least 1 elt from clustering process
    NonEmptyVector.fromVector(currentTrees.values.toVector).get
  }

  // less efficient, traditional n^3 implementation.
  def runPartialAgglomerativeClustering_N3(
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
        sanityCheck(newLoss, leftTree, leftParam, rightTree, rightParam)
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

  def finishAgglomerativeClustering(
    initTrees: NonEmptyVector[(MergeTree[Index], ClusterParam)]
  ): (MergeTree[Index], ClusterParam) = {
    runPartialAgglomerativeClustering(
      initTrees,
      (_, _, _, _) => false
    ).head
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

  def sanityCheck(
    loss: Double,
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam) = {
    if(!(loss >= left.loss && loss >= right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(leftParam)
      // println(aggregateLosses(indices.map(getInstanceLoss(_, leftParam))))
      println(right)
      println(rightParam)
      // println(aggregateLosses(indices.map(getInstanceLoss(_, rightParam))))
      println(loss)
      ???
    }
  }
}
