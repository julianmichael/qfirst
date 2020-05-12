package qfirst.frame.models
import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._

import scala.collection.immutable.Vector

// same as DirichletMAPClustering but only does MLE (no smoothing) and agglomeration
// can also be regarded as "MLE clustering"
object MinEntropyClustering {
  case class ClusterMixture(counts: DenseVector[Double], total: Double)
}
// TODO maybe sparse vectors ... or dense, since they need to be constructed for cluster params anyway? or maybe that would just use way too much memory
class MinEntropyClustering[I](
  getInstance: I => Map[Int, Double],
  vocabSize: Int
) extends ClusteringAlgorithm {
  import MinEntropyClustering._
  type ClusterParam = ClusterMixture
  type Index = I

  // loss is entropy * num elements (same as likelihood under MLE in our case)
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    getInstance(index).iterator.map { case (item, count) =>
      math.log(param.counts(item) / param.total) * count
    }.sum * -1.0
  }

  // just get MLE by counting
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val arr = new Array[Double](vocabSize)
    var total = 0.0
    var numInstances = 0.0
    indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (index, prob) =>
        numInstances = numInstances + prob
        getInstance(index).foreach { case (index, count) =>
          val pcount = prob * count
          arr(index) += pcount
          total = total + pcount
        }
      }
    ClusterMixture(DenseVector(arr), total)
  }

  // can do efficient merge by summing counts
  override def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val counts = leftParam.counts + rightParam.counts
    val total = leftParam.total + rightParam.total
    ClusterMixture(counts, total)
  }

  override def mergeLoss(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    val param = mergeParams(left, leftParam, right, rightParam)
    val newLoss = param.counts.activeValuesIterator
      .filter(_ > 0.0) // prevent log of 0
      .map(c => c * log(c / param.total)) // count * log probability = log likelihood
      .sum * -1.0
    newLoss
  }

}
