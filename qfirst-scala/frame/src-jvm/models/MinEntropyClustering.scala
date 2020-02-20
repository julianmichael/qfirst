package qfirst.frame.models
import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Multinomial

import scala.collection.immutable.Vector

// same as DirichletMAPClustering but only does MLE (no smoothing) and agglomeration
// can also be regarded as "MLE clustering"
class MinEntropyClustering(
  vocabSize: Int
) extends ClusteringAlgorithm {
  case class ClusterParam(counts: DenseVector[Double], total: Double)
  type Instance = Map[Int, Double] // sparse soft counts

  // loss is entropy * num elements (same as likelihood under MLE in our case)
  def getInstanceLoss(
    instance: Instance,
    param: ClusterParam
  ): Double = {
    instance.iterator.map { case (item, count) =>
      math.log(param.counts(item) / param.total) * count
    }.sum * -1.0
  }

  // just get MLE by counting
  def estimateParameterSoft(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val arr = new Array[Double](vocabSize)
    var total = 0.0
    var numInstances = 0.0
    instances.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (instance, prob) =>
        numInstances = numInstances + prob
        instance.foreach { case (index, count) =>
          val pcount = prob * count
          arr(index) += pcount
          total = total + pcount
        }
      }
    ClusterParam(DenseVector(arr), total)
  }

  // can do efficient merge by summing counts
  override def mergeParams(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): ClusterParam = {
    val counts = leftParam.counts + rightParam.counts
    val total = leftParam.total + rightParam.total
    ClusterParam(counts, total)
  }

  override def mergeLoss(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): Double = {
    val param = mergeParams(instances, left, leftParam, right, rightParam)
    val newLoss = param.counts.activeValuesIterator
      .filter(_ > 0.0) // prevent log of 0
      .map(c => c * log(c / param.total)) // count * log probability = log likelihood
      .sum * -1.0
    newLoss
  }

}
