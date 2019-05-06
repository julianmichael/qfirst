package qfirst.paraphrase.models
import qfirst.MergeTree

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
object MinEntropyClustering extends ClusteringAlgorithm {
  case class ClusterParam(counts: DenseVector[Double], total: Double)
  type Instance = Map[Int, Int] // sparse counts
  case class Hyperparams(vocabSize: Int)

  // loss is entropy * num elements (same as likelihood under MLE in our case)
  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    instance.iterator.map { case (item, count) =>
      math.log(param.counts(item) / param.total) * count
    }.sum * -1.0
  }

  // just get MLE by counting
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val arr = new Array[Double](hyperparams.vocabSize)
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
  override def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): MergeCandidate = {
    val counts = leftParam.counts + rightParam.counts
    val total = leftParam.total + rightParam.total
    val newLoss = counts.activeValuesIterator
      .filter(_ > 0.0) // prevent log of 0
      .map(c => c * log(c / total)) // count * log probability = log likelihood
      .sum * -1.0
    if(!(newLoss >= left.loss && newLoss >= right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(instances)
      println(left)
      println(leftParam)
      println(right)
      println(rightParam)
      println(counts)
      println(total)
      println(newLoss)
      ???
    }
    MergeCandidate(left, right, ClusterParam(counts, total), newLoss)
  }

}
