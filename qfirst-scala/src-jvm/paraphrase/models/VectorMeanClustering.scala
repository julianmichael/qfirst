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

object VectorMeanClustering extends ClusteringAlgorithm {
  // cluster means keep track of size too so they can be added and for loss calculation
  case class ClusterParam(mean: DenseVector[Float], size: Double)
  type Instance = DenseVector[Float]
  type Hyperparams = Unit

  // k-means loss: sum of square distances from mean
  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    val displacement = instance - param.mean
    (displacement dot displacement).toDouble / 175.0 // adjust so interpolation is reasonable
  }

  // just take the mean of all of the elements in the cluster (in expectation)
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val size = assignmentProbabilities.sum
    val mean = DenseVector.zeros[Float](instances.head.length)
    instances.iterator.zip(assignmentProbabilities.iterator)
      .foreach { case (instance, prob) =>
        mean :+= (instance *:* prob.toFloat)
      }
    mean :/= size.toFloat
    ClusterParam(mean, size)
  }

  override def getSingleInstanceParameter(
    instance: Instance,
    hyperparams: Hyperparams
  ): ClusterParam = ClusterParam(instance, 1)

  override def estimateParameterHard(
    instances: Vector[Instance],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val size = instances.size
    val mean = DenseVector.zeros[Float](instances.head.length)
    instances.iterator.foreach { instance =>
        mean :+= instance
      }
    mean :/= size.toFloat
    ClusterParam(mean, size)
  }


  // can efficiently merge by weighing each mean by its cluster size
  override def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams,
    sanityCheck: Boolean = true
  ): MergeCandidate = {
    val size = leftParam.size + rightParam.size
    val mean = (leftParam.mean *:* (leftParam.size.toFloat / size).toFloat) +
      (rightParam.mean *:* (rightParam.size / size).toFloat)
    val param = ClusterParam(mean, size)
    val newLoss = (left.values ++ right.values)
      .foldMap(i => computeLoss(instances(i), param, hyperparams))
    if(sanityCheck && !(newLoss > left.loss && newLoss > right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println("===== LEFT ===== : " + left)
      println("== LEFT PARAM == : " + leftParam)
      println("==== RIGHT ===== : " + right)
      println("= RIGHT PARAM == : " + rightParam)
      println("==== PARAM ===== : " + param)
      println("===== LOSS ===== : " + newLoss)
      ???
    }
    MergeCandidate(left, right, param, newLoss)
  }
}
