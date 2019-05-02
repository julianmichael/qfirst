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
  case class ClusterParam(mean: DenseVector[Float], size: Double)
  type Instance = DenseVector[Float]
  type Hyperparams = Unit

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    val displacement = instance - param.mean
    (displacement dot displacement).toDouble
  }

  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val mean = instances.iterator
      .zip(assignmentProbabilities.iterator)
      .map { case (instance, prob) =>
        instance *:* prob.toFloat
      }.reduce(_ + _)
    val size = assignmentProbabilities.sum
    ClusterParam(mean, size)
  }

  override def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): MergeCandidate = {
    val size = leftParam.size + rightParam.size
    val mean = (leftParam.mean *:* (leftParam.size / size).toFloat) +
      (rightParam.mean *:* (rightParam.size / size).toFloat)
    val param = ClusterParam(mean, size)
    val newLoss = instances.foldMap(computeLoss(_, param, hyperparams))
    if(!(newLoss > left.loss && newLoss > right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(right)
      println(newLoss)
      ???
    }
    MergeCandidate(left, right, param, newLoss)
  }
}
