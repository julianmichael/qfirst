package qfirst.paraphrase.models

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
  type ClusterParam = DenseVector[Float]
  type Instance = DenseVector[Float]
  type Hyperparams = Unit

  // for latent-variable clustering
  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    val displacement = instance - param
    (displacement dot displacement).toDouble
  }

  // for both latent-variable and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    instances.iterator
      .zip(assignmentProbabilities.iterator)
      .map { case (instance, prob) =>
        instance *:* prob.toFloat
      }.reduce(_ + _)
  }
}
