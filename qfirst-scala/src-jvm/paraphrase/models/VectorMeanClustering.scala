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
  def computeLosses(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): Vector[Vector[Double]] = { // each elt is the set of (nonnegative) losses for an instance
    instances.map(instance =>
      model.map { clusterMean =>
        val displacement = clusterMean - instance
        (displacement dot displacement).toDouble
      }
    )
  }

  // for agglomerative clustering
  def distance(
    p1: ClusterParam,
    p2: ClusterParam
  ): Double = {
    val displacement = p1 - p2
    displacement dot displacement
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
