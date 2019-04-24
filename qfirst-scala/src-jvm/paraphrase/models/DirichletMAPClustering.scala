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

// TODO properly use the available distributions stuff in Breeze
object DirichletMAPClustering extends ClusteringAlgorithm {
  type ClusterParam = DenseMultinomial
  type Instance = Map[Int, Int]
  case class Hyperparams(
    vocabSize: Int,
    clusterConcentrationParameter: Double
  )

  // for latent-variable clustering
  def computeLosses(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): Vector[Vector[Double]] = {// each elt is the set of (nonnegative) losses for an instance
    instances.map { instance =>
      model.zipWithIndex.map { case (dist, clusterIndex) =>
        instance.iterator.map { case (item, count) =>
          math.log(dist.probabilityOf(item)) * count
        }.sum * -1
      }
    }
  }

  // for agglomerative clustering
  def distance(
    p1: ClusterParam,
    p2: ClusterParam
  ): Double = {
    // TODO jensen-shannon divergence
    0.0
  }

  // for both latent-variable and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val pseudoCounts = instances.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .map { case (instance, prob) =>
        instance.transform { case (_, c) => prob * c } // expected counts
      }.foldLeft(Map.empty[Int, Double])(_ |+| _)
    dirichletPosteriorFromSparseNew(
      pseudoCounts, hyperparams.vocabSize, hyperparams.clusterConcentrationParameter
    )
  }
}
