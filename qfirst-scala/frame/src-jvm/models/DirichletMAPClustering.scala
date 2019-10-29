package qfirst.frame.models

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Multinomial

import scala.collection.immutable.Vector

object DirichletMAPClustering extends ClusteringAlgorithm {
  type ClusterParam = DenseMultinomial
  type Instance = Map[Int, Int]
  case class Hyperparams(
    vocabSize: Int,
    clusterConcentrationParameter: Double
  )

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    instance.iterator.map { case (item, count) =>
      math.log(param.probabilityOf(item)) * count
    }.sum * -1
  }

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
