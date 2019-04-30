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

// same as DirichletMAPClustering but only does MLE (no smoothing) and agglomeration
// can also be regarded as "MLE clustering"
object MinEntropyClustering extends ClusteringAlgorithm {
  type ClusterParam = DenseVector[Double] // pseudocounts
  type Instance = Map[Int, Int] // sparse counts
  case class Hyperparams(vocabSize: Int)

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    ??? // TODO
  }

  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    val arr = new Array[Double](hyperparams.vocabSize)
    instances.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (instance, prob) =>
        instance.foreach { case (index, count) =>
          arr(index) += prob * count
        }
      }
    DenseVector(arr)
  }
}
