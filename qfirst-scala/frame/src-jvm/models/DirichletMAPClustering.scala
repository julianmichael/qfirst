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

class DirichletMAPClustering(
  instances: Vector[Map[Int, Int]],
  vocabSize: Int,
  clusterConcentrationParameter: Double
) extends ClusteringAlgorithm {

  type Index = Int
  type ClusterParam = DenseMultinomial

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    instances(index).iterator.map { case (item, count) =>
      math.log(param.probabilityOf(item)) * count
    }.sum * -1
  }

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val pseudoCounts = indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .map { case (index, prob) =>
        instances(index).transform { case (_, c) => prob * c } // expected counts
      }.foldLeft(Map.empty[Int, Double])(_ |+| _)
    dirichletPosteriorFromSparseNew(
      pseudoCounts, vocabSize, clusterConcentrationParameter
    )
  }
}
