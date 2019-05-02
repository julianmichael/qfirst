package qfirst.paraphrase.models
import qfirst._

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Multinomial

import scala.collection.immutable.Vector

object CompleteLinkageClustering extends ClusteringAlgorithm {
  type ClusterParam = Set[Int] // DenseVector[Double] // boolean sets
  type Instance = Int // DenseVector[Double] // fuzzy eq neg log probs
  type Hyperparams = DenseMatrix[Double] // fuzzy eq neg log probs
  // case class Hyperparams(vocabSize: Int)

  // dummy impl, though actually kinda works I guess?
  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    param.map(i => hyperparams(i, instance)).max
  }

  // to make agglom work. assumes all instances included in cluster
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    instances.toSet
  }

  override def aggregateLosses(
    losses: Vector[Double]
  ): Double = losses.max

  override def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    rightLoss: Double
  ) = List(newLoss, leftLoss, rightLoss).max // though newLoss should always be biggest

  override def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): MergeCandidate = {
    val leftValues = left.values
    val rightValues = right.values
    val param = (leftValues ++ rightValues).toSet
    val newLoss = leftValues.flatMap { lv =>
      rightValues.map { rv =>
        hyperparams(lv, rv)
      }
    }.max
    if(!(newLoss > left.loss && newLoss > right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(right)
      println(newLoss)
      ???
    }
    val loss = List(newLoss, left.loss, right.loss).max
    MergeCandidate(left, right, param, loss)
  }
}
