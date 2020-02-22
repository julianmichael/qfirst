package qfirst.frame.models
import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Multinomial

import scala.collection.immutable.Vector

class VectorMeanClustering[I](
  getInstance: I => DenseVector[Float]
) extends ClusteringAlgorithm {
  // cluster means keep track of size too so they can be added and for loss calculation
  case class ClusterParam(mean: DenseVector[Float], size: Double)
  type Index = I

  // k-means loss: sum of square distances from mean
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    val displacement = getInstance(index) - param.mean
    (displacement dot displacement).toDouble / 175.0 // adjust so interpolation is reasonable
  }

  override def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = ClusterParam(getInstance(index), 1)

  // just take the mean of all of the elements in the cluster (in expectation)
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val size = assignmentProbabilities.sum
    val mean = DenseVector.zeros[Float](getInstance(indices.head).length)
    indices.iterator.zip(assignmentProbabilities.iterator)
      .foreach { case (index, prob) =>
        mean :+= (getInstance(index) *:* prob.toFloat)
      }
    mean :/= size.toFloat
    ClusterParam(mean, size)
  }

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    val size = indices.size
    val mean = DenseVector.zeros[Float](getInstance(indices.head).length)
    indices.iterator.foreach { index =>
        mean :+= getInstance(index)
      }
    mean :/= size.toFloat
    ClusterParam(mean, size)
  }

  // can efficiently merge by weighing each mean by its cluster size
  override def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val size = leftParam.size + rightParam.size
    val mean = (leftParam.mean *:* (leftParam.size.toFloat / size).toFloat) +
      (rightParam.mean *:* (rightParam.size / size).toFloat)
    ClusterParam(mean, size)
  }

  // can efficiently merge by weighing each mean by its cluster size
  override def mergeLoss(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    val param = mergeParams(left, leftParam, right, rightParam)
    val newLoss = (left.values ++ right.values)
      .foldMap(i => getInstanceLoss(i, param))
    newLoss
  }
}
