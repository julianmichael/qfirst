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

object VectorMeanClustering extends ClusteringAlgorithm {
  // cluster means keep track of size too so they can be added and for loss calculation
  case class ClusterParam(mean: DenseVector[Float], size: Double)
  type Instance = DenseVector[Float]

  // k-means loss: sum of square distances from mean
  def getInstanceLoss(
    instance: Instance,
    param: ClusterParam
  ): Double = {
    val displacement = instance - param.mean
    (displacement dot displacement).toDouble / 175.0 // adjust so interpolation is reasonable
  }

  override def getSingleInstanceParameter(
    index: Int,
    instance: Instance
  ): ClusterParam = ClusterParam(instance, 1)

  // just take the mean of all of the elements in the cluster (in expectation)
  def estimateParameterSoft(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double]
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

  override def estimateParameterHard(
    instances: Vector[Instance]
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
  override def mergeParams(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): ClusterParam = {
    val size = leftParam.size + rightParam.size
    val mean = (leftParam.mean *:* (leftParam.size.toFloat / size).toFloat) +
      (rightParam.mean *:* (rightParam.size / size).toFloat)
    ClusterParam(mean, size)
  }

  // can efficiently merge by weighing each mean by its cluster size
  override def mergeLoss(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): Double = {
    val param = mergeParams(instances, left, leftParam, right, rightParam)
    val newLoss = (left.values ++ right.values)
      .foldMap(i => getInstanceLoss(instances(i), param))
    newLoss
  }
}
