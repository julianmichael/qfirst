package qfirst.frame.models
import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._

import scala.collection.immutable.Vector

object MixtureOfStatesClustering {
  // dot product of obs NLLs with a prior will yield a loss
  case class StateInstance(
    state: Int, obsNLLs: DenseVector[Double]
  )
  case class StateCounts(counts: DenseVector[Double], total: Double)
}

// same as DirichletMAPClustering but only does MLE (no smoothing) and agglomeration
// can also be regarded as "MLE clustering"
class MixtureOfStatesClustering[I](
  getInstance: I => MixtureOfStatesClustering.StateInstance,
  vocabSize: Int
) extends ClusteringAlgorithm {
  import MixtureOfStatesClustering.{StateInstance,StateCounts}
  type Index = I
  type ClusterParam = StateCounts

  // loss is NLL under cluster dist
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    val instance = getInstance(index)
    param.counts.dot(instance.obsNLLs) / param.total
  }

  // just get MLE by counting
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val arr = new Array[Double](vocabSize)
    var total = 0.0
    var numInstances = 0.0
    indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (index, prob) =>
        numInstances = numInstances + prob
        val stateIdx = getInstance(index).state
        arr(stateIdx) += prob
        total = total + prob
      }
    StateCounts(DenseVector(arr), total)
  }

  // can do efficient merge by summing counts
  override def mergeParams(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val counts = leftParam.counts + rightParam.counts
    val total = leftParam.total + rightParam.total
    StateCounts(counts, total)
  }
}
