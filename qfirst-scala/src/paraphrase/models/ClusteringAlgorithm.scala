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

// object MeanClustering extends ClusteringAlgorithm {
//   type ClusterParam = Vector[Double]
//   type Instance = Vector[Double]
//   type Hyperparams = Unit

//   // for latent-variable clustering
//   def computeLosses(
//     instances: Vector[Instance],
//     model: Vector[ClusterParam]
//   ): Vector[Logits] = { // each elt is the set of (nonnegative) losses for an instance
//     instances.map
//   }

//   // for agglomerative clustering
//   def distance(
//     p1: ClusterParam,
//     p2: ClusterParam
//   ): Double

//   // for both latent-variable and agglomerative clustering
//   def estimateParameter(
//     instances: Vector[Instance],
//     assignmentProbabilities: Vector[DenseMultinomial],
//     hyperparams: Hyperparams
//   ): ClusterParam
// }

trait ClusteringAlgorithm {
  type ClusterParam
  type Instance
  type Hyperparams

  // for latent-variable clustering
  def computeLosses(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): Vector[DenseVector[Double]] // each elt is the set of (nonnegative) losses for an instance

  // for agglomerative clustering
  def distance(
    p1: ClusterParam,
    p2: ClusterParam
  ): Double

  // for both latent-variable and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[DenseMultinomial],
    hyperparams: Hyperparams
  ): ClusterParam

  // TODO: k-means|| init as well, and maybe random init?

  def initPlusPlus(
    instances: Vector[Instance],
    hyperparams: Hyperparams,
    numClusters: Int
  ): Vector[ClusterParam] = {
    val rand = new scala.util.Random()
    assert(numClusters >= 1)
    val firstCluster = estimateParameter(
      Vector(instances(rand.nextInt(instances.size))), Vector(Multinomial(DenseVector(1.0))), hyperparams
    )
    val uniqueInstances = instances.groupBy(x => x).keys.toVector
    val initMinLosses = computeLosses(uniqueInstances, Vector(firstCluster)).head
    initPlusPlusAux(uniqueInstances, hyperparams, Vector(firstCluster), numClusters - 1, initMinLosses)
  }

  private[this] def initPlusPlusAux(
    instances: Vector[Instance], hyperparams: Hyperparams,
    curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double]
  ): Vector[ClusterParam] = {
    if(numClustersLeft <= 0) curParams else {
      val newCenterProbs = Multinomial(curMinLosses)
      val newCenterIndex = newCenterProbs.draw
      val newCluster = estimateParameter(Vector(instances(newCenterIndex)), Vector(Multinomial(DenseVector(1.0))), hyperparams)
      val clusterLosses = computeLosses(instances, Vector(newCluster)).head
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(instances.take(newCenterIndex) ++ instances.drop(newCenterIndex + 1), hyperparams, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
    }
  }

  // TODO:
  // hard EM, soft EM, hierarchical agglomerative clustering

}
