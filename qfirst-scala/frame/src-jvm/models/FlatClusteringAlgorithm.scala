package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector

import breeze.stats.distributions.Multinomial
import breeze.linalg._
import breeze.numerics._

trait FlatClusteringAlgorithm {
  type ClusterParam
  type Index

  // override for efficiency
  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    estimateParameterHard(Vector(index))
  }

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double],
  ): ClusterParam

  // override for efficiency
  def estimateParameterHard(
    indices: Vector[Index],
  ): ClusterParam = {
    estimateParameterSoft(indices, indices.as(1.0))
  }

  // // override eg for max
  // def aggregateLosses(
  //   losses: Vector[Double]
  // ): Double = losses.sum

  // // override eg for max
  // def getLossChangePriority(
  //   newLoss: Double,
  //   leftLoss: Double,
  //   rightLoss: Double
  // ) = newLoss - leftLoss - rightLoss

  // TODO: k-means|| init as well, and maybe random init?
  // TODO: perhaps make matrices out of getInstanceLoss for more efficiency

  def initPlusPlus(
    indices: Vector[Index],
    numClusters: Int
  ): Vector[ClusterParam] = {
    val rand = new scala.util.Random()
    assert(numClusters >= 1)
    val firstClusterIndex = rand.nextInt(indices.size)
    val firstCluster = getSingleInstanceParameter(indices(firstClusterIndex))
    // TODO: maybe not do unique instances? maybe take them as an input? idk
    val uniqueInstances = indices.groupBy(x => x).keys.toVector
    val initMinLosses = DenseVector(uniqueInstances.map(getInstanceLoss(_, firstCluster)).toArray)

    initPlusPlusAux(uniqueInstances, Set(), Vector(firstCluster), numClusters - 1, initMinLosses)
  }

  private[this] def initPlusPlusAux(
    indices: Vector[Index],
    chosenInstances: Set[Index], curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double]
  ): Vector[ClusterParam] = {
    if(numClustersLeft <= 0) curParams else {
      // println(s"cur min losses: $curMinLosses")
      // instances.foreach(i => println(i.toString.take(200)))
      val newCenterProbs = Multinomial(curMinLosses)
      val newCenterIndex = {
        var newIndex = indices(newCenterProbs.draw)
        while(chosenInstances.contains(newIndex)) {
          newIndex = indices(newCenterProbs.draw)
        }
        newIndex
      }
      val newCluster = getSingleInstanceParameter(newCenterIndex)
      val clusterLosses = DenseVector(indices.map(getInstanceLoss(_, newCluster)).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(indices, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
    }
  }

  def softEStep(
    indices: Vector[Index],
    model: Vector[ClusterParam]
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val allLosses = indices.map(i => model.map(p => getInstanceLoss(i, p)))
    val assignments = allLosses.map { v =>
      val vec = DenseVector(v.toArray) *:* -1.0 // from unnormalized neg log likelihoods to unnorm log likelihoods
      val probs = exp(vec - logSumExp(vec)) // normalized likelihoods
      Multinomial(probs) // assume uniform prior -> likelihoods are assignment probabilities
    }
    val instanceLosses = assignments.map(_.sum)
    (assignments, instanceLosses)
  }

  def softMStep(
    numClusters: Int,
    indices: Vector[Index],
    assignments: Vector[DenseMultinomial]
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      val clusterProbs = assignments.map(_.probabilityOf(clusterIndex))
      estimateParameterSoft(indices, clusterProbs)
    }
  }

  def runSoftEM(
    initModel: Vector[ClusterParam],
    indices: Vector[Index],
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
    var (assignments, stepLosses) = softEStep(indices, initModel)
    var losses: List[Double] = List(mean(stepLosses))
    var model: Vector[ClusterParam] = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.size, indices, assignments)
      val p = softEStep(indices, model)
      assignments = p._1
      stepLosses = p._2
      val loss = mean(stepLosses)
      losses = loss :: losses
      if(shouldLog) {
        println("=== Stepping ===")
        val prior = assignments.map(a => a.params / a.sum).reduce(_ + _) / assignments.size.toDouble
        println(s"Prior: " + prior.toScalaVector.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
        println(s"Loss: $loss")
      }
    }
    if(shouldLog) {
      println("=== Stopped ===")
    }
    (model, assignments, losses.head)
  }
}
