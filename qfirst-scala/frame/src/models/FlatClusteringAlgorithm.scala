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
  type Instance

  // override for efficiency
  def getSingleInstanceParameter(
    instance: Instance
  ): ClusterParam = {
    estimateParameterHard(Vector(instance))
  }

  def getInstanceLoss(
    instance: Instance,
    param: ClusterParam
  ): Double

  def estimateParameterSoft(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
  ): ClusterParam

  // override for efficiency
  def estimateParameterHard(
    instances: Vector[Instance],
  ): ClusterParam = {
    estimateParameterSoft(instances, instances.as(1.0))
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
    instances: Vector[Instance],
    numClusters: Int
  ): Vector[ClusterParam] = {
    val rand = new scala.util.Random()
    assert(numClusters >= 1)
    val firstCluster = getSingleInstanceParameter(
      instances(rand.nextInt(instances.size))
    )
    val uniqueInstances = instances.groupBy(x => x).keys.toVector
    val initMinLosses = DenseVector(uniqueInstances.map(getInstanceLoss(_, firstCluster)).toArray)

    initPlusPlusAux(uniqueInstances, Set(), Vector(firstCluster), numClusters - 1, initMinLosses)
  }

  private[this] def initPlusPlusAux(
    instances: Vector[Instance],
    chosenInstances: Set[Int], curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double]
  ): Vector[ClusterParam] = {
    if(numClustersLeft <= 0) curParams else {
      // println(s"cur min losses: $curMinLosses")
      // instances.foreach(i => println(i.toString.take(200)))
      val newCenterProbs = Multinomial(curMinLosses)
      val newCenterIndex = {
        var newIndex = newCenterProbs.draw
        while(chosenInstances.contains(newIndex)) {
          newIndex = newCenterProbs.draw
        }
        newIndex
      }
      val newCluster = getSingleInstanceParameter(instances(newCenterIndex))
      val clusterLosses = DenseVector(instances.map(getInstanceLoss(_, newCluster)).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(instances, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
    }
  }

  def softEStep(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val allLosses = instances.map(i => model.map(p => getInstanceLoss(i, p)))
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
    instances: Vector[Instance],
    assignments: Vector[DenseMultinomial]
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      val clusterProbs = assignments.map(_.probabilityOf(clusterIndex))
      estimateParameterSoft(instances, clusterProbs)
    }
  }

  def runSoftEM(
    initModel: Vector[ClusterParam],
    instances: Vector[Instance],
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
    var (assignments, stepLosses) = softEStep(instances, initModel)
    var losses: List[Double] = List(mean(stepLosses))
    var model: Vector[ClusterParam] = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.size, instances, assignments)
      val p = softEStep(instances, model)
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
