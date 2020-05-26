package qfirst.frame.models

import qfirst.frame.MergeTree
import qfirst.frame.logLevel

import jjm.implicits._

import cats.Foldable
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector
import scala.annotation.tailrec

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
    val firstClusterIndex = rand.nextInt(indices.size.toInt)
    val firstCluster = getSingleInstanceParameter(indices(firstClusterIndex))
    // TODO: maybe not do unique instances? maybe take them as an input? idk
    val uniqueInstances = indices.toVector.groupBy(x => x).keys.toVector
    val initMinLosses = DenseVector(uniqueInstances.map(getInstanceLoss(_, firstCluster)).toArray)

    val (params, minLosses) = initPlusPlusAux(uniqueInstances, Set(), Vector(firstCluster), numClusters - 1, initMinLosses, rand)
    if(minLosses.activeValuesIterator.exists(v => v.isNaN || v.isInfinite)) {
      System.err.println(s"Warning: NaN/Infinite loss items remain after initializing $numClusters clusters for ${indices.size} elements")
      System.err.println(minLosses.activeValuesIterator.map(x => f"$x%.2f").mkString(","))
    }
    params
  }

  @tailrec
  private[this] def initPlusPlusAux(
    indices: Vector[Index],
    chosenInstances: Set[Index], curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double],
    rand: scala.util.Random
  ): (Vector[ClusterParam], DenseVector[Double]) = {
    if(numClustersLeft <= 0) (curParams -> curMinLosses) else {
      val infiniteLossIndexIndices = curMinLosses.activeIterator
        .collect { case (index, minLoss) if minLoss.isPosInfinity => index }
        .toVector
      // already-picked instances shouldn't have infinite loss
      require(infiniteLossIndexIndices.forall(i => !chosenInstances.contains(indices(i))))
      val newCenterIndex = if(infiniteLossIndexIndices.nonEmpty) {
        indices(infiniteLossIndexIndices(rand.nextInt(infiniteLossIndexIndices.size)))
      } else {
        val newCenterProbs = Multinomial(curMinLosses)
        var newIndex = indices(newCenterProbs.draw)
        while(chosenInstances.contains(newIndex)) {
          newIndex = indices(newCenterProbs.draw)
        }
        newIndex
      }
      // System.errprintln(s"cur min losses: $curMinLosses")
      // instances.foreach(i => println(i.toString.take(200)))
      val newCluster = getSingleInstanceParameter(newCenterIndex)
      val clusterLosses = DenseVector(indices.map(getInstanceLoss(_, newCluster)).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(indices, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses, rand)
    }
  }

  def softEStep(
    indices: Vector[Index],
    model: Vector[ClusterParam],
    temperature: Double = 1.0
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val allLosses = indices.map(i => model.map(p => getInstanceLoss(i, p)))
    val assignments = allLosses.map { v =>
      val vec = DenseVector(v.toArray) *:* (-1.0 / temperature) // from unnormalized neg log likelihoods to unnorm log likelihoods
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

  import cats.effect.IO
  import cats.Id
  import freelog.EphemeralTreeLogger


  // def runSoftEMPure(
  //   initModel: Vector[ClusterParam],
  //   indices: Vector[Index],
  //   stoppingThreshold: Double,
  //   temperatureSchedule: Int => Double = (n: Int) => 1.0)(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[(Vector[ClusterParam], Vector[DenseMultinomial], Double)] = IO {
  //   val (initAssignments, initStepLosses) = softEStep(indices, initModel, temperatureSchedule(0))
  //   for {
  //     assignmentsAndLosses <- Ref[IO].of(initAssignments -> initStepLosses)
  //     losses <- Ref[IO].of(List(mean(initStepLosses)))
  //     model <- Ref[IO].of(initModel)
  //     stepNum <- Ref[IO].of(1: Int)
  //     shouldContinue = losses.get >>= {
  //       case last :: secondLast :: _ => (secondLast - last) > stoppingThreshold
  //       case _ => true
  //     }
  //     _ <- shouldContinue.whileM_ {
  //       for {
  //         curAssignments <- assignmentsAndLosses.get.map(_._1)
  //         curModel <- model.updateAndGet(softMStep)
  //       }
  //     }
  //   }
  //   var (assignments, stepLosses) = softEStep(indices, initModel, temperatureSchedule(0))
  //   var losses: List[Double] = List(mean(stepLosses))
  //   var model: Vector[ClusterParam] = initModel
  //   var stepNum = 1
  //   def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
  //   def shouldContinue = getDelta.forall(_ > stoppingThreshold)
  //   while(shouldContinue) {
  //     model = softMStep(model.size, indices, assignments)
  //     val p = softEStep(indices, model, temperatureSchedule(stepNum))
  //     assignments = p._1
  //     stepLosses = p._2
  //     val loss = mean(stepLosses)
  //     losses = loss :: losses
  //     stepNum = stepNum + 1
  //     if(shouldLog) {
  //       println("=== Stepping ===")
  //       val prior = assignments.map(a => a.params / a.sum).reduce(_ + _) / assignments.size.toDouble
  //       println(s"Prior: " + prior.toScalaVector.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
  //       println(s"Loss: $loss")
  //     }
  //   }
  //   if(shouldLog) {
  //     println("=== Stopped ===")
  //   }
  //   (model, assignments, losses.head)
  // }

  def runSoftEM(
    initModel: Vector[ClusterParam],
    indices: Vector[Index],
    stoppingThreshold: Double,
    temperatureSchedule: Int => Double = (n: Int) => 1.0)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
    def sendLog(x: String) = Log.trace(x).unsafeRunSync()
    IO {
      var (assignments, stepLosses) = softEStep(indices, initModel, temperatureSchedule(0))
      var losses: List[Double] = List(mean(stepLosses))
      var model: Vector[ClusterParam] = initModel
      var stepNum = 1
      def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
      def shouldContinue = getDelta.forall(_ > stoppingThreshold)
      while(shouldContinue) {
        model = softMStep(model.size, indices, assignments)
        val p = softEStep(indices, model, temperatureSchedule(stepNum))
        assignments = p._1
        stepLosses = p._2
        val loss = mean(stepLosses)
        losses = loss :: losses
        stepNum = stepNum + 1

        sendLog("=== Stepping ===")
        val prior = assignments.map(a => a.params / a.sum).reduce(_ + _) / assignments.size.toDouble
        sendLog(s"Prior: " + prior.toScalaVector.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
        sendLog(s"Loss: $loss")
      }
      (model, assignments, losses.head)
    }.unsafeRunSync()
  }

  def hardEStep(
    indices: Vector[Index],
    model: Vector[ClusterParam]
  ): (Vector[Int], Vector[Double]) = {
    indices.map { i =>
      val clusterLosses = model.map(p => getInstanceLoss(i, p))
      clusterLosses.zipWithIndex.maxBy(_._1).swap
    }.unzip
  }

  def hardMStep(
    numClusters: Int,
    indices: Vector[Index],
    assignments: Vector[Int]
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      estimateParameterHard(
        assignments.zipWithIndex.filter(_._1 == clusterIndex).map(p => indices(p._2))
      )
    }
  }

  def runHardEM(
    initModel: Vector[ClusterParam],
    indices: Vector[Index],
    stoppingThreshold: Double)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): (Vector[ClusterParam], Vector[Int], Double) = {
    def sendLog(x: String) = Log.trace(x).unsafeRunSync()
    IO {
      var (assignments, stepLosses) = hardEStep(indices, initModel)
      var losses: List[Double] = List(mean(stepLosses))
      var model: Vector[ClusterParam] = initModel
      def getDelta() = (losses.get(1), losses.get(0)).mapN(_ - _)
      def shouldContinue() = getDelta().forall(_ > stoppingThreshold)

      while(shouldContinue()) {
        model = hardMStep(model.size, indices, assignments)
        val p = hardEStep(indices, model)
        assignments = p._1
        stepLosses = p._2
        val loss = mean(stepLosses)
        losses = loss :: losses
        sendLog("=== Stepping ===")
        val prior = assignments.counts.values.toVector.map(_ / assignments.size.toDouble)
        sendLog(s"Prior: " + prior.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
        sendLog(s"Loss: $loss")
      }
      (model, assignments, losses.head)
    }.unsafeRunSync()
  }
}
