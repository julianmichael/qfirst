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

sealed trait ClusteringTree[Param, Instance] {
  import ClusteringTree._
  def instances: Vector[Instance] = this match {
    case Merge(_, l, r) => l.instances ++ r.instances
    case Leaf(i) => Vector(i)
  }
}
object ClusteringTree {
  case class Merge[Param, Instance](
    param: Param,
    left: ClusteringTree[Param, Instance],
    right: ClusteringTree[Param, Instance]
  ) extends ClusteringTree[Param, Instance]
  case class Leaf[Param, Instance](
    value: Instance
  ) extends ClusteringTree[Param, Instance]
}

trait ClusteringAlgorithm {
  type ClusterParam
  type Instance
  type Hyperparams

  // TODO: make it a matrix
  // for latent-variable clustering (e-step)
  def computeLosses(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): Vector[Vector[Double]] // each elt is the set of (nonnegative) losses for an instance

  // for agglomerative clustering
  def distance(
    p1: ClusterParam,
    p2: ClusterParam
  ): Double

  // for both latent-variable (m-step) and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam

  // can be overridden for efficiency
  def merge(
    left: ClusteringTree[ClusterParam, Instance],
    right: ClusteringTree[ClusterParam, Instance],
    hyperparams: Hyperparams
  ): ClusteringTree.Merge[ClusterParam, Instance] = {
    val instances = left.instances ++ right.instances
    val param = estimateParameter(instances, instances.as(1.0), hyperparams)
    ClusteringTree.Merge(param, left, right)
  }

  // TODO: k-means|| init as well, and maybe random init?

  def initPlusPlus(
    instances: Vector[Instance],
    hyperparams: Hyperparams,
    numClusters: Int
  ): Vector[ClusterParam] = {
    val rand = new scala.util.Random()
    assert(numClusters >= 1)
    val firstCluster = estimateParameter(
      Vector(instances(rand.nextInt(instances.size))), Vector(1.0), hyperparams
    )
    val uniqueInstances = instances.groupBy(x => x).keys.toVector
    val initMinLosses = DenseVector(computeLosses(uniqueInstances, Vector(firstCluster)).map(_.head).toArray)

    // if(sum(curMinLosses) < 0.01) {
      // println(s"Instances: ${uniqueInstances.size}")
      // println(s"cluster: ${firstCluster.toString.take(200)}")
      // println(s"Num clusters: $numClusters")
    // }

    initPlusPlusAux(uniqueInstances, hyperparams, Set(), Vector(firstCluster), numClusters - 1, initMinLosses)
  }

  private[this] def initPlusPlusAux(
    instances: Vector[Instance], hyperparams: Hyperparams,
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
      val newCluster = estimateParameter(Vector(instances(newCenterIndex)), Vector(1.0), hyperparams)
      val clusterLosses = DenseVector(computeLosses(instances, Vector(newCluster)).map(_.head).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(instances, hyperparams, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
    }
  }

  def softEStep(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val allLosses = computeLosses(instances, model)
    val assignments = allLosses.map(v => Multinomial(exp(DenseVector(v.toArray))))
    val instanceLosses = assignments.map(_.sum)
    (assignments, instanceLosses)
  }

  def softMStep(
    numClusters: Int,
    instances: Vector[Instance],
    assignments: Vector[DenseMultinomial],
    hyperparams: Hyperparams
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      val clusterProbs = assignments.map(_.probabilityOf(clusterIndex))
      estimateParameter(instances, clusterProbs, hyperparams)
    }
  }

  def runSoftEM(
    initModel: Vector[ClusterParam],
    instances: Vector[Instance],
    hyperparams: Hyperparams,
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
    var (assignments, stepLosses) = softEStep(instances, initModel)
    var losses: List[Double] = List(mean(stepLosses))
    var model: Vector[ClusterParam] = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.size, instances, assignments, hyperparams)
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

  def runMergeClustering(
    instances: Vector[Instance],
    hyperparams: Hyperparams,
    shouldLog: Boolean = true
  ): ClusteringTree[ClusterParam, Instance] = {
    ??? // TODO
  }
}

trait CompositeClusteringAlgorithm extends ClusteringAlgorithm {
  val _1: ClusteringAlgorithm
  val _2: ClusteringAlgorithm
  val lambda: Double
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Instance = (_1.Instance, _2.Instance)
  type Hyperparams = (_1.Hyperparams, _2.Hyperparams)

  def computeLosses(
    instances: Vector[Instance],
    model: Vector[ClusterParam]
  ): Vector[Vector[Double]] = { // each elt is the set of (nonnegative) losses for an instance
    val l1 = _1.computeLosses(instances.map(_._1), model.map(_._1))
    val l2 = _2.computeLosses(instances.map(_._2), model.map(_._2))
    l1.zip(l2).map { case (il1, il2) =>
      il1.zip(il2).map(Function.tupled((x: Double, y: Double) => (lambda * x) + ((1.0 - lambda) * y)))
    }
  }

  // for agglomerative clustering
  def distance(
    p1: ClusterParam,
    p2: ClusterParam
  ): Double = {
    (lambda * _1.distance(p1._1, p2._1)) + ((1.0 - lambda) * _2.distance(p1._2, p2._2))
  }

  // for both latent-variable (m-step) and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    (_1.estimateParameter(instances.map(_._1), assignmentProbabilities, hyperparams._1),
     _2.estimateParameter(instances.map(_._2), assignmentProbabilities, hyperparams._2))
  }
}
