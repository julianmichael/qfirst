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

trait ClusteringAlgorithm {
  type ClusterParam
  type Instance
  type Hyperparams

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double

  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam

  // override eg for max
  def aggregateLosses(
    losses: Vector[Double]
  ): Double = losses.sum

  // override eg for max
  def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    rightLoss: Double
  ) = newLoss - leftLoss - rightLoss

  case class MergeCandidate(
    left: MergeTree[(ClusterParam, Double), Int],
    right: MergeTree[(ClusterParam, Double), Int],
    param: ClusterParam,
    loss: Double
  ) {
    val delta = getLossChangePriority(loss, left.param._2, right.param._2)
    def toTree(rank: Int) = MergeTree.Merge(
      rank, param -> loss, left, right
    )
  }
  object MergeCandidate {
    implicit val mergeCandidateOrdering = {
      Ordering.by[MergeCandidate, Double](_.delta)
    }
  }

  // should be overridden for efficiency, if possible
  // TODO separate ClusterParam out of trees and just take it as arguments
  // this way we don't keep the stale params around in the whole tree
  // but we are free to use them to efficiently merge.
  // also enables this to be implemented in the composite case.
  // TODO also just return param and loss?
  def merge(
    instances: Vector[Instance],
    left: MergeTree[(ClusterParam, Double), Int],
    right: MergeTree[(ClusterParam, Double), Int],
    hyperparams: Hyperparams
  ): MergeCandidate = {
    val indices = left.values ++ right.values
    val theseInstances = indices.map(instances.apply)
    val param = estimateParameter(theseInstances, indices.as(1.0), hyperparams)
    val loss = aggregateLosses(theseInstances.map(computeLoss(_, param, hyperparams)))
    MergeCandidate(left, right, param, loss)
  }

  def runAgglomerativeClustering(
    instances: Vector[Instance],
    hyperparams: Hyperparams
  ): MergeTree[(ClusterParam, Double), Int] = {
    val leafParams = instances.map(i => estimateParameter(Vector(i), Vector(1.0), hyperparams))
    val leaves = leafParams.zipWithIndex.map { case (param, index) =>
      val loss = computeLoss(instances(index), param, hyperparams)
      MergeTree.Leaf(param -> loss, index)
    }.toList
    import scala.collection.mutable
    var currentTrees: Set[MergeTree[(ClusterParam, Double), Int]] =
      (leaves: List[MergeTree[(ClusterParam, Double), Int]]).toSet
    val mergeHeap = mutable.PriorityQueue.empty[MergeCandidate]
    leaves.tails.foreach {
      case Nil => ()
      case left :: rights =>
        mergeHeap.enqueue(rights.map(r => merge(instances, left, r, hyperparams)): _*)
    }
    var nextRank = 1
    while(mergeHeap.nonEmpty) {
      mergeHeap.filterDequeue(mt =>
        currentTrees.contains(mt.left) && currentTrees.contains(mt.right)
      ).map(_.toTree(nextRank)).foreach { next =>
        nextRank = nextRank + 1
        currentTrees = currentTrees - next.left - next.right
        val newMerges = currentTrees.map(left => merge(instances, left, next, hyperparams))
        currentTrees = currentTrees + next
        mergeHeap.enqueue(newMerges.toSeq: _*)
      }
    }
    if(currentTrees.size != 1) {
      println("WARNING: More than one tree remaining after agglomerative clustering")
      currentTrees.foreach(println)
      ???
    }
    currentTrees.head
  }

  // TODO: k-means|| init as well, and maybe random init?
  // TODO: perhaps make matrices out of computeLoss for more efficiency

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
    val initMinLosses = DenseVector(uniqueInstances.map(computeLoss(_, firstCluster, hyperparams)).toArray)

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
      val clusterLosses = DenseVector(instances.map(computeLoss(_, newCluster, hyperparams)).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(instances, hyperparams, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
    }
  }

  def softEStep(
    instances: Vector[Instance],
    model: Vector[ClusterParam],
    hyperparams: Hyperparams
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val allLosses = instances.map(i => model.map(p => computeLoss(i, p, hyperparams)))
    val assignments = allLosses.map { v =>
      val vec = DenseVector(v.toArray)
      val probs = exp(vec - logSumExp(vec)) // normalize first to keep stable
      Multinomial(probs)
    }
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
    var (assignments, stepLosses) = softEStep(instances, initModel, hyperparams)
    var losses: List[Double] = List(mean(stepLosses))
    var model: Vector[ClusterParam] = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.size, instances, assignments, hyperparams)
      val p = softEStep(instances, model, hyperparams)
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

trait CompositeClusteringAlgorithm extends ClusteringAlgorithm {
  val _1: ClusteringAlgorithm
  val _2: ClusteringAlgorithm
  val lambda: Double
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Instance = (_1.Instance, _2.Instance)
  type Hyperparams = (_1.Hyperparams, _2.Hyperparams)
  // case class Hyperparams(_1: _1.Hyperparams, _2: _2.Hyperparams, lambda: Double)

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    (lambda * _1.computeLoss(instance._1, param._1, hyperparams._1)) +
      ((1.0 - lambda) * _2.computeLoss(instance._2, param._2, hyperparams._2))
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
