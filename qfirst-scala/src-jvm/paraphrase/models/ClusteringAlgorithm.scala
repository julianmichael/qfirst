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
    left: MergeTree[Int],
    right: MergeTree[Int],
    param: ClusterParam,
    loss: Double
  ) {
    val delta = getLossChangePriority(loss, left.loss, right.loss)
    def toTreeAndParam(rank: Int) = MergeTree.Merge(
      rank, loss, left, right
    ) -> param
  }
  object MergeCandidate {
    implicit val mergeCandidateOrdering = {
      Ordering.by[MergeCandidate, Double](_.delta)
    }
  }

  // should be overridden for efficiency, if possible
  def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
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
  ): (MergeTree[Int], ClusterParam) = {
    val leafParams = instances.map(i => estimateParameter(Vector(i), Vector(1.0), hyperparams))
    val distances = DenseMatrix.zeros[Double](instances.size, instances.size)
    var currentTrees = leafParams.zipWithIndex.map { case (param, index) =>
      val loss = computeLoss(instances(index), param, hyperparams)
      index -> ((MergeTree.Leaf(loss, index): MergeTree[Int]) -> param)
    }.toMap
    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, Double](j => distances(i, j))
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      (0 until instances.size).map(i => i -> mutable.SortedMap.empty[Int, MergeCandidate](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val cand = merge(instances, left, leftParam, right, rightParam, hyperparams)
          distances.update(leftIndex, rightIndex, cand.delta)
          distances.update(rightIndex, leftIndex, cand.delta)
          queues(leftIndex) += (rightIndex -> cand)
        }
    }
    var nextRank = 1
    while(currentTrees.size > 1) {
      // println(currentTrees)
      // queues.foreach(println)
      val i = queues.iterator.minBy { case (k, q) => distances(k, q.firstKey) }._1
      val (j, mergeCandidate) = queues(i).head
      queues -= j // remove j from consideration
      queues(i).clear() // clear i's queue since we'll refresh it
      val emptyQueues = queues.iterator.flatMap { case (k, q) =>
        q --= List(i, j)  // remove stale entries for i and j from all queues
        if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
      }
      queues --= emptyQueues // and remove other now-empty queues
      currentTrees --= List(i, j) // remove i and j from current trees
      val (newTree, newParam) = mergeCandidate.toTreeAndParam(nextRank)
      // get new merge candidates before adding new cluster back in
      val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
        val cand = merge(instances, newTree, newParam, right, rightParam, hyperparams)
        // update distances before inserting anything into queues to keep things consistent
        distances.update(i, k, cand.delta)
        distances.update(k, i, cand.delta)
        k -> cand
      }.toList
      currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
      queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
      nextRank = nextRank + 1
    }

    /*
    val mergeHeap = mutable.PriorityQueue.empty[MergeCandidate]
    currentTrees.toList.tails.foreach {
      case Nil => ()
      case (left, leftParam) :: rights =>
        mergeHeap.enqueue(
          rights.map { case (right, rightParam) =>
            merge(instances, left, leftParam, right, rightParam, hyperparams)
          }: _*
        )
    }
    var nextRank = 1
    while(mergeHeap.nonEmpty) {
      mergeHeap.filterDequeue(mt =>
        currentTrees.contains(mt.left) && currentTrees.contains(mt.right)
      ).map(_.toTreeAndParam(nextRank)).foreach { case (next, nextParam) =>
          println(s"=== ADDING MERGE === ${next -> nextParam}")
          nextRank = nextRank + 1
          currentTrees = currentTrees - next.left - next.right
          val newMerges = currentTrees.toList.map { case (left, leftParam) =>
            merge(instances, left, leftParam, next, nextParam, hyperparams)
          }
          currentTrees = currentTrees + (next -> nextParam)
          mergeHeap.enqueue(newMerges: _*)
      }
    }
    */

    if(currentTrees.size != 1) {
      println("WARNING: More than one tree remaining after agglomerative clustering")
      currentTrees.foreach(println)
      ???
    }
    currentTrees.head._2
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
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Instance = (_1.Instance, _2.Instance)
  case class Hyperparams(__1: _1.Hyperparams, __2: _2.Hyperparams, lambda: Double)

  def computeLoss(
    instance: Instance,
    param: ClusterParam,
    hyperparams: Hyperparams
  ): Double = {
    (hyperparams.lambda * _1.computeLoss(instance._1, param._1, hyperparams.__1)) +
      ((1.0 - hyperparams.lambda) * _2.computeLoss(instance._2, param._2, hyperparams.__2))
  }

  // for both latent-variable (m-step) and agglomerative clustering
  def estimateParameter(
    instances: Vector[Instance],
    assignmentProbabilities: Vector[Double],
    hyperparams: Hyperparams
  ): ClusterParam = {
    (_1.estimateParameter(instances.map(_._1), assignmentProbabilities, hyperparams.__1),
     _2.estimateParameter(instances.map(_._2), assignmentProbabilities, hyperparams.__2))
  }

  // should be overridden for efficiency, if possible
  // TODO only works with summing aggregateLosses and delta
  override def merge(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): MergeCandidate = {
    val leftMerge = _1.merge(instances.map(_._1), left, leftParam._1, right, rightParam._1, hyperparams.__1)
    val rightMerge = _2.merge(instances.map(_._2), left, leftParam._2, right, rightParam._2, hyperparams.__2)
    MergeCandidate(left, right, (leftMerge.param, rightMerge.param), leftMerge.loss + rightMerge.loss)
  }
}
