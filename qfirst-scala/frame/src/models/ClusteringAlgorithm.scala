package qfirst.frame.models

import qfirst.frame.MergeTree

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

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

  // override for efficiency
  def getSingleInstanceParameter(
    instances: Instance,
    hyperparams: Hyperparams
  ): ClusterParam = {
    estimateParameter(Vector(instances), Vector(1.0), hyperparams)
  }

  // override for efficiency
  def estimateParameterHard(
    instances: Vector[Instance],
    hyperparams: Hyperparams
  ): ClusterParam = {
    estimateParameter(instances, instances.as(1.0), hyperparams)
  }

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

  // case class MergeCandidate(
  //   left: MergeTree[Int],
  //   right: MergeTree[Int],
  //   loss: Double
  // ) {
  //   val delta = getLossChangePriority(loss, left.loss, right.loss)
  //   def toTree(rank: Int) = MergeTree.Merge(
  //     rank, loss, left, right
  //   )
  // }
  // object MergeCandidate {
  //   implicit val mergeCandidateOrdering = {
  //     Ordering.by[MergeCandidate, Double](_.delta)
  //   }
  // }

  // should be overridden for efficiency, if possible
  def mergeParams(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): ClusterParam = {
    val indices = left.values ++ right.values
    val theseInstances = indices.map(instances.apply)
    val param = estimateParameterHard(theseInstances, hyperparams)
    param
  }

  // should be overridden for efficiency, if possible
  def mergeLoss(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams,
    sanityCheck: Boolean = true
  ): Double = {
    val indices = left.values ++ right.values
    val theseInstances = indices.map(instances.apply)
    val param = estimateParameterHard(theseInstances, hyperparams)
    val loss = aggregateLosses(theseInstances.map(computeLoss(_, param, hyperparams)))
    if(sanityCheck && !(loss >= left.loss && loss >= right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(right)
      println(loss)
      ???
    }
    loss
  }

  def runAgglomerativeClustering(
    instances: Vector[Instance],
    hyperparams: Hyperparams
  ): (MergeTree[Int], ClusterParam) = {
    require(instances.size > 0)
    val leafParams = instances.map(i => getSingleInstanceParameter(i, hyperparams))
    val distances = Array.ofDim[Double](instances.size, instances.size)

    var currentTrees = leafParams.zipWithIndex.map { case (param, index) =>
      val loss = computeLoss(instances(index), param, hyperparams)
      index -> ((MergeTree.Leaf(loss, index): MergeTree[Int]) -> param)
    }.toMap

    import scala.collection.mutable
    def simOrdering(i: Int) = Ordering.by[Int, (Double, Int)](j => distances(i)(j) -> j)
    // omit the last instance index because it's covered by all of the other queues
    val queues = mutable.Map(
      (0 until instances.size).map(i => i -> mutable.SortedMap.empty[Int, Double](simOrdering(i))): _*
    )
    currentTrees.toList.tails.toVector.foreach {
      case Nil => ()
      case (leftIndex, (left, leftParam)) :: rights =>
        if(rights.isEmpty) queues -= leftIndex // remove empty queue
        else rights.foreach { case (rightIndex, (right, rightParam)) =>
          val loss = mergeLoss(instances, left, leftParam, right, rightParam, hyperparams)
          val delta = getLossChangePriority(loss, left.loss, right.loss)
          distances(leftIndex)(rightIndex) = delta
          distances(rightIndex)(leftIndex) = delta
          queues(leftIndex) += (rightIndex -> loss)
        }
    }

    var nextRank = 1
    while(currentTrees.size > 1) {
      val i = queues.iterator.minBy { case (k, q) => distances(k)(q.firstKey) }._1
      val (j, newLoss) = queues(i).head

      queues -= j // remove j from consideration
      queues(i).clear() // clear i's queue since we'll refresh it
      val emptyQueues = queues.iterator.flatMap { case (k, q) =>
        q --= List(i, j)  // remove stale entries for i and j from all queues
        if(q.isEmpty && k != i) Some(k) else None // track which queues are now empty
      }
      queues --= emptyQueues // and remove other now-empty queues
      val (leftTree, leftParam) = currentTrees(i)
      val (rightTree, rightParam) = currentTrees(j)
      val newTree = MergeTree.Merge(nextRank, newLoss, leftTree, rightTree)
      val newParam = mergeParams(instances, leftTree, leftParam, rightTree, rightParam, hyperparams)
      currentTrees --= List(i, j) // remove i and j from current trees
      // get new merge candidates before adding new cluster back in
      val newMerges = currentTrees.iterator.map { case (k, (right, rightParam)) =>
        val loss = mergeLoss(instances, newTree, newParam, right, rightParam, hyperparams)
        val delta = getLossChangePriority(loss, newTree.loss, right.loss)
        // update distances before inserting anything into queues to keep things consistent
        distances(i)(k) = delta
        distances(k)(i) = delta
        k -> loss
      }.toList
      currentTrees += (i -> (newTree -> newParam)) // now add the new merged cluster to current trees
      queues(i) ++= newMerges // throw them all into queue i. don't need the symmetric case
      nextRank = nextRank + 1
    }

    if(currentTrees.size > 1) {
      println("WARNING: More than one tree remaining after agglomerative clustering")
      currentTrees.foreach(println)
      ???
    }
    currentTrees.head._2
  }

  // TODO: k-means|| init as well, and maybe random init?
  // TODO: perhaps make matrices out of computeLoss for more efficiency

  // def initPlusPlus(
  //   instances: Vector[Instance],
  //   hyperparams: Hyperparams,
  //   numClusters: Int
  // ): Vector[ClusterParam] = {
  //   val rand = new scala.util.Random()
  //   assert(numClusters >= 1)
  //   val firstCluster = estimateParameter(
  //     Vector(instances(rand.nextInt(instances.size))), Vector(1.0), hyperparams
  //   )
  //   val uniqueInstances = instances.groupBy(x => x).keys.toVector
  //   val initMinLosses = DenseVector(uniqueInstances.map(computeLoss(_, firstCluster, hyperparams)).toArray)

  //   initPlusPlusAux(uniqueInstances, hyperparams, Set(), Vector(firstCluster), numClusters - 1, initMinLosses)
  // }

  // private[this] def initPlusPlusAux(
  //   instances: Vector[Instance], hyperparams: Hyperparams,
  //   chosenInstances: Set[Int], curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double]
  // ): Vector[ClusterParam] = {
  //   if(numClustersLeft <= 0) curParams else {
  //     // println(s"cur min losses: $curMinLosses")
  //     // instances.foreach(i => println(i.toString.take(200)))
  //     val newCenterProbs = Multinomial(curMinLosses)
  //     val newCenterIndex = {
  //       var newIndex = newCenterProbs.draw
  //       while(chosenInstances.contains(newIndex)) {
  //         newIndex = newCenterProbs.draw
  //       }
  //       newIndex
  //     }
  //     val newCluster = estimateParameter(Vector(instances(newCenterIndex)), Vector(1.0), hyperparams)
  //     val clusterLosses = DenseVector(instances.map(computeLoss(_, newCluster, hyperparams)).toArray)
  //     val newMinLosses = min(curMinLosses, clusterLosses)
  //     initPlusPlusAux(instances, hyperparams, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses)
  //   }
  // }

  // def softEStep(
  //   instances: Vector[Instance],
  //   model: Vector[ClusterParam],
  //   hyperparams: Hyperparams
  // ): (Vector[DenseMultinomial], Vector[Double]) = {
  //   val allLosses = instances.map(i => model.map(p => computeLoss(i, p, hyperparams)))
  //   val assignments = allLosses.map { v =>
  //     val vec = DenseVector(v.toArray) *:* -1.0 // from unnormalized neg log likelihoods to unnorm log likelihoods
  //     val probs = exp(vec - logSumExp(vec)) // normalized likelihoods
  //     Multinomial(probs) // assume uniform prior -> likelihoods are assignment probabilities
  //   }
  //   val instanceLosses = assignments.map(_.sum)
  //   (assignments, instanceLosses)
  // }

  // def softMStep(
  //   numClusters: Int,
  //   instances: Vector[Instance],
  //   assignments: Vector[DenseMultinomial],
  //   hyperparams: Hyperparams
  // ): Vector[ClusterParam] = {
  //   (0 until numClusters).toVector.map { clusterIndex =>
  //     val clusterProbs = assignments.map(_.probabilityOf(clusterIndex))
  //     estimateParameter(instances, clusterProbs, hyperparams)
  //   }
  // }

  // def runSoftEM(
  //   initModel: Vector[ClusterParam],
  //   instances: Vector[Instance],
  //   hyperparams: Hyperparams,
  //   stoppingThreshold: Double,
  //   shouldLog: Boolean = true
  // ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
  //   var (assignments, stepLosses) = softEStep(instances, initModel, hyperparams)
  //   var losses: List[Double] = List(mean(stepLosses))
  //   var model: Vector[ClusterParam] = initModel
  //   def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
  //   def shouldContinue = getDelta.forall(_ > stoppingThreshold)
  //   while(shouldContinue) {
  //     model = softMStep(model.size, instances, assignments, hyperparams)
  //     val p = softEStep(instances, model, hyperparams)
  //     assignments = p._1
  //     stepLosses = p._2
  //     val loss = mean(stepLosses)
  //     losses = loss :: losses
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

  override def getSingleInstanceParameter(
    instance: Instance,
    hyperparams: Hyperparams
  ): ClusterParam = (
    _1.getSingleInstanceParameter(instance._1, hyperparams.__1),
    _2.getSingleInstanceParameter(instance._2, hyperparams.__2),
  )

  override def estimateParameterHard(
    instances: Vector[Instance],
    hyperparams: Hyperparams
  ): ClusterParam = {
    (_1.estimateParameterHard(instances.map(_._1), hyperparams.__1),
     _2.estimateParameterHard(instances.map(_._2), hyperparams.__2))
  }

  // should be overridden for efficiency, if possible
  // TODO only works with summing aggregateLosses and delta
  override def mergeParams(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams
  ): ClusterParam = {
    val leftMerge = _1.mergeParams(instances.map(_._1), left, leftParam._1, right, rightParam._1, hyperparams.__1)
    val rightMerge = _2.mergeParams(instances.map(_._2), left, leftParam._2, right, rightParam._2, hyperparams.__2)
    (leftMerge, rightMerge)
  }

  override def mergeLoss(
    instances: Vector[Instance],
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam,
    hyperparams: Hyperparams,
    sanityCheck: Boolean = true
  ): Double = {
    val param = mergeParams(instances, left, leftParam, right, rightParam, hyperparams)
    val newLoss = (left.values ++ right.values).foldMap(i => computeLoss(instances(i), param, hyperparams))
    if(sanityCheck && !(newLoss >= left.loss && newLoss >= right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(right)
      println(newLoss)
      ???
    }
    newLoss
  }
}
