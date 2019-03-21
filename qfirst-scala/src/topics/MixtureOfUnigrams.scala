package qfirst.topics

import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

object MixtureOfUnigrams {

  type Counts = Map[Int, Int]
  // assume sum to 1
  type Dist = Vector[Double]

  // prior.size == frames.size
  // all frames(i).size equal
  @JsonCodec case class UnigramMixtureModel(
    prior: Dist,
    clusters: Vector[Dist]) {
    def numClusters = prior.size
    def numItems = clusters.head.size
  }
  object UnigramMixtureModel {
    def init(numClusters: Int, numItems: Int, rand: Random): UnigramMixtureModel = {
      val clusterInitNums = (1 to numItems).toVector
      val numsTotal = clusterInitNums.sum
      val clusterInitProbs = clusterInitNums.map(_.toDouble / numsTotal)
      UnigramMixtureModel(
        prior = Vector.fill(numClusters)(1.0 / numClusters),
        clusters = Vector.fill(numClusters)(rand.shuffle(clusterInitProbs))
      )
    }

    // TODO kmeans++ initialization
  }

  def softEStep(
    instances: List[Counts],
    model: UnigramMixtureModel
  ): (List[Dist], Double) = {
    val (assignments, nlls) = instances.map { instance =>
      val unnormClusterProbs = model.prior.indices.map { clusterNum =>
        instance.map { case (itemNum, itemCount) =>
          math.pow(model.prior(clusterNum) * model.clusters(clusterNum)(itemNum), itemCount)
        }.product
      }.toVector
      val likelihood = unnormClusterProbs.sum
      val clusterProbs = unnormClusterProbs.map(_ / likelihood)
      val negLogLikelihood = -math.log(likelihood)
      clusterProbs -> negLogLikelihood
    }.unzip
    assignments -> (nlls.sum / nlls.size)
  }

  def softMStep(
    numItems: Int,
    instances: List[Counts],
    assignments: List[Dist]
  ): UnigramMixtureModel = {
    // add-1 smooth prior
    val prior = assignments.transpose.map(clusterCounts => (clusterCounts.sum + 1) / (assignments.size + assignments.head.size)).toVector
    val iaPairs = instances.zip(assignments)
    val clusters = assignments.head.indices.map { clusterNum =>
      val pseudoCounts = iaPairs.foldMap { case (instance, assignment) =>
        instance.map { case (itemNum, count) =>
          itemNum -> (assignment(clusterNum) * count)
        }
      }
      // add-0.01 smooth clusters
      val pseudoCountSum = pseudoCounts.values.sum
      pseudoCounts.foldLeft(Vector.fill(numItems)(0.01 / numItems)) {
        case (vec, (idx, pcount)) =>
          vec.updated(idx, (0.01 + pcount) / (pseudoCountSum + (0.01 * numItems)))
      }
    }.toVector
    UnigramMixtureModel(prior, clusters)
  }

  def runSoftEM(
    initModel: UnigramMixtureModel,
    instances: List[Counts],
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (UnigramMixtureModel, List[Vector[Double]], Double) = {
    var (assignments, loss) = softEStep(instances, initModel)
    var losses: List[Double] = List(loss)
    var model: UnigramMixtureModel = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.numItems, instances, assignments)
      val p = softEStep(instances, model)
      assignments = p._1
      loss = p._2
      losses = loss :: losses
      if(shouldLog) {
        println("=== Stepping ===")
        println(s"Prior: " + model.prior.sortBy(-_).take(10).map(x => f"$x%.2f").mkString(", "))
        println(s"Loss: $loss")
      }
    }
    if(shouldLog) {
      println("=== Stopped ===")
    }
    (model, assignments, losses.head)
  }
}
