package qfirst.frame

import qfirst.frame.clustering.FlatClusteringAlgorithm
import qfirst.frame.clustering.AgglomerativeClusteringAlgorithm
import qfirst.frame.clustering.AgglomerativeSetClustering

import freelog.EphemeralTreeLogger
import freelog.implicits._

import cats.data.NonEmptyVector
import cats.effect.IO
import cats.implicits._

object ClusteringParams {

  import breeze.linalg.DenseVector
  import breeze.stats.distributions.Multinomial

  val flatClusteringThreshold = 300
  val numFlatClusters = 200

  // soft EM hyperparams
  val flatClusteringSoftStoppingDelta = 1e-8
  val flatClusteringTempSched = (x: Int) => scala.math.pow(0.8, x)
  val flatClusteringPriorEstimatorDense = (counts: DenseVector[Double]) => {
    // smoothes with dirichlet, then inverts the odds.
    // invertOdds(dirichletPosteriorFromDense(counts, 1000))

    // uniform prior
    Multinomial(DenseVector.ones[Double](counts.size))
  }
  // hard EM hyperparams
  val numEMTrials = 10
  val flatClusteringHardStoppingDelta = 1e-5
  val flatClusteringPriorEstimatorSparse = (counts: Map[Int, Int], numClusters: Int) => {
    // smoothes with dirichlet, then inverts the odds.
    // invertOdds(dirichletPosteriorFromSparseNew(counts, numClusters, 1000))

    // always assume uniform prior --- seems to work just as well if not better
    Multinomial(DenseVector.ones[Double](numClusters))
  }

  def runCombinedClustering[I, FP, AP](
    indices: NonEmptyVector[I],
    flatAlgorithm: FlatClusteringAlgorithm { type Index = I; type ClusterParam = FP },
    agglomAlgorithm: AgglomerativeClusteringAlgorithm { type Index = I; type ClusterParam = AP })(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[(MergeTree[Set[I]], AP)] = {
    if(indices.size <= flatClusteringThreshold) { // we can immediately do agglom. clustering
      IO {
        val (argClusterTree, params) = agglomAlgorithm.runFullAgglomerativeClustering(indices)
        argClusterTree.map(Set(_)) -> params
      }
    } else { // we need a flat pre-clustering step
      val indicesVec = indices.toVector
      for {
        allEMTrials <- (1 to numEMTrials).toList.infoBarTraverse(
          s"Pre-clustering ${indices.size} items."
        )(i =>
          Log.traceBranch(s"Flat clustering trial $i")(
            for {
              initModel <- Log.traceBranch("Initializing clusters") {
                IO(flatAlgorithm.initPlusPlus(indicesVec, numFlatClusters))
              }
              // softEMModel <- flatAlgorithm.runSoftEM(
              //   initModel, argIds,
              //   flatClusteringSoftStoppingDelta,
              //   flatClusteringTempSched,
              //   flatClusteringPriorEstimatorDense
              // ).map(_._1)
              res <- flatAlgorithm.runHardEM(
                initModel /*softEMModel*/, indicesVec,
                flatClusteringHardStoppingDelta,
                flatClusteringPriorEstimatorSparse
              )
            } yield res
          )
        )
        hardEMAssignments = allEMTrials.filterNot(_._3.isNaN).minBy(_._3)._2
        hardEMClusters = hardEMAssignments.zipWithIndex.groupBy(_._1).toVector.map {
          case (_, is) => is.map(i => indicesVec(i._2)).toSet
        }
        setClusteringAlg = new AgglomerativeSetClustering(agglomAlgorithm)
      } yield setClusteringAlg.runFullAgglomerativeClustering(
        NonEmptyVector.fromVector(hardEMClusters).get
      )
    }
  }
}
