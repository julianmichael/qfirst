package qfirst.frame

import qfirst.frame.clustering.FlatClusteringAlgorithm
import qfirst.frame.clustering.AgglomerativeClusteringAlgorithm
import qfirst.frame.clustering.AgglomerativeSetClustering

import freelog.EphemeralTreeLogger

import cats.data.NonEmptyVector
import cats.effect.IO
import cats.implicits._

object Clustering {

  import breeze.linalg.DenseVector
  import breeze.stats.distributions.Multinomial

  val numFlatClusters = 100

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
  val flatClusteringHardStoppingDelta = 1e-9
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
    if(indices.size <= numFlatClusters) { // we can immediately do agglom. clustering
      IO {
        val (argClusterTree, params) = agglomAlgorithm.runFullAgglomerativeClustering(indices)
        argClusterTree.map(Set(_)) -> params
      }
    } else { // we need a flat pre-clustering step
      val indicesVec = indices.toVector
      for {
        _ <- Log.info(s"Pre-clustering ${indices.size} items.")
        allEMTrials <- (1 to 3).toList.traverse(i =>
          Log.traceBranch(s"Flat clustering trial $i")(
            for {
              initModel <- IO(flatAlgorithm.initPlusPlus(indicesVec, numFlatClusters))
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
