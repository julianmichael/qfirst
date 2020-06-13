package qfirst.frame.eval

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

import qfirst.metrics.Chosen
import qfirst.metrics.Numbers

import qfirst.frame.logLevel
import qfirst.frame.progressSpec
import qfirst.frame.getMetricsString

trait SplitTuningCriterion {
  def name: String
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(Double, WeightedPR)]]

}
object SplitTuningCriterion {

  def getTunedWeightedStats[VerbType](
    allResults: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    choose: NonEmptyList[ConfStatsPoint] => ConfStatsPoint
  ): WeightedPR = {
    allResults.toList.foldMap { case (verbType, results) =>
      choose(results).weightedPR
    }
  }

  def tuneWeightedStats[VerbType, A](
    thresholds: List[A],
    allResults: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    choose: A => NonEmptyList[ConfStatsPoint] => ConfStatsPoint)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(A, WeightedPR)]] = {
    thresholds.infoBarTraverse("Tuning weighted stats") { threshold =>
      IO.pure(threshold -> getTunedWeightedStats(allResults, choose(threshold)))
    }
  }
}

import SplitTuningCriterion._

object NumClustersCriterion extends SplitTuningCriterion {
  val name: String = "const num clusters"
  val thresholds = (1 to 30).toList.map(_.toDouble)
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val allStatsSortedByNumClusters = allStats.transform { case (_, results) =>
      results.sortBy(_.numClusters)
    }
    for {
      tuningResults <- tuneWeightedStats(
        thresholds, allStatsSortedByNumClusters)(
        t => stats => {
          val qualified = stats.toList.dropWhile(_.numClusters < t)
          qualified.headOption.getOrElse(stats.last)
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      _ <- Log.info(s"Tuned results (best num clusters): ${getMetricsString(tunedBest)}")
      // _ <- allTuningResults.update(_ + (name -> tuningResults))
    } yield tuningResults
  }
}

object SqNumClustersPenaltyCriterion extends SplitTuningCriterion {
  val name: String = "sq num clusters penalty"
  val coeffs = (0 to 50).map(_.toDouble / 2).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
      s.loss + (t * scala.math.pow(s.numClusters, 2.0))
    }
    for {
      tuningResults <- tuneWeightedStats(
        coeffs, allStats)(
        t => stats => {
          val getLoss = getTotalLoss(t)
          stats.toList.minBy(getLoss)
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      _ <- Log.info(s"Tuned results (sq num cluster penalty): ${getMetricsString(tunedBest)}")
    } yield tuningResults
  }
}

object CuttingDeltaCriterion extends SplitTuningCriterion {
  val name: String = "cutting delta"
  val deltasPerItem = (0 to 100).map(_.toDouble / 10).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val allStatsSortedByLoss = allStats.transform { case (_, results) =>
      results.sortBy(_.loss)
    }
    for {
      tuningResults <- tuneWeightedStats(
        deltasPerItem, allStatsSortedByLoss)(
        t => stats => {
          val deltaThreshold = t * stats.head.numItems
          stats.toList.sliding(2)
            .filter(_.size == 2)
            .takeWhile(w => w(1).loss - w(0).loss < deltaThreshold)
            .toList.lastOption
            .fold(stats.head)(_(0))
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      _ <- Log.info(s"Tuned results (per-item cutting delta): ${getMetricsString(tunedBest)}")
    } yield tuningResults
  }
}

object LossPerItemCriterion extends SplitTuningCriterion {
  val name: String = "loss per item"
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val lossPerItemAll = allStats.toList.foldMap { case (verbType, results) =>
      results.foldMap(s => Numbers(s.lossPerItem))
    }
    val lossPerItemBest = allStats.toList.foldMap { case (verbType, results) =>
      val bestF1 = results.map(_.f1).maximum
      results.filter(_.f1 == bestF1).foldMap(s => Numbers(s.lossPerItem))
    }
    val stats = lossPerItemAll.stats
    val interval = scala.math.round(stats.quartiles.max - stats.quartiles.min) / 50.0
    val allStatsSortedByLoss = allStats.transform { case (_, results) =>
      results.sortBy(-_.lossPerItem)
    }
    val lossThresholds = (
      scala.math.round(stats.quartiles.min / interval).toInt to scala.math.round(stats.quartiles.max / interval).toInt
    ).map(_ * interval).toList
    for {
      tuningResults <- tuneWeightedStats(
        lossThresholds, allStatsSortedByLoss)(
        t => stats => {
          val qualified = stats.toList.takeWhile(_.lossPerItem > t)
          qualified.lastOption.getOrElse(stats.head)
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      // _ <- Log.info(s"Loss per item (all): ${getMetricsString(lossPerItemAll)}")
      // _ <- Log.info(s"Loss per item (best): ${getMetricsString(lossPerItemBest)}")
      _ <- Log.info(s"Tuned results (best loss per item): ${getMetricsString(tunedBest)}")
    } yield tuningResults
  }
}

object OracleCriterion extends SplitTuningCriterion {
  val name: String = "oracle"
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val logBound = 10
    val betas = (-logBound to logBound).toList.map(scala.math.pow(1.2, _))
    for {
      tuningResults <- tuneWeightedStats(
        betas, allStats)(
        t => stats => {
          stats.toList.maxBy(_.fMeasure(t))
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      _ <- Log.info(s"Tuned results (oracle): ${getMetricsString(tunedBest)}")
    } yield tuningResults
  }
}

object TotalEntropyCriterion extends SplitTuningCriterion {
  val name: String = "total entropy"
  val coeffs = (-40 to 40).map(_.toDouble / 20).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
      val numItems = s.numItems
      s.loss + (t * s.clusterSizes.foldMap(size => -size * scala.math.log(size.toDouble / numItems)))
    }
    for {
      tuningResults <- tuneWeightedStats(
        coeffs, allStats)(
        t => stats => {
          val getLoss = getTotalLoss(t)
          stats.toList.minBy(getLoss)
        }
      )
      tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
      _ <- Log.info(s"Tuned results (cluster dist entropy loss coeff): ${getMetricsString(tunedBest)}")
    } yield tuningResults
  }
}
