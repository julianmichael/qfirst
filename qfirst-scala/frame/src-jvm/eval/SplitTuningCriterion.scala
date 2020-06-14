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

case class SplitTuningSpec(
  criterion: SplitTuningCriterion,
  thresholds: Option[List[Double]] = None
) {
  def run[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val runThresholds = thresholds.getOrElse(criterion.defaultThresholds)
    criterion.runTuning(allStats, runThresholds).flatTap { tuningResults =>
      val tunedBest = {
        val maxima = Chosen(tuningResults.toMap).keepMaximaBy(_.f1).data
        if(maxima.contains(runThresholds.max)) Chosen(Map(maxima.minBy(_._1)))
        else if(maxima.contains(runThresholds.min)) Chosen(Map(maxima.maxBy(_._1)))
        else Chosen(maxima).keepMaxBy(_.f1)
      }
      Log.info(s"Tuned results (${criterion.name}): ${getMetricsString(tunedBest)}")
    }
  }
}

trait SplitTuningCriterion {
  def name: String
  def defaultThresholds: List[Double]
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
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

  val all = List(
    OracleCriterion,
    NumClustersCriterion,
    TotalEntropyCriterion,
    SqNumClustersPenaltyCriterion,
    CuttingDeltaCriterion,
    LossPerItemCriterion
  )

  def fromString(x: String): Option[SplitTuningCriterion] = {
    all.find(_.name == x)
  }
}

import SplitTuningCriterion._

object NumClustersCriterion extends SplitTuningCriterion {
  val name: String = "num-clusters"
  val defaultThresholds = (1 to 35).toList.map(_.toDouble)
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val allStatsSortedByNumClusters = allStats.transform { case (_, results) =>
      results.sortBy(_.numClusters)
    }
    tuneWeightedStats(
      thresholds, allStatsSortedByNumClusters)(
      t => stats => {
        val qualified = stats.toList.dropWhile(_.numClusters < t)
        qualified.headOption.getOrElse(stats.last)
      }
    )
  }
}

object OracleCriterion extends SplitTuningCriterion {
  val name: String = "oracle"
  val defaultThresholds = {
    val logBound = 10
      (-logBound to logBound).toList.map(scala.math.pow(1.2, _))
  }
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    tuneWeightedStats(
      thresholds, allStats)(
      t => stats => {
        stats.toList.maxBy(_.fMeasure(t))
      }
    )
  }
}

object TotalEntropyCriterion extends SplitTuningCriterion {
  val name: String = "entropy"
  val defaultThresholds = (-40 to 40).map(_.toDouble / 20).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
      val numItems = s.numItems
      s.loss + (t * s.clusterSizes.foldMap(size => -size * scala.math.log(size.toDouble / numItems)))
    }
    tuneWeightedStats(
      thresholds, allStats)(
      t => stats => {
        val getLoss = getTotalLoss(t)
        stats.toList.minBy(getLoss)
      }
    )
  }
}

object SqNumClustersPenaltyCriterion extends SplitTuningCriterion {
  val name: String = "sq-nclusters"
  val defaultThresholds = (0 to 50).map(_.toDouble / 2).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
      s.loss + (t * scala.math.pow(s.numClusters, 2.0))
    }
    tuneWeightedStats(
      thresholds, allStats)(
      t => stats => {
        val getLoss = getTotalLoss(t)
        stats.toList.minBy(getLoss)
      }
    )
  }
}

object CuttingDeltaCriterion extends SplitTuningCriterion {
  val name: String = "cut-delta"
  val defaultThresholds = (0 to 100).map(_.toDouble / 10).toList
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val allStatsSortedByLoss = allStats.transform { case (_, results) =>
      results.sortBy(_.loss)
    }
    tuneWeightedStats(
      thresholds, allStatsSortedByLoss)(
      t => stats => {
        val deltaThreshold = t * stats.head.numItems
        stats.toList.sliding(2)
          .filter(_.size == 2)
          .takeWhile(w => w(1).loss - w(0).loss < deltaThreshold)
          .toList.lastOption
          .fold(stats.head)(_(0))
      }
    )
  }
}

object LossPerItemCriterion extends SplitTuningCriterion {
  val name: String = "loss"
  val defaultThresholds = (0 to 40).toList.map(_.toDouble / 10.0)
  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val allStatsSortedByLoss = allStats.transform { case (_, results) =>
      results.sortBy(-_.lossPerItem)
    }
    tuneWeightedStats(
      thresholds, allStatsSortedByLoss)(
      t => stats => {
        val qualified = stats.toList.takeWhile(_.lossPerItem > t)
        qualified.lastOption.getOrElse(stats.head)
      }
    )
  }
}
