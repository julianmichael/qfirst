package qfirst.frame.eval

import qfirst.frame._
import qfirst.frame.features.Features
import qfirst.frame.util.Duad
import qfirst.frame.util.NonMergingMap

import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

import jjm.io.FileUtil
import jjm.implicits._

import freelog.EphemeralTreeLogger
import freelog.SequentialEphemeralTreeLogger
import freelog.implicits._

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._

import cats.effect.IO
import cats.effect.Timer
import cats.effect.concurrent.Ref

import java.nio.file.{Path => NIOPath}

import scala.annotation.tailrec

object Evaluation {

  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

  def getAllClusteringConfs[A](
    goldLabelTree: MergeTree[Map[A, Int]],
    extraClusters: Map[String, Map[A, Int]],
    metric: ClusterPRMetric
  ): NonEmptyList[ConfStatsPoint] = {
    val goldClusterSizes = goldLabelTree.unorderedFold |+| extraClusters.unorderedFold
    NonEmptyList.fromList(
      goldLabelTree.clusterSplittingsStreamByMaxLossPerCount(_.unorderedFold).take(1000).map { clusters =>
        val losses = clusters.map(_.loss)
        val clusterSizes = clusters.map(_.unorderedFoldMap(_.unorderedFold))
        val weightedPR = metric(goldClusterSizes, clusters.map(_.unorderedFold) ++ extraClusters.values)
        ConfStatsPoint(losses, clusterSizes, weightedPR)
      }.compile.toList
    ).get
  }

  def getLeafClusteringConf[A](
    goldLabelTree: MergeTree[Map[A, Int]],
    extraClusters: Map[String, Map[A, Int]],
    metric: ClusterPRMetric
  ): ConfStatsPoint = {
    val goldClusterSizes = goldLabelTree.unorderedFold |+| extraClusters.unorderedFold
    val clusters = goldLabelTree.valuesWithLosses
    val losses = clusters.map(_._1)
    val clusterSizes = clusters.map(_._2.unorderedFold)
    val weightedPR = metric(goldClusterSizes, clusters.map(_._2) ++ extraClusters.values)
    ConfStatsPoint(losses, clusterSizes, weightedPR)
  }

  def getAllPRStats[VerbType, InstanceId, GoldLabel](
    clusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric,
    maxClustersOnly: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, NonEmptyList[ConfStatsPoint]]] = clusterings.toList
    .filter(_._2.nonEmpty)
    .infoBarTraverse(s"Calculating ${metric.name} for all clusterings") { case (verbType, clustering) =>
      val getGoldLabelForVerb = getGoldLabel(verbType)
      Log.trace(s"$verbType (${clustering.size} leaves + extras)") >> IO {
        val extraClusterGoldCounts = clustering.extraClusters.mapVals(
          _.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1))
        )
        val confs = clustering.clusterTreeOpt match {
          case None =>
            val goldClusterSizes = extraClusterGoldCounts.unorderedFold
            NonEmptyList.of(
              ConfStatsPoint(
                Vector(), Vector(), metric(goldClusterSizes, extraClusterGoldCounts.values.toVector)
              )
            )
          case Some(tree) =>
            val goldCountTree = tree.map(
              _.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1))
            )
            // we only consider default clustering of gold-derived models
            if(maxClustersOnly) NonEmptyList.of(
              getLeafClusteringConf(goldCountTree, extraClusterGoldCounts, metric)
            )
            else getAllClusteringConfs(goldCountTree, extraClusterGoldCounts, metric)
        }
        verbType -> confs
      }
    }.map(_.toMap)

  def runTuningCalibration[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpec: SplitTuningSpec,
    resultsDir: NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val name = tuningSpec.criterion.name
    for {
      dataPoints <- allStats.toList
        .groupBy(_._2.head.numItems).toList
        .traverse { case (numItems, statSets) =>
          tuningSpec.run(statSets.toMap)(EphemeralTreeLogger.noop).map(points =>
            points.map { case (threshold, pr) =>
              PlottingJVM.WeirdLinePoint(scala.math.log(numItems.toDouble), threshold, pr.f1, (numItems * statSets.size) / 25.0)
            }
          )
        }
      _ <- PlottingJVM.plotWeirdLines(
        dataPoints,
        title = s"Scores ($name) by number of instances and thresholds",
        xAxisLabel = "Log num instances",
        yAxisLabel = "Threshold",
        path = resultsDir.resolve(s"tuning-calibration-$name.png")
      )
    } yield ()
  }

  def runTuningCalibration2[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpec: SplitTuningSpec,
    resultsDir: NIOPath
  ): IO[Unit] = {
    val name = tuningSpec.criterion.name
    case class CalibrationPoint(
      threshold: Double,
      stats: ConfStatsPoint
    )
    val dataPoints: List[CalibrationPoint] = allStats.values.toList.map { choices =>
      val threshold = SplitTuningCriterion.chooseBest(
        tuningSpec.thresholds.map(t =>
          t -> tuningSpec.criterion.selectPoint(t)(choices).weightedPR
        )
      ).data.head._1
      CalibrationPoint(threshold, tuningSpec.criterion.selectPoint(threshold)(choices))
    }
    PlottingJVM.plotWeightedScatter(
      dataPoints.foldMap(p =>
        Map(
          "max F1" -> List(PlottingJVM.WeightedScatterPoint(p.stats.numItems, p.threshold, p.stats.numItems)),
        )
      ),
      title = s"Best threshold ($name) by number of instances",
      xAxisLabel = "Num instances",
      yAxisLabel = "Best threshold",
      path = resultsDir.resolve(s"tuning-calibration-$name.png")
    )
  }

  def runClusterTuning[VerbType](
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    resultsDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[(SplitTuningCriterion, Double)] = {
    def outerLogger = Log
    freelog.loggers.TimingDelayedLogger.file(resultsDir.resolve("results.txt")) { fileLogger =>
      implicit val Log = outerLogger |+| fileLogger // shadow the outer one, removing implicit ambiguity
      for {
        _ <- Log.info {
          // max precision FYI for e.g. knowing how well flat clustering worked
          val maxPrecision = allStats.toList.foldMap { case (verbType, results) =>
            results.toList.maxBy(_.precision).weightedPR
          }.precision
          f"Max precision: $maxPrecision%.3f"
        }
        tuningResults <- tuningSpecs.traverse(
          tuningSpec => Log.infoBranch(s"Tuning (${tuningSpec.criterion.name})") {
            runTuningCalibration(allStats, tuningSpec, resultsDir) >>
              tuningSpec.run(allStats).map(tuningSpec.criterion.name -> _)
          }
        ).map(_.toList.toMap)
        _ <- {
          IO.unit
        }
        _ <- PlottingJVM.plotPrecisionRecallCurves(
          tuningResults,
          modelName, precisionAxisLabel, recallAxisLabel,
          resultsDir.resolve("tuning-strategies.png")
        )
        (bestCriterion, bestThreshold, bestStats) = tuningResults.toList.filter(_._1 != "oracle").map { case (name, stats) =>
          val criterion = SplitTuningCriterion.fromString(name).get
          val (threshold, result) = SplitTuningCriterion.chooseBest(stats).data.head
          (criterion, threshold, result)
        }.maxBy(_._3.f1)
        bestSettingString = bestCriterion.name + "=" + bestThreshold
        _ <- FileUtil.writeString(resultsDir.resolve("best-setting.txt"))(bestSettingString)
        _ <- FileUtil.writeString(resultsDir.resolve("best-result.txt"))(getMetricsString(bestStats))
        _ <- PlottingJVM.plotWeightedScatter(
          allStats.values.toList.foldMap { stats =>
            val res = bestCriterion.selectPoint(bestThreshold)(stats)
            Map(
              "precision" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.precision, res.numItems)),
              "recall" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.recall, res.numItems)),
              "f1" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.f1, res.numItems))
            )
          },
          title = s"Tuned stats ($bestSettingString) by verb frequency",
          xAxisLabel = "Num instances",
          yAxisLabel = "Score",
          path = resultsDir.resolve("by-freq-stats.png")
        )
      } yield bestCriterion -> bestThreshold
    }
  }

  def runClusteringEvalWithMetric[VerbType, InstanceId, GoldLabel : Order : Show](
    parentDir: NIOPath,
    modelName: String,
    argClusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      getAllPRStats(argClusterings, getGoldLabel, metric, modelName.startsWith("gold")) >>= (allStats =>
        for {
          resultsDir <- IO.pure(parentDir.resolve(metric.name)).flatTap(createDir)
          _ <- PlottingJVM.plotAllStatsVerbwise(
            modelName, allStats,
            metric.precisionName, metric.recallName,
            name => resultsDir.resolve(s"by-verb-$name.png")
          )
          (bestCriterion, bestThreshold) <- runClusterTuning(
            modelName, allStats,
            tuningSpecs,
            metric.precisionName, metric.recallName,
            resultsDir
          )
          allLabeledClusters = argClusterings.toVector.map { case (verbType, clustering) =>
            val goldLabelFn = getGoldLabel(verbType)
            val clusters = clustering.clusterTreeOpt.foldMap(tree =>
              bestCriterion
                .splitTree(tree, (x: Set[InstanceId]) => x.size, bestThreshold)
                .map(_.unorderedFold)
            ) ++ clustering.extraClusters.values
            clusters.map(_.unorderedFoldMap(id => Map(goldLabelFn(id) -> 1)))
          }
          npmis = EvalUtils.calculateAggregateNPMIs(allLabeledClusters)
          _ <- IO(
            Plotting.plotNPMI[GoldLabel](
              npmis,
              f"Normalized PMIs ($bestCriterion%s=$bestThreshold%.2f)"
            ).render().write(new java.io.File(resultsDir.resolve("best-pnmi.png").toString))
          )
        } yield ()
      )
    }
  }

  val activeMetrics: List[ClusterPRMetric] = {
    import ClusterPRMetric._
    List(
      b3instance,
      // b3mfs,
      // b3lfs,
      // b3label,
      purityCollocation
      // b3verb,
      // purityCollocationByVerb
    )
  }

  def evaluateClusters[VerbType, InstanceId, GoldLabel : Order : Show](
    resultsDir: NIOPath,
    modelName: String,
    clusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = activeMetrics.traverse(metric =>
    runClusteringEvalWithMetric(resultsDir, modelName, clusterings, getGoldLabel, tuningSpecs, metric)
  ).void

  def evaluateArgumentClusters[VerbType, Arg](
    resultsDir: NIOPath,
    modelName: String,
    argClusterings: Map[VerbType, Clustering[ArgumentId[Arg]]],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) {
      val getLabel = (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
      evaluateClusters(resultsDir, modelName, argClusterings, getLabel, tuningSpecs)
    } else {
      val getLabel = (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
      evaluateClusters(resultsDir, modelName, argClusterings, getLabel, tuningSpecs)
    }

  }

  // summarize

  def evaluateModels[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering[InstanceId]], SplitTuningSpec)],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    for {
      tunedStatsByModel <- models.toList.infoBarTraverse("Getting tuned P/R curves for models") { case (model, (argClusterings, tuningSpec)) =>
        Log.info(model) >> getAllPRStats(argClusterings, getGoldLabel, metric, model.startsWith("gold"))
          .flatMap(tuningSpec.run(_))
          .map(model -> _)
      }.map(_.toMap)
      _ <- Log.infoBranch(s"Plotting best tuned P/R curves") {
        PlottingJVM.plotPrecisionRecallCurves(
          tunedStatsByModel,
          "Best tuned performance", metric.precisionName, metric.recallName,
          resultsDir.resolve("best.png")
        )
      }
      _ <- FileUtil.writeString(resultsDir.resolve("best.txt"))(
        getMetricsString(
          Chosen(
            tunedStatsByModel.transform { case (model, stats) =>
              Chosen(
                stats.map { case (k, v) => f"${models(model)._2.criterion}%s=$k%.3f" -> v }.toMap
              ).keepMaxBy(_.f1)
            }
          )
        )
      )
      _ <- IO.pure(!includeOracle).ifM(
        IO.unit, for {
          oracleStatsByModel <- models.toList.infoBarTraverse("Getting oracle P/R curves for models") { case (model, (argClusterings, _)) =>
            Log.info(model) >> getAllPRStats(argClusterings, getGoldLabel, metric, model.startsWith("gold"))
              .flatMap(SplitTuningSpec(OracleCriterion).run(_))
              .map(model -> _)
          }.map(_.toMap)
          _ <- Log.infoBranch(s"Plotting oracle P/R curves") {
            PlottingJVM.plotPrecisionRecallCurves(
              oracleStatsByModel,
              "Oracle tuned performance", metric.precisionName, metric.recallName,
              resultsDir.resolve("oracle.png")
            )
          }
          _ <- FileUtil.writeString(resultsDir.resolve("oracle.txt"))(
            getMetricsString(
              Chosen(
                oracleStatsByModel.transform { case (model, stats) =>
                  Chosen(
                    stats.map { case (k, v) => f"oracle=$k%.3f" -> v }.toMap
                  ).keepMaxBy(_.f1)
                }
              )
            )
          )
        } yield ()
      )
    } yield ()
  }

  def evaluateArgumentModels[VerbType, Arg](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering.Argument[Arg]], SplitTuningSpec)],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    val getRoleLabel = if(useSenseSpecificRoles) {
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
    } else {
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
    }
    evaluateModels(resultsDir, metric, models, getRoleLabel, includeOracle)
  }

  def evaluateVerbModels[VerbType](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering.Verb], SplitTuningSpec)],
    getVerbSenseLabel: VerbType => VerbId => String,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    evaluateModels(
      resultsDir, metric, models,
      (verbType: VerbType) => (verbId: VerbId) => getVerbSenseLabel(verbType)(verbId),
      includeOracle
    )
  }

  // def doPropBankClusterDebugging(
  //   config: Config,
  //   senseLabels: Instances.PropBankLabels,
  //   verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
  //   chosenThresholds: Map[VerbSenseConfig, Double])(
  //   implicit Log: TreeLogger[IO, String]
  // ): IO[Unit] = for {
  //   _ <- Log.infoBranch("Writing loss graph")(
  //     writeLossGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("loss-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing depth graph")(
  //     writeDepthGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("depth-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing PropBank gold sense graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       lemma: String,
  //       numOccurrences: Int,
  //       numSenses: Int,
  //       val x: Double,
  //       val y: Double) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.x) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = senseLabels.values.iterator.map { case (lemma, sentenceMap) =>
  //       val instances = sentenceMap.iterator.flatMap(_._2.values.iterator).toList
  //       val senses = instances.toSet
  //       ThisPoint(lemma, instances.size, senses.size, scala.math.min(1000, instances.size + noise), senses.size + (noise * 10))
  //     }.toList

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => "gold"), size = Some(2.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("propbank-sense-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition sizes graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numInstances: Int,
  //       numClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.values.toList.map { model =>
  //         val numInstances = model.clusterTree.size.toInt
  //         val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //         val numClusters = clusters.size
  //         ThisPoint(vsConfig, numInstances, numClusters, scala.math.min(1000, numInstances.toDouble + noise), numClusters.toDouble + (noise * 10))
  //       }
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("predicted-cluster-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition size comparison graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numGoldClusters: Int,
  //       numPredictedClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 4.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.toList.flatMap { case (verbLemma, model) =>
  //         senseLabels.values.get(verbLemma).map { verbSenseLabels =>
  //           val numGoldClusters = verbSenseLabels.values.iterator.flatMap(_.values.iterator).toSet.size
  //           val numInstances = model.clusterTree.size
  //           val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //           val numPredictedClusters = clusters.size
  //           ThisPoint(vsConfig, numGoldClusters, numPredictedClusters, numGoldClusters.toDouble + noise, numPredictedClusters.toDouble + noise)
  //         }
  //       }
  //     }

  //     val pearsonR = {
  //       val num = data.map(d => d.numGoldClusters * d.numPredictedClusters).sum
  //       val denomGold = data.map(d => d.numGoldClusters * d.numGoldClusters).sum
  //       val denomPredicted = data.map(d => d.numPredictedClusters * d.numPredictedClusters).sum
  //       num.toDouble / scala.math.exp((scala.math.log(denomGold) + scala.math.log(denomPredicted)) / 2)
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     Log.info("Pearson's R between gold and predicted number of senses: " + pearsonR) >>
  //       config.globalPropBankResultsDir.flatMap(path =>
  //         IO(plot.render().write(new java.io.File(path.resolve("cluster-num-correlation.png").toString)))
  //       )
  //   }
  //   // _ <- verbModelsByConfig.toList.traverse { case (vsConfig, verbModels) =>
  //   //   val maxLoss = chosenThresholds(vsConfig)
  //   //   verbModels.toList.traverse { case (verbLemma, verbModel) =>
  //   //     val clusters = verbModel.clusterTree.splitWhile(_.loss > maxLoss)
  //   //     IO.unit
  //   //   }
  //   // }
  // } yield ()
}
