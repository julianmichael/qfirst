package qfirst.frame.eval

import qfirst.frame._
import qfirst.frame.features.Features
import qfirst.frame.features.PropBankRoleLabel
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
import cats.data.NonEmptyList
import cats.implicits._

import cats.effect.IO
import cats.effect.Timer
import cats.effect.concurrent.Ref

import java.nio.file.{Path => NIOPath}

import scala.annotation.tailrec

object Evaluation {

  def getAllClusteringConfs[A](
    goldLabelTree: MergeTree[Map[A, Int]],
    metric: ClusterPRMetric
  ): NonEmptyList[ConfStatsPoint] = {
    val goldClusterSizes = goldLabelTree.unorderedFold
    NonEmptyList.fromList(
      goldLabelTree.clusterSplittings.map { clusters =>
        val loss = clusters.foldMap(_.loss)
        val clusterSizes = clusters.map(_.unorderedFoldMap(_.unorderedFold))
        val weightedPR = metric(goldClusterSizes, clusters.map(_.unorderedFold))
        ConfStatsPoint(loss, clusterSizes, weightedPR)
      }.toList
    ).get
  }

  def getLeafClusteringConf[A](
    goldLabelTree: MergeTree[Map[A, Int]],
    metric: ClusterPRMetric
  ): ConfStatsPoint = {
    val goldClusterSizes = goldLabelTree.unorderedFold
    val clusters = goldLabelTree.valuesWithLosses
    val loss = clusters.foldMap(_._1)
    val clusterSizes = clusters.map(_._2.unorderedFold)
    val weightedPR = metric(goldClusterSizes, clusters.map(_._2))
    ConfStatsPoint(loss, clusterSizes, weightedPR)
  }

  def getAllPRStats[VerbType, InstanceId, GoldLabel](
    trees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric,
    maxClustersOnly: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, NonEmptyList[ConfStatsPoint]]] = trees.toList
    .infoBarTraverse(s"Calculating ${metric.name} for all clusterings") { case (verbType, tree) =>
      val getGoldLabelForVerb = getGoldLabel(verbType)
      Log.trace(s"$verbType (${tree.size} leaves)") >> IO {
        val goldCountTree = tree.map(
          _.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1))
        )
        verbType -> {
          // we only consider default clustering of gold-derived models
          if(maxClustersOnly) NonEmptyList.of(getLeafClusteringConf(goldCountTree, metric))
          else getAllClusteringConfs(goldCountTree, metric)
        }
      }
    }.map(_.toMap)

  def runClusterTuning[VerbType](
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    resultsDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    def outerLogger = Log
    freelog.loggers.TimingDelayedLogger.file[Unit](resultsDir.resolve("results.txt")) { fileLogger =>
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
            tuningSpec.run(allStats).map(tuningSpec.criterion.name -> _)
          }
        ).map(_.toList.toMap)
        _ <- Plotting.plotPrecisionRecallCurves(
          tuningResults,
          modelName, precisionAxisLabel, recallAxisLabel,
          resultsDir.resolve("tuning-strategies.png")
        )
        bestResult = tuningResults.toList.filter(_._1 != "oracle").map { case (name, stats) =>
          name -> SplitTuningCriterion.chooseBest(stats).data.head
        }.maxBy(_._2._2.f1)
        _ <- FileUtil.writeString(resultsDir.resolve("best-setting.txt"))(bestResult._1 + "=" + bestResult._2._1)
        _ <- FileUtil.writeString(resultsDir.resolve("best-result.txt"))(getMetricsString(bestResult._2._2))
      } yield ()
    }
  }

  def runClusteringEvalWithMetric[VerbType, InstanceId, GoldLabel](
    parentDir: NIOPath,
    modelName: String,
    argTrees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      getAllPRStats(argTrees, getGoldLabel, metric, modelName.startsWith("gold")) >>= (allStats =>
        for {
          resultsDir <- IO.pure(parentDir.resolve(metric.name)).flatTap(createDir)
          _ <- Plotting.plotAllStatsVerbwise(
            modelName, allStats,
            metric.precisionName, metric.recallName,
            name => resultsDir.resolve(s"by-verb-$name.png")
          )
          _ <- runClusterTuning(
            modelName, allStats,
            tuningSpecs,
            metric.precisionName, metric.recallName,
            resultsDir
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
      // b3verb,
      purityCollocation
    )
  }

  def evaluateClusters[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    modelName: String,
    trees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = activeMetrics.traverse(metric =>
    runClusteringEvalWithMetric(resultsDir, modelName, trees, getGoldLabel, tuningSpecs, metric)
  ).void

  def evaluateModels[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, MergeTree[Set[InstanceId]]], SplitTuningSpec)],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    for {
      tunedStatsByModel <- models.toList.infoBarTraverse("Getting tuned P/R curves for models") { case (model, (argTrees, tuningSpec)) =>
        Log.info(model) >> getAllPRStats(argTrees, getGoldLabel, metric, model.startsWith("gold"))
          .flatMap(tuningSpec.run(_))
          .map(model -> _)
      }.map(_.toMap)
      _ <- Log.infoBranch(s"Plotting best tuned P/R curves") {
        Plotting.plotPrecisionRecallCurves(
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
          oracleStatsByModel <- models.toList.infoBarTraverse("Getting oracle P/R curves for models") { case (model, (argTrees, _)) =>
            Log.info(model) >> getAllPRStats(argTrees, getGoldLabel, metric, model.startsWith("gold"))
              .flatMap(SplitTuningSpec(OracleCriterion).run(_))
              .map(model -> _)
          }.map(_.toMap)
          _ <- Log.infoBranch(s"Plotting oracle P/R curves") {
            Plotting.plotPrecisionRecallCurves(
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

  def evaluateArgumentClusters[VerbType, Arg](
    resultsDir: NIOPath,
    modelName: String,
    argTrees: Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) evaluateClusters(
      resultsDir, modelName,
      argTrees, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId),
      tuningSpecs
    ) else evaluateClusters(
      resultsDir, modelName,
      argTrees, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role,
      tuningSpecs
    )
  }

  def evaluateArgumentModels[VerbType, Arg](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]], SplitTuningSpec)],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) evaluateModels(
      resultsDir, metric, models,
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId),
      includeOracle
    ) else evaluateModels(
      resultsDir, metric, models,
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role,
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
