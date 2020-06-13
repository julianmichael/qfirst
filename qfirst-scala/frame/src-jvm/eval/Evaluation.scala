package qfirst.frame.eval

import qfirst.frame._
import qfirst.frame.features.Features
import qfirst.frame.features.PropBankRoleLabel
import qfirst.frame.util.Duad
import qfirst.frame.util.NonMergingMap

import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

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

  def getAllPRStats[VerbType, InstanceId, GoldLabel](
    trees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, NonEmptyList[ConfStatsPoint]]] = trees.toList
    .infoBarTraverse(s"Calculating ${metric.name} for all clusterings") { case (verbType, tree) =>
      val getGoldLabelForVerb = getGoldLabel(verbType)
      Log.trace(s"$verbType (${tree.size} leaves)") >> IO {
        val goldCountTree = tree.map(
          _.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1))
        )
        verbType -> getAllClusteringConfs(goldCountTree, metric)
      }
    }.map(_.toMap)

  def getSingleClusteringPRStats[VerbType, InstanceId, GoldLabel](
    clusterings: Map[VerbType, Vector[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, NonEmptyList[ConfStatsPoint]]] = clusterings.toList
    .infoBarTraverse(s"Calculating ${metric.name} for single clusterings") { case (verbType, clusters) =>
      val getGoldLabelForVerb = getGoldLabel(verbType)
      Log.trace(s"$verbType (${clusters.size} clusters)") >> IO {
        val goldCounts = clusters.map(
          _.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1))
        )
        val goldClusterSizes = goldCounts.combineAll
        verbType -> NonEmptyList.of(
          ConfStatsPoint(
            0.0,
            clusters.map(_.size),
            metric(goldClusterSizes, goldCounts)
          )
        )
      }
    }.map(_.toMap)

  val activeTuningCriteria = List[SplitTuningCriterion](
    OracleCriterion,
    NumClustersCriterion,
    // SqNumClustersPenaltyCriterion,
    // CuttingDeltaCriterion,
    // LossPerItemCriterion,
    TotalEntropyCriterion
  )

  def runClusterTuning[VerbType](
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    resultsDir: NIOPath)(
    implicit logger: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    freelog.loggers.TimingDelayedLogger.file[Unit](resultsDir.resolve("results.txt")) { fileLogger =>
      val Log = logger |+| fileLogger
      for {
        _ <- Log.info {
          val stats = SplitTuningCriterion.getTunedWeightedStats(
            allStats, _.toList.maxBy(_.precision)
          )
          s"Max precision: ${getMetricsString(stats)}"
        }
        tuningResults <- activeTuningCriteria.traverse(
          tuningCriterion => Log.infoBranch(s"Tuning (${tuningCriterion.name})") {
            tuningCriterion.runTuning(allStats).map(tuningCriterion.name -> _)
          }
        ).map(_.toMap)
        _ <- Plotting.plotTuningResults(
          modelName,
          tuningResults,
          precisionAxisLabel, recallAxisLabel,
          resultsDir
        )
      } yield ()
    }
  }

  def runClusteringEvalWithMetric[VerbType, InstanceId, GoldLabel](
    parentDir: NIOPath,
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      for {
        resultsDir <- IO.pure(parentDir.resolve(metric.name)).flatTap(createDir)
        _ <- Plotting.plotAllStatsVerbwise(
          modelName, allStats,
          metric.precisionName, metric.recallName,
          name => resultsDir.resolve(s"by-verb-$name.png")
        )
        _ <- runClusterTuning(
          modelName, allStats,
          metric.precisionName, metric.recallName,
          resultsDir
        )
      } yield ()
    }
  }

  def runHierarchicalClusteringEvalWithMetric[VerbType, InstanceId, GoldLabel](
    parentDir: NIOPath,
    modelName: String,
    argTrees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      getAllPRStats(argTrees, getGoldLabel, metric) >>= (allStats =>
        runClusteringEvalWithMetric(parentDir, modelName, allStats, metric)
      )
    }
  }

  def runSingleClusteringEvalWithMetric[VerbType, InstanceId, GoldLabel](
    parentDir: NIOPath,
    modelName: String,
    clusterings: Map[VerbType, Vector[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      getSingleClusteringPRStats(clusterings, getGoldLabel, metric) >>= (allStats =>
        runClusteringEvalWithMetric(parentDir, modelName, allStats, metric)
      )
    }
  }

  val activeMetrics: List[ClusterPRMetric] = {
    import ClusterPRMetric._
    List(
      bCubedPerInstance,
      // bCubedMFS,
      // bCubedLFS,
      // bCubedPerLabel,
      // bCubedPerVerbType,
      purityCollocation
    )
  }

  def evaluateSingleClustering[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    modelName: String,
    clusterings: Map[VerbType, Vector[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = activeMetrics.traverse(metric =>
    runSingleClusteringEvalWithMetric(resultsDir, modelName, clusterings, getGoldLabel, metric)
  ).void

  def evaluateClusters[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    modelName: String,
    trees: Map[VerbType, MergeTree[Set[InstanceId]]],
    getGoldLabel: VerbType => InstanceId => GoldLabel)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = activeMetrics.traverse(metric =>
    runHierarchicalClusteringEvalWithMetric(resultsDir, modelName, trees, getGoldLabel, metric)
  ).void

  def evaluateArgumentClusters[VerbType, Arg](
    resultsDir: NIOPath,
    modelName: String,
    argTrees: Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) evaluateClusters(
      resultsDir, modelName,
      argTrees, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
    ) else evaluateClusters(
      resultsDir, modelName,
      argTrees, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
    )
  }

  def evaluateSingleArgumentClustering[VerbType, Arg](
    resultsDir: NIOPath,
    modelName: String,
    argClusterings: Map[VerbType, Vector[Set[ArgumentId[Arg]]]],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) evaluateSingleClustering(
      resultsDir, modelName,
      argClusterings, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
    ) else evaluateSingleClustering(
      resultsDir, modelName,
      argClusterings, (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
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
  //   //     IO.unit // TODO
  //   //   }
  //   // }
  // } yield ()
}
