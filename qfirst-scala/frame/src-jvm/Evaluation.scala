package qfirst.frame

import qfirst.frame.util.Duad
import qfirst.frame.util.NonMergingMap

import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

import jjm.ling.ESpan
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

  case class ConfStatsPoint(loss: Double, clusterSizes: Vector[Int], precision: Double, recall: Double) {
    def numClusters = clusterSizes.size
    def numItems = clusterSizes.sum
    def lossPerItem = loss / numItems
    def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
    def f1 = Functions.harmonicMean(precision, recall)
  }

  def getAllClusteringPurityCollocationConfs[A](goldLabelTree: MergeTree[A]): NonEmptyList[ConfStatsPoint] = {
    val goldClusterSizes = goldLabelTree.values.counts
    NonEmptyList.fromList(
      goldLabelTree.clusterSplittings.map { clusters =>
        val clusterLabelCounts = clusters.map(_.values.counts)
        val purity = clusterLabelCounts.foldMap { counts =>
          val primaryLabelCount = counts.values.max
          val total = counts.values.sum
          Proportion.Stats(
            included = primaryLabelCount,
            excluded = total - primaryLabelCount
          )
        }
        val collocation = goldClusterSizes.toList.foldMap { case (goldLabel, numLabels) =>
          val numInPrimaryCluster = clusterLabelCounts.map(_.getOrElse(goldLabel, 0)).max
          Proportion.Stats(
            included = numInPrimaryCluster,
            excluded = numLabels - numInPrimaryCluster
          )
        }
        val loss = clusters.foldMap(_.loss)
        val clusterSizes = clusters.map(_.size.toInt)
        ConfStatsPoint(loss, clusterSizes, purity.proportion, collocation.proportion)
      }.toList
    ).get
  }

  def getAllPurityCollocationStats(
    argTrees: Map[String, MergeTree[ArgumentId[ESpan]]],
    argRoleLabels: Map[String, NonMergingMap[ArgumentId[ESpan], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[String, NonEmptyList[ConfStatsPoint]]] = argTrees.toList.infoBarTraverse("Calculating purity/collocation for all clusterings") { case (verbType, tree) =>
      Log.trace(verbType) >> IO {
        val labels = argRoleLabels(verbType).value
        verbType -> getAllClusteringPurityCollocationConfs(tree.map(labels))
      }
    }.map(_.toMap)

  def getAllClusteringBCubedConfs[A: Order](trues: Set[Duad[A]], tree: MergeTree[A]): NonEmptyList[ConfStatsPoint] = {
    val numArgs = tree.size.toInt
    val numPairs = numArgs * (numArgs + 1) / 2
    val initConf = BinaryConf.Stats(tp = trues.size, fp = numPairs - trues.size)
    val initStats = ConfStatsPoint(tree.loss, Vector(numArgs), initConf.precision, initConf.recall)
    @tailrec
    def recurseConfs(acc: NonEmptyList[ConfStatsPoint], lastConf: BinaryConf.Stats, trees: Vector[MergeTree[A]]): NonEmptyList[ConfStatsPoint] = {
      trees.flatMap(_.deltaOpt).maximumOption match {
        case None => acc
        case Some(maxDelta) =>
          val (oldTrees, treesToSplit) = trees.map(t =>
            MergeTree.merge.getOption(t).filter(_.delta >= maxDelta) match {
              case None => Left(t)
              case Some(m) => Right(m)
            }
          ).separate
          val newConf = lastConf |+| treesToSplit.foldMap { (m: MergeTree.Merge[A]) =>
            val positivesBeingLost = for {
              l <- m.left.values.toSet: Set[A]
              r <- m.right.values.toSet
            } yield l <-> r
            val numTPsLost = positivesBeingLost.filter(trues.contains).size
            val numFPsLost = positivesBeingLost.size - numTPsLost
            BinaryConf.Stats(
              tp = -numTPsLost,
              fp = -numFPsLost,
              tn = numFPsLost,
              fn = numTPsLost
            )
          }
          val newTrees = oldTrees ++ treesToSplit.flatMap(m => Vector(m.left, m.right))
          val newLoss = newTrees.foldMap(_.loss)
          recurseConfs(ConfStatsPoint(newLoss, newTrees.map(_.size.toInt), newConf.precision, newConf.recall) :: acc, newConf, newTrees)
      }
    }
    recurseConfs(NonEmptyList.of(initStats), initConf, Vector(tree))
  }

  def getAllBCubedStats(
    argTrees: Map[String, MergeTree[ArgumentId[ESpan]]],
    argRoleLabels: Map[String, NonMergingMap[ArgumentId[ESpan], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[String, NonEmptyList[ConfStatsPoint]]] = argTrees.toList.infoBarTraverse("Calculating B-cubed for all clusterings") { case (verbType, tree) =>
      Log.trace(verbType) >> IO {
        val args = tree.values.toSet
        val labels = argRoleLabels(verbType).value.filter { case (k, _) => args.contains(k) }
        val argsByGoldLabel = labels.toList
          .groupBy(p => if(useSenseSpecificRoles) p._2 else p._2.role)
          .map { case (label, argPairs) => label -> argPairs.map(_._1) }
        val trues = argsByGoldLabel.values.toList.foldMap { args =>
          @tailrec def loop(
            acc: Set[Duad[ArgumentId[ESpan]]],
            rest: List[ArgumentId[ESpan]]
          ): Set[Duad[ArgumentId[ESpan]]] = rest match {
            case head :: tail => loop(acc ++ rest.map(_ <-> head).toSet, tail)
            case Nil => acc
          }
          loop(Set.empty, args.toList)
        }
        verbType -> getAllClusteringBCubedConfs(trues, tree)
      }
    }.map(_.toMap)

  def plotAllStatsVerbwise(
    modelName: String,
    allStats: Map[String, NonEmptyList[ConfStatsPoint]],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    path: NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch(s"Plotting metrics for all verbs ($precisionAxisLabel / $recallAxisLabel)") {
      import com.cibo.evilplot._
      import com.cibo.evilplot.colors._
      import com.cibo.evilplot.geometry._
      import com.cibo.evilplot.numeric._
      import com.cibo.evilplot.plot._
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      import com.cibo.evilplot.plot.renderers.PointRenderer
      import com.cibo.evilplot.plot.renderers.PathRenderer

      case class PRPoint(
        verbType: String,
        loss: Double,
        numItems: Int,
        numClusters: Int,
        isBestF1: Boolean,
        recall: Double,
        precision: Double) extends Datum2d[PRPoint] {
        val x = recall
        val y = precision
        def f1 = 2 * precision * recall / (precision + recall)

        def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
      }

      // val rand = new scala.util.Random(2643642L)
      // def noise = scala.math.abs(rand.nextGaussian / 200.0)

      val allData = allStats.transform { case (verbType, allStats) =>

        val bestF1 = allStats.map(_.f1).maximum
        allStats.map { case p @ ConfStatsPoint(loss, clusterSizes, precision, recall) =>
          PRPoint(verbType, loss, clusterSizes.sum, clusterSizes.size, p.f1 == bestF1, recall, precision)
        }
      }

      val maxNumClusters = allData.values.toList.flatMap(_.toList).filter(_.isBestF1).map(_.numClusters).max
      val maxNumInstances = allData.values.toList.map(_.head.numItems).max

      val plot = Overlay.fromSeq(
        allData.toList.map { case (verbType, data) =>
          val propMaxInstances = data.head.numItems.toDouble / maxNumInstances
          LinePlot(
		        data.toList,
            pathRenderer = Some(PathRenderer.default[PRPoint](strokeWidth = Some(0.2), color = Some(RGBA(0, 0, 0, propMaxInstances))))
	        )
        } ++
          allData.toList.map { case (verbType, data) =>
            ScatterPlot(
		          data.toList.filter(_.isBestF1),
              pointRenderer = Some(
                PointRenderer.depthColor[PRPoint](
                  depth = _.numClusters.toDouble,
                  size = Some(2.0), min = 1.0, max = maxNumClusters,
                  coloring = Some(ContinuousColoring.gradient(start = RGBA(0, 0, 0, 1.0), end = RGBA(255, 0, 0, 1.0)))
                )
              )
	          )
          }
      ).title(s"PropBank argument clustering ($modelName)")
        .xLabel(recallAxisLabel)
        .yLabel(precisionAxisLabel)
        .xbounds(0.0, 1.0).ybounds(0.0, 1.0)
        .xAxis().yAxis()
        .frame().rightLegend()

      IO(plot.render().write(new java.io.File(path.toString)))
    }

  case class WeightedPR(
    precisions: WeightedNumbers[Double],
    recalls: WeightedNumbers[Double]
  ) {
    def pseudocount = precisions.stats.pseudocount
    def precision = precisions.stats.weightedMean
    def recall = recalls.stats.weightedMean
    def f1 = Functions.harmonicMean(precision, recall)
    def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
  }
  object WeightedPR {
    implicit val weightedPRMonoid: Monoid[WeightedPR] = {
      import cats.derived.auto.monoid._
      cats.derived.semi.monoid
    }
    implicit val weightedPRHasMetrics = new HasMetrics[WeightedPR] {
      def getMetrics(pr: WeightedPR) = MapTree.fromPairs(
        // "pseudocount" -> Metric.double(pr.pseudocount),
        "precision" -> Metric.double(pr.precision),
        "recall" -> Metric.double(pr.recall),
        "f1" -> Metric.double(pr.f1)
      )
    }
  }

  def getTunedWeightedStats(
    allResults: Map[String, NonEmptyList[ConfStatsPoint]],
    choose: NonEmptyList[ConfStatsPoint] => ConfStatsPoint
  ): WeightedPR = {
    allResults.toList.foldMap { case (verbType, results) =>
      val result = choose(results)
      val weight = results.head.numItems
      WeightedPR(
        precisions = WeightedNumbers(result.precision, weight = weight),
        recalls = WeightedNumbers(result.recall, weight = weight),
      )
    }
  }

  def tuneWeightedStats[A](
    thresholds: List[A],
    allResults: Map[String, NonEmptyList[ConfStatsPoint]])(
    choose: A => NonEmptyList[ConfStatsPoint] => ConfStatsPoint)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(A, WeightedPR)]] = {
    thresholds.infoBarTraverse("Tuning weighted stats") { threshold =>
      IO.pure(threshold -> getTunedWeightedStats(allResults, choose(threshold)))
    }
  }

  def runClusterTuning(
    modelName: String,
    allStats: Map[String, NonEmptyList[ConfStatsPoint]],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    resultsDir: NIOPath)(
    implicit logger: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    freelog.loggers.TimingDelayedLogger.file[Unit](resultsDir.resolve("results.txt")) { fileLogger =>
      val Log = logger |+| fileLogger
      for {
        allTuningResults <- Ref[IO].of(Map.empty[String, List[(Double, WeightedPR)]])
        _ <- Log.infoBranch("Oracle tuning") {
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
            _ <- allTuningResults.update(_ + ("oracle" -> tuningResults))
          } yield ()
        }
        _ <- Log.infoBranch("Loss-per-item tuning") {
          val lossPerItemAll = allStats.toList.foldMap { case (verbType, results) =>
            results.foldMap(s => Numbers(s.lossPerItem))
          }
          val lossPerItemBest = allStats.toList.foldMap { case (verbType, results) =>
            val bestF1 = results.map(_.f1).maximum
            results.filter(_.f1 == bestF1).foldMap(s => Numbers(s.lossPerItem))
          }
          val interval = 0.05
          val stats = lossPerItemAll.stats
          val lossThresholds = (
            scala.math.round(stats.quartiles.min / interval).toInt to scala.math.round(stats.quartiles.max / interval).toInt
          ).map(_ * interval).toList
          for {
            tuningResults <- tuneWeightedStats(
              lossThresholds, allStats)(
              t => stats => {
                val sortedStats = stats.sortBy(-_.lossPerItem)
                val qualified = sortedStats.toList.takeWhile(_.lossPerItem > t)
                qualified.lastOption.getOrElse(sortedStats.head)
              }
            )
            tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
            _ <- Log.info(s"Loss per item (all): ${getMetricsString(lossPerItemAll)}")
            _ <- Log.info(s"Loss per item (best): ${getMetricsString(lossPerItemBest)}")
            _ <- Log.info(s"Tuned results (best loss per item): ${getMetricsString(tunedBest)}")
            _ <- allTuningResults.update(_ + ("loss per item" -> tuningResults))
          } yield ()
        }
        _ <- Log.infoBranch("Constant num clusters tuning") {
          val thresholds = (1 to 50).toList.map(_.toDouble)
          for {
            tuningResults <- tuneWeightedStats(
              thresholds, allStats)(
              t => stats => {
                val sortedStats = stats.sortBy(_.numClusters)
                val qualified = sortedStats.toList.dropWhile(_.numClusters < t)
                qualified.headOption.getOrElse(sortedStats.last)
              }
            )
            tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
            _ <- Log.info(s"Tuned results (best num clusters): ${getMetricsString(tunedBest)}")
            _ <- allTuningResults.update(_ + ("constant num clusters" -> tuningResults))
          } yield ()
        }
        _ <- Log.infoBranch("Square num clusters penalty tuning") {
          val coeffs = (0 to 100).map(_.toDouble / 5).toList
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
            _ <- allTuningResults.update(_ + ("sq num clusters penalty" -> tuningResults))
          } yield tuningResults
        }
        _ <- Log.infoBranch("Cutting delta tuning") {
          val deltasPerItem = (0 to 100).map(_.toDouble / 10).toList
          for {
            tuningResults <- tuneWeightedStats(
              deltasPerItem, allStats)(
              t => stats => {
                val deltaThreshold = t * stats.head.numItems
                val sortedStats = stats.sortBy(_.loss)
                sortedStats.toList.sliding(2)
                  .filter(_.size == 2)
                  .takeWhile(w => w(1).loss - w(0).loss < deltaThreshold)
                  .toList.lastOption
                  .fold(sortedStats.head)(_(0))
              }
            )
            tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
            _ <- Log.info(s"Tuned results (per-item cutting delta): ${getMetricsString(tunedBest)}")
            _ <- allTuningResults.update(_ + ("cutting delta" -> tuningResults))
          } yield tuningResults
        }
        _ <- Log.infoBranch("Total entropy loss tuning") {
          val coeffs = (0 to 40).map(_.toDouble / 20).toList
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
            _ <- allTuningResults.update(_ + ("total entropy loss" -> tuningResults))
          } yield tuningResults
        }
        tuningResults <- allTuningResults.get
        _ <- Log.infoBranch("Plotting tuning results") {
          import com.cibo.evilplot._
          import com.cibo.evilplot.colors._
          import com.cibo.evilplot.geometry._
          import com.cibo.evilplot.numeric._
          import com.cibo.evilplot.plot._
          import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
          import com.cibo.evilplot.plot.renderers.PointRenderer
          import com.cibo.evilplot.plot.renderers.PathRenderer

          case class PRPoint(
            category: String,
            value: Double,
            isBestF1: Boolean,
            recall: Double,
            precision: Double) extends Datum2d[PRPoint] {
            val x = recall
            val y = precision
            def f1 = Functions.harmonicMean(precision, recall)

            def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
          }

          // val rand = new scala.util.Random(2643642L)
          // def noise = scala.math.abs(rand.nextGaussian / 200.0)
          def makePoints(category: String, prs: List[(Double, WeightedPR)]) = {
            val bestF1 = prs.map(_._2.f1).max
            prs.map { case (t, pr) => PRPoint(category, t, pr.f1 == bestF1, pr.recall, pr.precision) }
          }

          val allData = tuningResults.map { case (cat, items) =>
            cat -> makePoints(cat, items)
          }

          // val maxNumClusters = allData.flatMap(_.toList).filter(_.isBestF1).map(_.numClusters).max
          // val maxNumInstances = allData.values.toList.map(_.head.numItems).max
          val colors = Color.getDefaultPaletteSeq(allData.size)

          val plot = Overlay.fromSeq(
            colors.zip(allData).map { case (color, (category, data)) =>
              LinePlot.series(data, category, color, strokeWidth = Some(1.0))
            } ++
              colors.zip(allData).map { case (color, (category, data)) =>
                ScatterPlot(
		              data.toList.filter(_.isBestF1),
                  pointRenderer = Some(
                    PointRenderer.default[PRPoint](
                      color = Some(color), pointSize = Some(2.0)
                    )
                  )
	              )
              }
          ).title(s"PropBank argument clustering ($modelName)")
            .xLabel(recallAxisLabel)
            .yLabel(precisionAxisLabel)
            .xbounds(0.0, 1.0).ybounds(0.0, 1.0)
            .xAxis().yAxis()
            .frame().rightLegend()

          val path = resultsDir.resolve("tuning-strategies.png")
          IO(plot.render().write(new java.io.File(path.toString)))
        }
      } yield ()
    }
  }

  def evaluateArgumentClusters(
    modelName: String,
    features: PropBankGoldSpanFeatures,
    argTrees: Map[String, MergeTree[ArgumentId[ESpan]]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = for {
    _ <- Log.info("Initializing eval features")
    verbIds <- features.verbArgSets.eval.get
    args <- features.args.eval.get
    argRoleLabels <- features.argRoleLabels.eval.get
    // calculate B^3 precision-recall curve for each verb
    _ <- Log.infoBranch("Calculating B-Cubed metrics") {
      for {
        bCubedResults <- getAllBCubedStats(argTrees, argRoleLabels, useSenseSpecificRoles)
        resultsDir <- features.modelDir.map(_.resolve(s"$modelName/b-cubed")).flatTap(features.createDir)
        _ <- plotAllStatsVerbwise(
          modelName,
          bCubedResults,
          "B^3 Precision",
          "B^3 Recall",
          resultsDir.resolve("all-by-verb.png")
        )
        _ <- runClusterTuning(
          modelName,
          bCubedResults,
          "B^3 Precision",
          "B^3 Recall",
          resultsDir
        )
      } yield ()
    }
    _ <- Log.infoBranch("Calculating Purity/Collocation metrics") {
      for {
        allStats <- getAllPurityCollocationStats(argTrees, argRoleLabels, useSenseSpecificRoles)
        resultsDir <- features.modelDir.map(_.resolve(s"$modelName/pur-coll")).flatTap(features.createDir)
        _ <- plotAllStatsVerbwise(
          modelName,
          allStats,
          "Purity",
          "Collocation",
          resultsDir.resolve("all-by-verb.png")
        )
        _ <- runClusterTuning(
          modelName,
          allStats,
          "Purity",
          "Collocation",
          resultsDir
        )
      } yield ()
    }
  } yield ()


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
