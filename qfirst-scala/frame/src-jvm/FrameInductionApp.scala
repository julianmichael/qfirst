package qfirst.frame
import qfirst.frame.models._
import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
// import qfirst.frame.browse._
// import qfirst.model.eval.protocols.SimpleQAs
import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._
import QAInputApp.{SentenceQAOutput, ClauseQAOutput, ClauseQAQuestion}

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.Validated
import cats.implicits._

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource}

import com.monovore.decline._
import com.monovore.decline.effect._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files
import java.nio.file.Paths

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileUtil
import jjm.implicits._

import qasrl.ArgumentSlot
import qasrl.bank._
import qasrl.data._
import qasrl.labeling.SlotBasedLabel

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import breeze.linalg._
import scala.collection.immutable.Vector
import scala.collection.immutable.Map
import scala.annotation.tailrec

import freelog._
import freelog.implicits._

object FrameInductionApp extends CommandIOApp(
  name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
  header = "Induce verb frames.") {

  implicit val logLevel = LogLevel.Trace

  val allModelConfigs = {
    List(ModelConfig.SingleCluster, ModelConfig.EntropyOnly, ModelConfig.ELMoOnly) ++
      List(ModelConfig.Interpolated(0.5))
    // (1 to 9).map(_.toDouble / 10.0).toList.map(ModelConfig.Interpolated(_))
  }

  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder](
    name: String, features: Features[VerbType, Arg],
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[ArgumentId[Arg]]]]] = {
    // TODO organize models into subdirs
    features.modelDir.map(modelDir =>
      FileCached[Map[VerbType, MergeTree[ArgumentId[Arg]]]](
        s"Argument cluster model: $name")(
        path = modelDir.resolve(s"$name.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, MergeTree[ArgumentId[Arg]])](path)
          .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
        write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
        import ClusteringModel._
        val model = QuestionEntropy
        // val model = Composite.argument(
        //   QuestionEntropy -> 1.0,
        //   AnswerEntropy -> 1.0
        // )
        Log.infoBranch("Initializing model features")(features.args.full.get >> model.init(features)) >>
          features.verbArgSets.full.get >>= (
            _.toList
              .infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
              // hack to make it possible to even do the clustering on the really common words. need to generalize later
              val verbIds = NonEmptyVector.fromVector(verbs.value.keySet.toVector).get
              val verbIdSet = verbIds.toVector.toSet
              Log.trace(renderVerbType(verbType)) >> {
                features.args.full.get.map(_.apply(verbType)) >>= { argIds =>
                  model.create(features, verbType).map { algorithm =>
                    NonEmptyVector
                      .fromVector(argIds.filter(arg => verbIdSet.contains(arg.verbId)).toVector)
                      .map(algorithm.runFullAgglomerativeClustering(_))
                      .map { case (argClusterTree, finalParams) => verbType -> argClusterTree }
                  }
                }
              }
            }.map(_.flatten.toMap)
          )
      }
    )
  }

  case class ConfStatsPoint(loss: Double, clusterSizes: Vector[Int], conf: BinaryConf.Stats) {
    def numClusters = clusterSizes.size
    def numItems = clusterSizes.sum
    def lossPerItem = loss / numItems
  }

  def getAllClusteringBCubedConfs[A: Order](trues: Set[Duad[A]], tree: MergeTree[A]): NonEmptyList[ConfStatsPoint] = {
    val numArgs = tree.size.toInt
    val numPairs = numArgs * (numArgs + 1) / 2
    val initStats = ConfStatsPoint(tree.loss, Vector(numArgs), BinaryConf.Stats(tp = trues.size, fp = numPairs - trues.size))
    @tailrec
    def recurseConfs(acc: NonEmptyList[ConfStatsPoint], trees: Vector[MergeTree[A]]): NonEmptyList[ConfStatsPoint] = {
      trees.flatMap(_.deltaOpt).maximumOption match {
        case None => acc
        case Some(maxDelta) =>
          val (oldTrees, treesToSplit) = trees.map(t =>
            MergeTree.merge.getOption(t).filter(_.delta >= maxDelta) match {
              case None => Left(t)
              case Some(m) => Right(m)
            }
          ).separate
          val newConf = acc.head.conf |+| treesToSplit.foldMap { (m: MergeTree.Merge[A]) =>
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
          recurseConfs(ConfStatsPoint(newLoss, newTrees.map(_.size.toInt), newConf) :: acc, newTrees)
      }
    }
    recurseConfs(NonEmptyList.of(initStats), Vector(tree))
  }

  def getAllBCubedResults(
    argTrees: Map[String, MergeTree[ArgumentId[ESpan]]],
    argRoleLabels: Map[String, NonMergingMap[ArgumentId[ESpan], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[String, NonEmptyList[ConfStatsPoint]]] = argTrees.toList.infoBarTraverse("Calculating B-Cubed metrics") { case (verbType, tree) =>
      Log.trace(verbType) >> IO {
        import qfirst.metrics._
        // import metrics.implicits.__
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

  def plotAllBCubedVerbwise(
    modelName: String,
    bCubedResults: Map[String, NonEmptyList[ConfStatsPoint]],
    path: NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch("Plotting B-cubed metrics") {
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

      val allData = bCubedResults.transform { case (verbType, allStats) =>

        val bestF1 = allStats.map(_.conf.f1).maximum
        allStats.map { case ConfStatsPoint(loss, clusterSizes, conf) =>
          PRPoint(verbType, loss, clusterSizes.sum, clusterSizes.size, conf.f1 == bestF1, conf.recall, conf.precision)
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
      ).title(s"PropBank argument clustering ($modelName)").xLabel("B^3 Recall").yLabel("B^3 Precision").xAxis().yAxis().frame().rightLegend()

      IO(plot.render().write(new java.io.File(path.toString)))
    }

  def harmonicMean(x: Double, y: Double) = 2 * x * y / (x + y)

  case class WeightedPR(
    precisions: WeightedNumbers[Double],
    recalls: WeightedNumbers[Double]
  ) {
    def pseudocount = precisions.stats.pseudocount
    def precision = precisions.stats.weightedMean
    def recall = recalls.stats.weightedMean
    def f1 = harmonicMean(precision, recall)
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

  def getTunedWeightedBCubedStats(
    allResults: Map[String, NonEmptyList[ConfStatsPoint]],
    choose: NonEmptyList[ConfStatsPoint] => ConfStatsPoint
  ): WeightedPR = {
    allResults.toList.foldMap { case (verbType, results) =>
      val result = choose(results)
      val weight = results.head.numItems
      WeightedPR(
        precisions = WeightedNumbers(result.conf.precision, weight = weight),
        recalls = WeightedNumbers(result.conf.recall, weight = weight),
      )
    }
  }

  def tuneWeightedBCubedStats[A](
    thresholds: List[A],
    allResults: Map[String, NonEmptyList[ConfStatsPoint]])(
    choose: A => NonEmptyList[ConfStatsPoint] => ConfStatsPoint)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(A, WeightedPR)]] = {
    thresholds.infoBarTraverse("Tuning weighted stats") { threshold =>
      IO.pure(threshold -> getTunedWeightedBCubedStats(allResults, choose(threshold)))
    }
  }

  def evaluateArgumentClusters(
    modelName: String,
    features: PropBankGoldSpanFeatures,
    argTrees: Map[String, MergeTree[ArgumentId[ESpan]]],
    useSenseSpecificRoles: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    _ <- Log.info("Initializing eval features")
    verbIds <- features.verbArgSets.eval.get
    args <- features.args.eval.get
    argRoleLabels <- features.argRoleLabels.eval.get
    // calculate B^3 precision-recall curve for each verb
    bCubedResults <- getAllBCubedResults(argTrees, argRoleLabels, useSenseSpecificRoles)
    resultsDir <- features.modelDir.map(_.resolve(modelName)).flatTap(features.createDir)
    _ <- plotAllBCubedVerbwise(
      modelName,
      bCubedResults,
      resultsDir.resolve("bcubed-trends-by-verb.png")
    )
    _ <- {
      val oracleResults = getTunedWeightedBCubedStats(bCubedResults, _.toList.maxBy(_.conf.f1))
      Log.info(s"Oracle F1: ${getMetricsString(oracleResults)}")
    }
    lossPerItemTuning <- Log.infoBranch("Loss-per-item tuning") {
      val lossPerItemAll = bCubedResults.toList.foldMap { case (verbType, results) =>
        results.foldMap(s => Numbers(s.lossPerItem))
      }
      val lossPerItemBest = bCubedResults.toList.foldMap { case (verbType, results) =>
        val bestF1 = results.map(_.conf.f1).maximum
        results.filter(_.conf.f1 == bestF1).foldMap(s => Numbers(s.lossPerItem))
      }
      val interval = 0.05
      val stats = lossPerItemAll.stats
      val lossThresholds = (
        scala.math.round(stats.quartiles.min / interval).toInt to scala.math.round(stats.quartiles.max / interval).toInt
      ).map(_ * interval).toList
      for {
        tuningResults <- tuneWeightedBCubedStats(
          lossThresholds, bCubedResults)(
          t => stats => {
            val sortedStats = stats.sortBy(-_.lossPerItem)
            val qualified = sortedStats.toList.takeWhile(_.lossPerItem > t)
            qualified.lastOption.getOrElse(sortedStats.head)
          }
        )
        tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
        // _ <- Log.info(s"Loss per item (all): ${getMetricsString(lossPerItemAll)}")
        // _ <- Log.info(s"Loss per item (best): ${getMetricsString(lossPerItemBest)}")
        _ <- Log.info(s"Tuned results (best loss per item): ${getMetricsString(tunedBest)}")
      } yield tuningResults
    }
    constantNumClustersTuning <- Log.infoBranch("Constant num clusters tuning") {
      val thresholds = (1 to 50).toList.map(_.toDouble)
      for {
        tuningResults <- tuneWeightedBCubedStats(
          thresholds, bCubedResults)(
          t => stats => {
            val sortedStats = stats.sortBy(_.numClusters)
            val qualified = sortedStats.toList.dropWhile(_.numClusters < t)
            qualified.headOption.getOrElse(sortedStats.head)
          }
        )
        tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
        _ <- Log.info(s"Tuned results (best num clusters): ${getMetricsString(tunedBest)}")
      } yield tuningResults
    }
    // Remaining Tuning methods:  overall-entropy, num clusters polynomial,
    squareNumClustersPenaltyTuning <- Log.infoBranch("Square num clusters penalty tuning") {
      val coeffs = (0 to 50).map(_.toDouble / 10).toList
      val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
        s.loss + (t * scala.math.pow(s.numClusters, 2.0))
      }
      for {
        tuningResults <- tuneWeightedBCubedStats(
          coeffs, bCubedResults)(
          t => stats => {
            val getLoss = getTotalLoss(t)
            stats.toList.minBy(getLoss)
          }
        )
        tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
        _ <- Log.info(s"Tuned results (sq num cluster penalty): ${getMetricsString(tunedBest)}")
      } yield tuningResults
    }
    totalEntropyLossTuning <- Log.infoBranch("Total entropy loss tuning") {
      val coeffs = (0 to 40).map(_.toDouble / 20).toList
      val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
        val numItems = s.numItems
        s.loss + (t * s.clusterSizes.foldMap(size => -size * scala.math.log(size.toDouble / numItems)))
      }
      for {
        tuningResults <- tuneWeightedBCubedStats(
          coeffs, bCubedResults)(
          t => stats => {
            val getLoss = getTotalLoss(t)
            stats.toList.minBy(getLoss)
          }
        )
        tunedBest = Chosen(tuningResults.toMap).keepMaxBy(_.f1)
        _ <- Log.info(s"Tuned results (cluster dist entropy loss coeff): ${getMetricsString(tunedBest)}")
      } yield tuningResults
    }
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
        def f1 = harmonicMean(precision, recall)

        def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
      }

      // val rand = new scala.util.Random(2643642L)
      // def noise = scala.math.abs(rand.nextGaussian / 200.0)
      def makePoints(category: String, prs: List[(Double, WeightedPR)]) = {
        val bestF1 = prs.map(_._2.f1).max
        prs.map { case (t, pr) => PRPoint(category, t, pr.f1 == bestF1, pr.recall, pr.precision) }
      }

      val allData = List(
        "loss per item" -> lossPerItemTuning,
        "constant num clusters" -> constantNumClustersTuning,
        "square num clusters penalty" -> squareNumClustersPenaltyTuning,
        "total entropy loss" -> totalEntropyLossTuning
      ).map { case (cat, items) =>
          cat -> makePoints(cat, items)
      }.toMap

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
        .xLabel("B^3 Recall")
        .yLabel("B^3 Precision")
        .xAxis().yAxis()
        .frame().rightLegend()

      val path = resultsDir.resolve("tuning-strategies.png")
      IO(plot.render().write(new java.io.File(path.toString)))
    }
  } yield ()

  def getVerbClusterModels[VerbType: Encoder : Decoder, Arg: Encoder : Decoder](
    features: Features[VerbType, Arg], modelConfig: ModelConfig,
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    features.modelDir.map(modelDir =>
      FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]](
        s"QA-SRL cluster model: $modelConfig")(
        path = modelDir.resolve(s"$modelConfig.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType, Arg])](path)
          .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
        write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
        import ClusteringModel._
        val argumentModel = QuestionEntropy
        // val argumentModel = Composite.argument(
        //   QuestionEntropy -> 1.0,
        //   AnswerEntropy -> 1.0
        // )
        val model = Joint(argumentModel)
        // val model = Composite.withJoint(
        //   VerbSqDist -> (1.0 / 200),
        //   Joint(argumentModel) -> 1.0
        // )
        // val questionModel = Composite(
        //   Composite(
        //     QuestionEntropy -> 1.0,
        //     AnswerEntropy -> 2.0
        //   ) -> 1.0,
        //   AnswerNLL -> 2.0
        // )
        // val model = Composite(
        //   Composite(
        //     VerbClauseEntropy -> 2.0,
        //     VerbSqDist -> (1.0 / 175),
        //     ) -> 1.0,
        //   Joint(questionModel) -> 1.0
        // )
        Log.infoBranch("Initializing model features")(model.init(features)) >>
          features.verbArgSets.full.get >>= (
            _.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
              val verbIds = NonEmptyVector.fromVector(verbs.value.keySet.toVector).get
              Log.trace(renderVerbType(verbType)) >> {
                for {
                  argumentAlgorithm <- argumentModel.create(features, verbType)
                  algorithm <- model.create(features, verbType)
                  (verbClusterTree, finalParams) <- IO(algorithm.runFullAgglomerativeClustering(verbIds))
                  argumentClusterTree = argumentAlgorithm.finishAgglomerativeClustering(finalParams)._1
                } yield verbType -> VerbClusterModel(
                  verbType,
                  verbClusterTree,
                  argumentClusterTree
                )
              }
            }.map(_.toMap)
          )
      }
    )
  }

  def runQasrlFrameInduction(
    features: GoldQasrlFeatures, modelOpt: Option[ModelConfig])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val modelConfigs = modelOpt.map(List(_)).getOrElse(allModelConfigs)
    for {
      // vocabs <- Log.infoBranch(s"Initializing question template vocabularies")(
      //   features.questionTemplateVocabsByVerb.get
      // )
      // _ <- Log.info(s"Total question templates: " + vocabs.unorderedFoldMap(_.items.toSet).size)
      // _ <- vocabs.toList.sortBy(-_._2.size).take(30).reverse.traverse { case (verb, vocab) =>
      //   Log.info(vocab.size + " " + verb.allForms.mkString(", "))
      // }
      _ <- Log.info(s"Running frame induction on QA-SRL. Models: ${modelConfigs.mkString(", ")}")
      verbModelsByConfig <- modelConfigs.traverse(vsConfig =>
        Log.infoBranch(s"Clustering for model: $vsConfig") {
          getVerbClusterModels[InflectedForms, ClausalQuestion](features, vsConfig, _.allForms.mkString(", ")) >>= (
            _.get.map(vsConfig -> _)
          )
        }
      ).map(_.toMap)
      // goldParaphrases <- config.readGoldParaphrases
      // evaluationItems <- config.evaluationItems.get
      // tunedThresholds <- Log.infoBranch("Evaluating and tuning thresholds")(
      //   tuningFullEvaluation(config, verbModelsByConfig, goldParaphrases, evaluationItems)
      // )
    } yield ()
  }

  def runPropBankGoldSpanFrameInduction(
    features: PropBankGoldSpanFeatures, modelOpt: Option[ModelConfig])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val modelConfigs = modelOpt.map(List(_)).getOrElse(allModelConfigs)
    // TODO read in tuned threshold (in case of test)
    // val chosenThresholds = Option(
    //   Map[ModelConfig, Double](
    //     ModelConfig.SingleCluster -> 0.0,
    //     ModelConfig.EntropyOnly -> 1.115,
    //     ModelConfig.ELMoOnly -> 0.718,
    //     ModelConfig.Interpolated(0.5) -> 1.105
    //   )
    // )
    for {
      _ <- Log.info(s"Running frame induction on PropBank with gold argument spans.")
      _ <- Log.info(s"Assume gold verb sense? " + (if(features.assumeGoldVerbSense) "yes" else "no"))
      _ <- Log.infoBranch("Running feature setup.")(features.setup)
      _ <- Log.info(s"Clustering models: ${modelConfigs.mkString(", ")}")
      verbModelsByConfig <- modelConfigs.traverse(vsConfig =>
        Log.infoBranch(s"Clustering for model: $vsConfig") {
          getVerbClusterModels[String, ESpan](features, vsConfig, identity[String]) >>= (
            _.get.map(vsConfig -> _)
          )
        }
      ).map(_.toMap)
      // evalSenseLabels <- config.propBankEvalLabels.get
      // tunedThresholds <- Log.infoBranch("Evaluating and tuning on PropBank")(
      //   evaluatePropBankVerbClusters(config, verbModelsByConfig, evalSenseLabels)
      // )
      // fullSenseLabels <- config.propBankFullLabels.get
      // _ <- chosenThresholds.foldMapM(thresholds =>
      //   Log.infoBranch("Printing debuggy stuff")(
      //     doPropBankClusterDebugging(config, fullSenseLabels, verbModelsByConfig, thresholds)
      //   )
      // )
    } yield ()
  }

  def runPropBankArgumentRoleInduction(
    features: PropBankGoldSpanFeatures)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val modelName = "test"
    for {
      _ <- Log.info(s"Running frame induction on PropBank with gold argument spans.")
      _ <- Log.info(s"Assume gold verb sense? " + (if(features.assumeGoldVerbSense) "yes" else "no"))
      _ <- Log.infoBranch("Running feature setup.")(features.setup)
      _ <- Log.info(s"Model name: $modelName")
      argTrees <- Log.infoBranch(s"Clustering arguments") {
        getArgumentClusters[String, ESpan](modelName, features, identity[String]).flatMap(_.get)
      }
      _ <- {
        if(features.assumeGoldVerbSense) {
          Log.infoBranch("Evaluating argument clustering")(
            evaluateArgumentClusters(modelName, features, argTrees, useSenseSpecificRoles = true)
          )
        } else {
          Log.infoBranch("Evaluating argument clustering (verb sense specific)")(
            evaluateArgumentClusters(s"$modelName-sense", features, argTrees, useSenseSpecificRoles = true)
          ) >> Log.infoBranch("Evaluating argument clustering (verb sense agnostic)")(
            evaluateArgumentClusters(s"$modelName-nosense", features, argTrees, useSenseSpecificRoles = false)
          )
        }
      }
      // evalSenseLabels <- config.propBankEvalLabels.get
      // tunedThresholds <- Log.infoBranch("Evaluating and tuning on PropBank")(
      //   evaluatePropBankVerbClusters(config, verbModelsByConfig, evalSenseLabels)
      // )
      // fullSenseLabels <- config.propBankFullLabels.get
      // _ <- chosenThresholds.foldMapM(thresholds =>
      //   Log.infoBranch("Printing debuggy stuff")(
      //     doPropBankClusterDebugging(config, fullSenseLabels, verbModelsByConfig, thresholds)
      //   )
      // )
    } yield ()
  }

  def main: Opts[IO[ExitCode]] = {
    val dataO = Opts.option[String](
      "data", metavar = "qasrl|propbank-span", help = "Data setting to run in."
    )

    val modeO = Opts.option[String](
      "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
    ).mapValidated { string =>
      RunMode.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test."))
    }

    val modelConfigOptO = Opts.option[String](
      "model", metavar = "entropy|elmo|<float>", help = "Verb sense model configuration."
    ).mapValidated { string =>
      ModelConfig.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid model $string: must be entropy, elmo, or a float (interpolation param)."))
    }.orNone

    (dataO, modeO, modelConfigOptO).mapN { (data, mode, modelConfigOpt) =>
      for {
        implicit0(logger: EphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.create()
        _ <- logger.info(s"Data: $data")
        _ <- logger.info(s"Mode: $mode")
        _ <- logger.info(s"Model configuration: $modelConfigOpt")
        _ <- data match {
          case "qasrl-gold" =>
            runQasrlFrameInduction(new GoldQasrlFeatures(mode), modelConfigOpt)
          case "propbank-sense-args" => // assume gold verb sense, only cluster/evaluate arguments
            runPropBankArgumentRoleInduction(new PropBankGoldSpanFeatures(mode, assumeGoldVerbSense = true))
          case "propbank-lemma-args" => // don't assume gold verb sense, only cluster arguments? TODO
            runPropBankArgumentRoleInduction(new PropBankGoldSpanFeatures(mode, assumeGoldVerbSense = false))
          case _ => ???
        }
      } yield ExitCode.Success
    }
  }
}
