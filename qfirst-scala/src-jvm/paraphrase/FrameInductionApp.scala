package qfirst.paraphrase
import qfirst._
import qfirst.paraphrase.browse._
import qfirst.frames.implicits._
import qfirst.metrics.HasMetrics.ops._
import qfirst.protocols.SimpleQAs
// import qfirst.frames._
// import qfirst.metrics._

import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.data.Validated
import cats.implicits._

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files
import java.nio.file.Paths

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.ArgumentSlot
import qasrl.bank._
import qasrl.data._
import qasrl.labeling.SlotBasedLabel

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.collection.immutable.Map

import scala.util.Random

class Vocab[A] private (
  indexToItem: Vector[A],
  itemToIndex: Map[A, Int]
) {
  def items = indexToItem
  def indices = indexToItem.indices.toVector
  def getItem(index: Int) = indexToItem(index)
  def getIndex(item: A) = itemToIndex(item)
  def size = indexToItem.size
}
object Vocab {
  def make[A](items: Set[A]): Vocab[A] = {
    val itemsVec = items.toVector
    new Vocab(itemsVec, itemsVec.zipWithIndex.toMap)
  }
}

// TODO applicative
case class ClusteringInstances[A](
  instances: Map[InflectedForms, Map[String, Map[Int, A]]]
) {
  def map[B](f: A => B): ClusteringInstances[B] = {
    ClusteringInstances(
      instances.transform { case (k, v) =>
        v.transform { case (k, v) =>
          v.transform { case (k, v) =>
            f(v)
          }
        }
      }
    )
  }
  // TODO merge and stuff
}
object ClusteringInstances {
  implicit def clusteringInstancesMonoid[A]: Monoid[ClusteringInstances[A]] =
    new Monoid[ClusteringInstances[A]] {
      def empty: ClusteringInstances[A] = ClusteringInstances(Map())
      def combine(x: ClusteringInstances[A], y: ClusteringInstances[A]) = {
        ClusteringInstances(
          (x.instances.keySet ++ y.instances.keySet).iterator.map { forms =>
            val xSents = x.instances.getOrElse(forms, Map())
            val ySents = y.instances.getOrElse(forms, Map())
            forms -> (xSents.keySet ++ ySents.keySet).iterator.map { sid =>
              val xVerbs = xSents.getOrElse(sid, Map())
              val yVerbs = ySents.getOrElse(sid, Map())
              sid -> (xVerbs ++ yVerbs)
            }.toMap
          }.toMap
        )
      }
    }
}

object FrameInductionApp extends IOApp {

  import ClauseResolution.ArgStructure

  // type QABeam = List[SimpleQAs.BeamItem[SlotBasedLabel[VerbForm]]]
  type Instances = Map[InflectedForms, Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  // type ResolvedInstances = Map[InflectedForms, Map[SentenceId, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]

  def makeVerbVocab(instances: Instances): Vocab[InflectedForms] = {
    Vocab.make(instances.keySet)
  }

  def makeVerbSpecificClauseVocab(instances: Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]): Vocab[ArgStructure] = {
    Vocab.make(
      instances.values.toList.foldMap(verbMap =>
        verbMap.values.toList.foldMap(qMap =>
          ClauseResolution.getResolvedStructures(qMap.keys.toList).map(_._1).toSet
        )
      )
    )
  }

  import breeze.linalg.DenseVector

  def getGoldELMoInstances(
    dataset: Dataset,
    filePrefix: String
  ): IO[ClusteringInstances[DenseVector[Float]]] = {
    val idsPath = Paths.get(filePrefix + "_ids.jsonl")
    val embPath = Paths.get(filePrefix + "_emb.bin")
    val embDim = 1024
    for {
      ids <- logOp(
        "Reading verb IDs",
        FileUtil.readJsonLines[VerbId](idsPath).compile.toList
      )
      embeddings <- logOp(
        "Reading verb embeddings",
        FileUtil.readDenseFloatVectorsNIO(embPath, embDim)
      )
      // _ <- IO(println(s"Number of IDs: ${ids.size}; Number of embeddings: ${embeddings.size}; embedding size: ${embeddings.head.size}"))
      _ <- IO {
        val numToCheck = 5
        val propSane = embeddings.take(numToCheck).foldMap(_.activeValuesIterator.map(math.abs).filter(f => f > 1e-2 && f < 1e2).size).toDouble / (numToCheck * embDim)
        val warnText = if(propSane < 0.8) "[== WARNING ==] there might be endianness issues with how you're reading the ELMo embeddings; " else ""
        println(warnText + f"Sanity check: ${propSane}%.3f of ELMo embedding units have absolute value between ${1e-2}%s and ${1e2}%s.")
        // embeddings.take(numToCheck).foreach(e => println(e.activeValuesIterator.take(10).mkString("\t")))
      }
    } yield ClusteringInstances(
      ids.zip(embeddings).foldMap { case (VerbId(sentenceId, verbIndex), embedding) =>
        // omit elmo vectors for verbs filtered out of the dataset
        dataset.sentences.get(sentenceId)
          .flatMap(_.verbEntries.get(verbIndex))
          .map(_.verbInflectedForms).foldMap(verbForms =>
            Map(verbForms -> Map(sentenceId -> Map(verbIndex -> List(embedding))))
          )
      }
    ).map(_.head)
  }

  import qfirst.paraphrase.models._
  import breeze.stats.distributions.Multinomial

  type ClausalQ = (ArgStructure, ArgumentSlot)
  import breeze.linalg._
  import scala.collection.immutable.Vector

  def runVerbWiseAgglomerative(
    algorithm: ClusteringAlgorithm)(
    verbInflectedForms: InflectedForms,
    verbIds: Vector[VerbId],
    makeInstance: VerbId => algorithm.Instance,
    hyperparams: algorithm.Hyperparams,
  ): IO[MergeTree[VerbId]] = {
    val instances = verbIds.map(makeInstance)
    IO(print(" " + verbInflectedForms.stem)) >>
      IO(algorithm.runAgglomerativeClustering(instances, hyperparams)._1.map(verbIds))
  }

  def runVerbSenseAgglomerativeClustering(
    verbSenseConfig: VerbSenseConfig,
    instances: Instances,
    elmoVecs: ClusteringInstances[DenseVector[Float]]
  ): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
    val verbs = instances.map { case (forms, sentMap) =>
      forms -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
        verbIndices.keys.toVector
          .filter(vi =>
            instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty &&
              elmoVecs.instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
          )
          .map(vi => VerbId(sid, vi))
      }
    }.filter(_._2.nonEmpty)
    IO(println("Clustering verbs:")) >>
      verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
        val verbInstances = instances(verbInflectedForms)
        val clauseVocab = makeVerbSpecificClauseVocab(verbInstances)
        val makeInstance = (verbId: VerbId) => {
          val questions = verbInstances(verbId.sentenceId)(verbId.verbIndex).keySet.toList
          val clauseCounts = ClauseResolution.getResolvedFramePairs(
            verbInflectedForms, questions
          ).map(_._1).map(ClauseResolution.getClauseTemplate)
            .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
          val vector = elmoVecs.instances(verbInflectedForms)(verbId.sentenceId)(verbId.verbIndex)
          clauseCounts -> vector
        }
        val clustering = verbSenseConfig match {
          case VerbSenseConfig.SingleCluster =>
            IO.pure(
              NonEmptyList.fromList(verbIds.toList).get.zipWithIndex
                .reduceLeftTo(p => MergeTree.Leaf(0.0, p._1): MergeTree[VerbId]) {
                  case (tree, (next, newRank)) => MergeTree.Merge(newRank, 0.0, tree, MergeTree.Leaf(0.0, next))
                }
            )
          case VerbSenseConfig.EntropyOnly =>
            runVerbWiseAgglomerative(MinEntropyClustering)(
              verbInflectedForms, verbIds, (v => makeInstance(v)._1), MinEntropyClustering.Hyperparams(clauseVocab.size)
            )
          case VerbSenseConfig.ELMoOnly =>
            runVerbWiseAgglomerative(VectorMeanClustering)(
              verbInflectedForms, verbIds, (v => makeInstance(v)._2), ()
            )
          case VerbSenseConfig.Interpolated(lambda) =>
            val algorithm = new CompositeClusteringAlgorithm {
              val _1 = MinEntropyClustering; val _2 = VectorMeanClustering
            }
            runVerbWiseAgglomerative(algorithm)(
              verbInflectedForms, verbIds, makeInstance,
              algorithm.Hyperparams(MinEntropyClustering.Hyperparams(clauseVocab.size), (), lambda)
            )
        }
        clustering.map(verbInflectedForms -> _)
      }.map(_.toMap).flatTap(_ => IO(println))
  }

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def getVerbSenseClusters(config: Config, verbSenseConfig: VerbSenseConfig): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
    config.getCachedVerbClusters(verbSenseConfig).flatMap {
      case Some(verbClusters) => IO.pure(verbClusters)
      case None => for {
        fullInstances <- config.fullInstances.get
        trainSet <- config.input.get
        evalSet <- config.eval.get
        trainElmoVecs <- getGoldELMoInstances(trainSet, config.inputElmoPrefix)
        evalElmoVecs <- getGoldELMoInstances(evalSet, config.evalElmoPrefix)
        verbClusters <- runVerbSenseAgglomerativeClustering(
          verbSenseConfig = verbSenseConfig,
          instances = fullInstances,
          elmoVecs = trainElmoVecs |+| evalElmoVecs
        )
        _ <- config.cacheVerbClusters(verbSenseConfig, verbClusters)
      } yield verbClusters
    }
  }

  import qfirst.metrics.BinaryConf

  @JsonCodec case class VerbSenseTuningPoint(
    model: VerbSenseConfig,
    maxLoss: Double,
    minClauseProb: Double,
    lbConf: BinaryConf.Stats,
    ubConf: BinaryConf.Stats
  )

  def evaluateVerbClusters(
    config: Config,
    verbClustersByConfig: Map[VerbSenseConfig, Map[InflectedForms, MergeTree[VerbId]]],
    goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
    evaluationItems: Vector[(InflectedForms, String, Int)]
  ): IO[Map[VerbSenseConfig, VerbSenseTuningPoint]] = {
    val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
      goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
    }
    if(presentEvaluationItems.isEmpty) IO.pure {
      verbClustersByConfig.transform { case (vsConfig, verbClusters) =>
        VerbSenseTuningPoint(vsConfig, verbClusters.values.map(_.loss).max, 0.0, BinaryConf.Stats(), BinaryConf.Stats()) // better max than inf i guess
      }
    } else {
      for {
        instances <- config.fullInstances.get
        _ <- {
          import com.cibo.evilplot._
          import com.cibo.evilplot.numeric._
          import com.cibo.evilplot.plot._
          import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
          import com.cibo.evilplot.plot.renderers.PointRenderer

          case class LossDatum(
            model: VerbSenseConfig,
            verb: InflectedForms,
            numInstances: Long,
            maxLoss: Double,
            val x: Double,
            val y: Double
          ) extends Datum2d[LossDatum] {
            def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
          }
          val lossData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
            clustersByVerb.toList.map { case (verbInflectedForms, clusterTree) =>
              val numInstances = clusterTree.size
              LossDatum(vsConfig, verbInflectedForms, numInstances, clusterTree.loss, numInstances.toDouble, clusterTree.loss)
            }
          }
          val plot = ScatterPlot(
		        lossData,
		        pointRenderer = Some(PointRenderer.colorByCategory(lossData, ((x: LossDatum) => x.model.modelName), size = Some(2.0)))
	        ).xAxis().yAxis().frame().rightLegend()
          config.globalResultsDir.flatMap(path =>
            IO(plot.render().write(new java.io.File(path.resolve("loss-trends.png").toString)))
          )
        }
        fullEvaluationResultsByConfig = verbClustersByConfig.transform { case (vsConfig, clustersByVerb) =>
          presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
            val verbClauseSets = instances(verbInflectedForms).flatMap { case (sid, verbMap) =>
              verbMap.map { case (vi, qMap) =>
                val questions = qMap.keySet.toList
                val clauses = ClauseResolution.getResolvedFramePairs(
                  verbInflectedForms, questions
                ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
                VerbId(sid, vi) -> clauses
              }
            }
            val inputClauseSet = verbClauseSets(VerbId(sentenceId, verbIndex))

            val verbClusters = clustersByVerb(verbInflectedForms)
            val instanceClusters = verbClusters.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present
                                                                                                    // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
            val clusterMetricsByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = instanceClusters.map { tree =>
              val clusterSize = tree.size
              val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss / clusterSize
              val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
                verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
              }
              val predictedClausesWithProbsIncreasing = clauseCounts
                .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
                .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
                .sortBy(_._2)
              val confsForClauseThresholds = predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
                val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
                val predictedClauses = clausesWithProbs.map(_._1).toList.toSet
                val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
                val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
                val lbConf = BinaryConf.Stats(
                  tp = (predictedClauses intersect correctClauses).size,
                  tn = 0, // we don't need this for p/r/f
                  fp = (predictedClauses -- correctClauses).size,
                  fn = (correctClauses -- predictedClauses).size
                )
                val ubConf = BinaryConf.Stats(
                  tp = (predictedClauses -- incorrectClauses).size,
                  tn = 0, // we don't need this for p/r/f
                  fp = (predictedClauses intersect incorrectClauses).size,
                  fn = (correctClauses -- predictedClauses).size
                )
                clauseThreshold -> (lbConf -> ubConf)
              }.toList
              lossThreshold -> confsForClauseThresholds
            }
            clusterMetricsByThresholds
          }
        }
        allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
          val lossThresholds = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
          lossThresholds.flatMap { maxLoss =>
            val chosenClusters = evaluationItemResults.map(
              _.find(_._1 <= maxLoss).get._2
            )
            val clauseProbThresholds = chosenClusters.flatMap(_.map(_._1)).toSet
            clauseProbThresholds.map { minClauseProb =>
              val confPairs = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
              val lbConf = confPairs.foldMap(_._1)
              val ubConf = confPairs.foldMap(_._2)
              VerbSenseTuningPoint(vsConfig, maxLoss, minClauseProb, lbConf, ubConf)
            }
          }
        }
        bestModels <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
          val lbBest = allPoints.maxBy(_.lbConf.f1)
          val ubBest = allPoints.maxBy(_.ubConf.f1)
          val lbResString = SandboxApp.getMetricsString(lbBest.lbConf)
          val ubResString = SandboxApp.getMetricsString(ubBest.ubConf)
          println(s"${vsConfig.modelName} clause lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson))
          println(s"${vsConfig.modelName} clause lb metrics: " + lbResString)
          println(s"${vsConfig.modelName} clause ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson))
          println(s"${vsConfig.modelName} clause ub metrics: " + ubResString)

          config.resultsPath(vsConfig).flatMap(path =>
            FileUtil.writeString(path.resolve("verb-sense-lb-results.txt"))(lbResString) >>
              FileUtil.writeString(path.resolve("verb-sense-ub-results.txt"))(ubResString) >>
              FileUtil.writeJson(path.resolve("verb-sense-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
              FileUtil.writeJson(path.resolve("verb-sense-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
          ).as(vsConfig -> ubBest)
        }.map(_.toMap)
        _ <- {
          import com.cibo.evilplot._
          import com.cibo.evilplot.numeric._
          import com.cibo.evilplot.plot._
          import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
          import com.cibo.evilplot.plot.renderers.PointRenderer

          case class PRPoint(
            model: VerbSenseConfig,
            recall: Double,
            precision: Double) extends Datum2d[PRPoint] {
            val x = recall
            val y = precision
            def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
          }

          val rand = new scala.util.Random(2643642L)
          def noise = math.abs(rand.nextGaussian / 200.0)

          val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
            PRPoint(vsTuningPoint.model, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
          }
          val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
            PRPoint(vsTuningPoint.model, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
          }

          val lbPlot = ScatterPlot(
		        lbData,
		        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()
          val ubPlot = ScatterPlot(
		        ubData,
		        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()

          config.globalResultsDir.flatMap(path =>
            IO(lbPlot.render().write(new java.io.File(path.resolve("verb-sense-lb.png").toString))) >>
              IO(ubPlot.render().write(new java.io.File(path.resolve("verb-sense-ub.png").toString)))
          )
        }
      } yield bestModels
    }
  }

  // TODO restrict coindexing to clauses above minimum probability threshold?
  def runCollapsedCoindexing(
    instances: Instances,
    verbSenseModel: VerbSenseTuningPoint,
    verbClusters: Map[InflectedForms, MergeTree[VerbId]],
    fuzzyArgEquivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  ): Map[InflectedForms, VerbFrameset] = {
    verbClusters.transform { case (verbInflectedForms, tree) =>
      val numInstancesForVerb = tree.size
      val verbClauseSets = instances(verbInflectedForms).flatMap { case (sid, verbMap) =>
        verbMap.map { case (vi, qMap) =>
          val questions = qMap.keySet.toList
          val clauses = ClauseResolution.getResolvedFramePairs(
            verbInflectedForms, questions
          ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
          VerbId(sid, vi) -> clauses
        }
      }
      val clusterTrees = tree.splitByPredicate(t => (t.loss / numInstancesForVerb) > verbSenseModel.maxLoss)
      val verbRel = fuzzyArgEquivalences(verbInflectedForms)
      val verbFrames = clusterTrees.map { frameTree =>
        val verbIds = frameTree.values
        val clauseTemplates = verbIds.foldMap { vid =>
          verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
        }.toList.map { case (c, count) => FrameClause(c, (count.toDouble / verbIds.size)) }
        val frameProb = verbIds.size.toDouble / numInstancesForVerb
        val clausalQVocab = Vocab.make(
          clauseTemplates.map(_.args).flatMap { clauseTemplate =>
            getArgumentSlotsForClauseTemplate(clauseTemplate).toList
              .map(slot => clauseTemplate -> slot)
          }.toSet
        )
        val indices = clausalQVocab.indices
        val fuzzyEquivMatrix = Array.ofDim[Double](clausalQVocab.size, clausalQVocab.size)
        for(i <- indices; j <- indices) {
          val (qi, qj) = clausalQVocab.getItem(i) -> clausalQVocab.getItem(j)
          val score = 1.0 - (
            if(i == j) 1.0 // reflexive
            else if(qi._1 == qj._1) 0.0 // prohibit coindexing within a clause
            else {
              val resOpt = verbRel.get(qi -> qj).orElse(verbRel.get(qj -> qi))
              resOpt.getOrElse {
                println(s"XXX: $qi \t $qj")
                0.5 // shouldn't happen
              }
            }
          )
          fuzzyEquivMatrix(i)(j) = score
        }
        val (mergeTree, _) = CompleteLinkageClustering.runAgglomerativeClustering(indices, fuzzyEquivMatrix)
        val coindexingTree = mergeTree.map(clausalQVocab.getItem)
        VerbFrame(verbIds.toSet, clauseTemplates, coindexingTree, frameProb)
      }.toList
      VerbFrameset(verbInflectedForms, verbFrames)
    }
  }

  @JsonCodec case class ParaphraseTuningPoint(
    vsConfig: VerbSenseConfig,
    minClauseProb: Double,
    minCoindexingProb: Double,
    lbConf: BinaryConf.Stats,
    ubConf: BinaryConf.Stats
  )

  def tuningParaphraseEvaluation(
    config: Config,
    verbFramesetsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbFrameset]],
    goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
    evaluationItems: Vector[(InflectedForms, String, Int)]
  ): IO[Map[VerbSenseConfig, ParaphraseTuningPoint]] = {
    val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
      goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
    }
    if(presentEvaluationItems.isEmpty) IO.pure {
      verbFramesetsByConfig.transform { case (vsConfig, verbClusters) =>
        ParaphraseTuningPoint(vsConfig, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
      }
    } else {
      for {
        instances <- config.fullInstances.get
        fullEvaluationResultsByConfig = verbFramesetsByConfig.transform { case (vsConfig, framesets) =>
          presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
            val verbClausalQs = instances(verbInflectedForms).flatMap { case (sid, verbMap) =>
              verbMap.map { case (vi, qMap) =>
                val questions = qMap.keySet.toList
                val clausalQs = ClauseResolution.getResolvedStructures(questions).toSet
                  .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
                VerbId(sid, vi) -> clausalQs
              }
            }
            val verbId = VerbId(sentenceId, verbIndex)
            val inputClausalQs = verbClausalQs(verbId)
            val verbFrameset = framesets(verbInflectedForms)
            val verbFrame = verbFrameset.frames.find(_.verbIds.contains(verbId)).get
            // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
            val paraphrasingMetricByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = {
              verbFrame.clauseTemplates.sortBy(_.probability).tails.toList.map { clauses =>
                val minClauseProb = clauses.headOption.fold(1.0)(_.probability)
                val clauseTemplates = clauses.map(_.args).toSet
                // each list is sorted increasing by minCoindexingProb
                val confPairLists = inputClausalQs.toList.map { cq =>
                  verbFrame.coindexingTree.clustersForValue(cq).get.map { tree =>
                    val clusterSize = tree.size
                    val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
                    val minCoindexingProb = 1.0 - maxLoss
                    val predictedParaphrases = tree.values.toSet - cq
                    val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
                    val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
                      instanceParaphrases.incorrectClauses.flatMap(ct =>
                        getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
                      )

                    val lbConf = BinaryConf.Stats(
                      tp = (predictedParaphrases intersect correctParaphrases).size,
                      tn = 0, // we don't need this for p/r/f
                      fp = (predictedParaphrases -- correctParaphrases).size,
                      fn = (correctParaphrases -- predictedParaphrases).size
                    )
                    val ubConf = BinaryConf.Stats(
                      tp = (predictedParaphrases -- incorrectParaphrases).size,
                      tn = 0, // we don't need this for p/r/f
                      fp = (predictedParaphrases intersect incorrectParaphrases).size,
                      fn = (correctParaphrases -- predictedParaphrases).size
                    )
                    minCoindexingProb -> (lbConf -> ubConf)
                  }
                }
                val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
                minClauseProb -> allMinCoindexingProbs.map { minCoindexingProb =>
                  val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
                  val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
                  minCoindexingProb -> (lbConf -> ubConf)
                }
              }
            }
            paraphrasingMetricByThresholds
          }
        }
        allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
          val minClauseProbs = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sorted
          minClauseProbs.flatMap { minClauseProb =>
            val chosenClauseSets = evaluationItemResults.map(
              _.find(_._1 >= minClauseProb).get._2
            )
            val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList
            minCoindexingProbs.map { minCoindexingProb =>
              val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
              val lbConf = confPairs.foldMap(_._1)
              val ubConf = confPairs.foldMap(_._2)
              ParaphraseTuningPoint(vsConfig, minClauseProb, minCoindexingProb, lbConf, ubConf)
            }
          }
        }
        bestThresholds <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
          val lbBest = allPoints.maxBy(_.lbConf.f1)
          val ubBest = allPoints.maxBy(_.ubConf.f1)
          // val ubBest = allPoints.maxBy(_.ubConf.f1)
          // val ubBestThresholds = CalibratedVerbSenseModel(
          //   vsConfig, verbClustersByConfig(vsConfig), ubBest.maxLoss, ubBest.minClauseProb
          // )
          // (lbBestModel, lbBest.lbConf, ubBestModel, ubBest.ubConf)
          // println(s"${vsConfig.modelName}: " + SandboxApp.getMetricsString(ubBest.ubConf))

          val lbResString = SandboxApp.getMetricsString(lbBest.lbConf)
          val ubResString = SandboxApp.getMetricsString(ubBest.ubConf)
          println(s"${vsConfig.modelName} paraphrase lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson))
          println(s"${vsConfig.modelName} paraphrase lb metrics: " + lbResString)
          println(s"${vsConfig.modelName} paraphrase ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson))
          println(s"${vsConfig.modelName} paraphrase ub metrics: " + ubResString)
          config.resultsPath(vsConfig).flatMap(path =>
            FileUtil.writeString(path.resolve("questions-lb-results.txt"))(lbResString) >>
              FileUtil.writeString(path.resolve("questions-ub-results.txt"))(ubResString) >>
              FileUtil.writeJson(path.resolve("questions-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
              FileUtil.writeJson(path.resolve("questions-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
          ).as(vsConfig -> ubBest)
        }.map(_.toMap)
        _ <- {
          import com.cibo.evilplot._
          import com.cibo.evilplot.numeric._
          import com.cibo.evilplot.plot._
          import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
          import com.cibo.evilplot.plot.renderers._

          case class PRPoint(
            model: VerbSenseConfig,
            recall: Double,
            precision: Double) extends Datum2d[PRPoint] {
            val x = recall
            val y = precision
            def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
          }

          // val linePointsByModel = allPointsByModel.transform { case (model, vsTuningPoints) =>
          //   NonEmptyList.fromList(vsTuningPoints.sortBy(-_.lbConf.recall)).get
          //     .reduceLeftTo(NonEmptyList.of(_)) { (best, next) =>
          //       if(next.lbConf.precision > best.head.lbConf.precision) {
          //         if(next.lbConf.recall == best.head.lbConf.recall) best
          //         else NonEmptyList(next, best.toList)
          //       } else best
          //     }.toList
          // }
          // val lbLineData = linePointsByModel.values.toList.flatten.map { vsTuningPoint =>
          //   PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall, vsTuningPoint.lbConf.precision)
          // }
          // val lbLinePlot = LinePlot(
		      //   lbData,
		      //   pathRenderer = Some(PathRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        // ).xAxis().yAxis().frame().rightLegend()


          val rand = new scala.util.Random(2643642L)
          def noise = math.abs(rand.nextGaussian / 200.0)

          val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
            PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
          }
          val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
            PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
          }

          val lbPlot = ScatterPlot(
		        lbData,
		        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()
          val ubPlot = ScatterPlot(
		        ubData,
		        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()

          config.globalResultsDir.flatMap(path =>
            // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
              IO(lbPlot.render().write(new java.io.File(path.resolve("question-lb.png").toString))) >>
              IO(ubPlot.render().write(new java.io.File(path.resolve("question-ub.png").toString)))
          )
        }
      } yield bestThresholds
    }
  }

  // TODO: elmo loss is ~175x greater; tune around this number
  // _ <- {
  //   import qfirst.metrics._
  //   val dist = Numbers(verbClusters.values.toVector.filterNot(_.loss == 0.0).map(tree => tree.loss / tree.size))
  //   IO(println(SandboxApp.getMetricsString(dist)))
  // }

  @JsonCodec case class ModelParams(
    config: VerbSenseConfig,
    maxClusterLoss: Double,
    minClauseThreshold: Double,
    minCoindexingThreshold: Double)

  def program(config: Config, modelOpt: Option[VerbSenseConfig]): IO[ExitCode] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(
      List(VerbSenseConfig.SingleCluster, VerbSenseConfig.EntropyOnly, VerbSenseConfig.ELMoOnly) ++
        List(VerbSenseConfig.Interpolated(0.5))
        // (1 to 9).map(_.toDouble / 10.0).toList.map(VerbSenseConfig.Interpolated(_))
    )
    // TODO read in tuned thresholds (in case of test)
    for {
      verbClustersByConfig <- verbSenseConfigs.traverse(vsConfig =>
        getVerbSenseClusters(config, vsConfig).map(vsConfig -> _)
      ).map(_.toMap)
      goldParaphrases <- config.readGoldParaphrases
      evaluationItems <- config.evaluationItems.get
      bestModels <- evaluateVerbClusters(config, verbClustersByConfig, goldParaphrases, evaluationItems)
      collapsedQAOutputs <- config.collapsedQAOutputs.get
      fullInstances <- config.fullInstances.get
      coindexedModels <- logOp(
        "Running coindexing",
        bestModels.transform { case (vsConfig, model) =>
          runCollapsedCoindexing(fullInstances, model, verbClustersByConfig(vsConfig), collapsedQAOutputs)
        }
      )
      _ <- logOp(
        "Writing framesets",
        coindexedModels.toList.traverse { case (vsConfig, framesets) =>
          config.writeFramesets(vsConfig, framesets)
        }
      )
      modelThresholds <- logOp(
        "Tuning paraphrasing thresholds",
        tuningParaphraseEvaluation(config, coindexedModels, goldParaphrases, evaluationItems)
      )
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val modeO = Opts.option[String](
      "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
    ).mapValidated { string =>
      RunMode.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test."))
    }
    val verbSenseConfigOptO = Opts.option[String](
      "model", metavar = "entropy|elmo|<float>", help = "Verb sense model configuration."
    ).mapValidated { string =>
      VerbSenseConfig.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid model $string: must be entropy, elmo, or a float (interpolation param)."))
    }.orNone

    (modeO, verbSenseConfigOptO).mapN { (mode, verbSenseConfigOpt) =>
      program(Config(mode), verbSenseConfigOpt)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
