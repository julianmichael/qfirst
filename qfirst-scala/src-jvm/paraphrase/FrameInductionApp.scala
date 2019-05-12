package qfirst.paraphrase
import qfirst._
import qfirst.paraphrase.browse._
import qfirst.frames.implicits._
import qfirst.metrics.HasMetrics
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

  // type ResolvedInstances = Map[InflectedForms, Map[SentenceId, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  type Instances = Map[InflectedForms, Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  type PropBankInstances = Map[String, Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  type PropBankLabels = Map[String, Map[String, Map[Int, String]]] // verb lemma, sentence id, verb index, verb sense

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
    IO(print("Clustering verbs: ")) >> // sort decreasing by size to front-load memory usage
      verbs.toList.sortBy(-_._2.size).traverse { case (verbInflectedForms, verbIds) =>
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

  def getQasrlVerbClusterModels(config: Config, verbSenseConfig: VerbSenseConfig): IO[Map[InflectedForms, VerbClusterModel]] = {
    config.cacheVerbModelComputation(verbSenseConfig) {
      for {
        instances <- config.fullInstances.get
        elmoVecs <- config.fullElmo.get
        verbClusters <- runVerbSenseAgglomerativeClustering(
          verbSenseConfig = verbSenseConfig,
          instances = instances,
          elmoVecs = elmoVecs
        )
        collapsedQAOutputs <- config.collapsedQAOutputs.get
        verbClusterModels = verbClusters.map { case (verbInflectedForms, clusterTree) =>
          val clauseSets = instances(verbInflectedForms).iterator.flatMap { case (sid, verbMap) =>
            verbMap.iterator.map { case (vi, qas) =>
              val questions = qas.keySet.toList
              val clauseSet = ClauseResolution.getResolvedFramePairs(
                verbInflectedForms, questions
              ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
              VerbId(sid, vi) -> clauseSet
            }
          }.toMap
          verbInflectedForms -> VerbClusterModel(
            verbInflectedForms,
            clusterTree,
            clauseSets,
            collapsedQAOutputs(verbInflectedForms).toList
          )
        }
      } yield verbClusterModels
    }
  }

  def getPropBankVerbClusterModels(config: Config, verbSenseConfig: VerbSenseConfig): IO[Map[String, PropBankVerbClusterModel]] = {
    config.cachePropBankVerbModelComputation(verbSenseConfig) {
      for {
        fullInstances <- config.propBankFullInstances.get
        // trainSet <- config.input.get
        // evalSet <- config.eval.get
        // trainElmoVecs <- getGoldELMoInstances(trainSet, config.inputElmoPrefix)
        // evalElmoVecs <- getGoldELMoInstances(evalSet, config.evalElmoPrefix)
        // verbClusters <- runVerbSenseAgglomerativeClustering(
        //   verbSenseConfig = verbSenseConfig,
        //   instances = fullInstances,
        //   elmoVecs = trainElmoVecs |+| evalElmoVecs
        // )
        // collapsedQAOutputs <- config.collapsedQAOutputs.get
        // verbClusterModels = verbClusters.map { case (verbInflectedForms, clusterTree) =>
        //   val clauseSets = fullInstances(verbInflectedForms).iterator.flatMap { case (sid, verbMap) =>
        //     verbMap.iterator.map { case (vi, qas) =>
        //       val questions = qas.keySet.toList
        //       val clauseSet = ClauseResolution.getResolvedFramePairs(
        //         verbInflectedForms, questions
        //       ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
        //       VerbId(sid, vi) -> clauseSet
        //     }
        //   }.toMap
        //   verbInflectedForms -> VerbClusterModel(
        //     verbInflectedForms,
        //     clusterTree,
        //     clauseSets,
        //     collapsedQAOutputs(verbInflectedForms).toList
        //   )
        // }
      } yield ??? //verbClusterModels
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
        _ <- writeLossGraph(
          verbClustersByConfig,
          config.globalResultsDir.map(_.resolve("loss-trends.png"))
        )
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
        val coindexingTree = Coindexing.getCoindexingTree(clauseTemplates.map(_.args).toSet, verbRel)
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

  @JsonCodec case class ModelMetrics(
    clauseLBConf: BinaryConf.Stats,
    clauseUBConf: BinaryConf.Stats,
    questionLBConf: BinaryConf.Stats,
    questionUBConf: BinaryConf.Stats)
  object ModelMetrics {
    implicit val modelMetricsHasMetrics: HasMetrics[ModelMetrics] = {
      new HasMetrics[ModelMetrics] {
        def getMetrics(mm: ModelMetrics) = MapTree.fork(
          "clause lb" -> mm.clauseLBConf.getMetrics,
          "clause ub" -> mm.clauseUBConf.getMetrics,
          "question lb" -> mm.questionLBConf.getMetrics,
          "question ub" -> mm.questionUBConf.getMetrics
        )
      }
    }
  }

  def writeLossGraph(
    verbClustersByConfig: Map[VerbSenseConfig, Map[InflectedForms, MergeTree[VerbId]]],
    getPath: IO[NIOPath]
  ): IO[Unit] = {
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
    getPath.flatMap(path =>
      IO(plot.render().write(new java.io.File(path.toString)))
    )
  }

  def writeDepthGraph(
    verbClustersByConfig: Map[VerbSenseConfig, Map[InflectedForms, MergeTree[VerbId]]],
    getPath: IO[NIOPath]
  ): IO[Unit] = {
    import com.cibo.evilplot._
    import com.cibo.evilplot.numeric._
    import com.cibo.evilplot.plot._
    import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
    import com.cibo.evilplot.plot.renderers.PointRenderer

    case class DepthDatum(
      model: VerbSenseConfig,
      verb: InflectedForms,
      numInstances: Long,
      avgDepth: Double,
      val x: Double,
      val y: Double
    ) extends Datum2d[DepthDatum] {
      def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
    }
    val depthData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
      clustersByVerb.toList.map { case (verbInflectedForms, clusterTree) =>
        val numInstances = clusterTree.size
        case class DepthAcc(depthSum: Int, numLeaves: Int) {
          def merge(that: DepthAcc) = {
            val totalLeaves = this.numLeaves + that.numLeaves
            val totalDepth = this.depthSum + that.depthSum + totalLeaves
            DepthAcc(totalDepth, totalLeaves)
          }
          def avgDepth = depthSum.toDouble / numLeaves
        }
        val avgDepth = clusterTree.cata[DepthAcc](
          leaf = (_, _) => DepthAcc(0, 1),
          merge = (_, _, l, r) => l.merge(r)
        ).avgDepth
        DepthDatum(vsConfig, verbInflectedForms, numInstances, avgDepth, numInstances.toDouble, avgDepth)
      }
    }
    val plot = ScatterPlot(
		  depthData,
		  pointRenderer = Some(PointRenderer.colorByCategory(depthData, ((x: DepthDatum) => x.model.modelName), size = Some(2.0)))
	  ).xAxis().yAxis().frame().rightLegend()
    getPath.flatMap(path =>
      IO(plot.render().write(new java.io.File(path.toString)))
    )
  }

  @JsonCodec case class FullTuningPoint(
    verbSenseConfig: VerbSenseConfig,
    maxVerbClusterLoss: Double,
    minClauseProb: Double,
    minCoindexingProb: Double,
    lbConf: BinaryConf.Stats,
    ubConf: BinaryConf.Stats
  )

  def tuningFullEvaluation(
    config: Config,
    verbModelsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbClusterModel]],
    goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
    evaluationItems: Vector[(InflectedForms, String, Int)]
  ): IO[Map[VerbSenseConfig, FullTuningPoint]] = {
    val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
      goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
    }
    if(presentEvaluationItems.isEmpty) IO.pure {
      verbModelsByConfig.transform { case (vsConfig, _) =>
        FullTuningPoint(vsConfig, 0.0, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
      }
    } else {
      for {
        instances <- config.fullInstances.get
        _ <- writeLossGraph(
          verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
          config.globalResultsDir.map(_.resolve("loss-trends.png"))
        )
        _ <- writeDepthGraph(
          verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
          config.globalResultsDir.map(_.resolve("depth-trends.png"))
        )
        fullEvaluationResultsByConfig = verbModelsByConfig.transform { case (vsConfig, verbModels) =>
          presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
            val verbId = VerbId(sentenceId, verbIndex)
            val goldQs = instances(verbInflectedForms)(sentenceId)(verbIndex).keySet
            val goldClausalQs = ClauseResolution.getResolvedStructures(goldQs.toList).toSet
              .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
            val goldClauseSet = goldClausalQs.map(_._1)
            val verbModel = verbModels(verbInflectedForms)
            val instanceClusters = verbModel.clusterTree.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present

            // max verb cluster loss/elt (dec) -> min clause prob (inc) -> min coindexing prob (inc) -> stats
            instanceClusters.map { tree =>
              val clusterSize = tree.size
              val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss / clusterSize
              val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
                verbModel.clauseSets(vid).toList.foldMap(c => Map(c -> 1))
              }
              val predictedClausesWithProbsIncreasing = clauseCounts
                // .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
                .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
                .sortBy(_._2)
              lossThreshold -> predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
                val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
                val frameClauses = clausesWithProbs.map(_._1).toList.toSet
                val coindexingTreeOpt = if(frameClauses.isEmpty) None else Some(
                  Coindexing.getCoindexingTree(frameClauses, verbModel.coindexingScores)
                )
                val confPairLists = goldClausalQs.toList.map { cq =>
                  val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
                  val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
                    instanceParaphrases.incorrectClauses.flatMap(ct =>
                      getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
                    )
                  coindexingTreeOpt.flatMap(_.clustersForValue(cq)) match {
                    case None =>
                      val lbConf = BinaryConf.Stats(fn = correctParaphrases.size)
                      val ubConf = lbConf
                      List(1.0 -> (lbConf -> ubConf))
                    case Some(clustersForQuestion) => clustersForQuestion.map { tree =>
                      val clusterSize = tree.size
                      val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
                      val minCoindexingProb = 1.0 - maxLoss
                      val predictedParaphrases = tree.values.toSet - cq
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
                }
                val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
                clauseThreshold -> allMinCoindexingProbs.map { minCoindexingProb =>
                  val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
                  val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
                  minCoindexingProb -> (lbConf -> ubConf)
                }
              }.toList
            }
            // clause stats
            // val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
            // val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
            // val lbConf = BinaryConf.Stats(
            //   tp = (predictedClauses intersect correctClauses).size,
            //   tn = 0, // we don't need this for p/r/f
            //   fp = (predictedClauses -- correctClauses).size,
            //   fn = (correctClauses -- predictedClauses).size
            // )
            // val ubConf = BinaryConf.Stats(
            //   tp = (predictedClauses -- incorrectClauses).size,
            //   tn = 0, // we don't need this for p/r/f
            //   fp = (predictedClauses intersect incorrectClauses).size,
            //   fn = (correctClauses -- predictedClauses).size
            // )
            // clauseThreshold -> (lbConf -> ubConf)
          }
        }
        allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
          val maxLosses = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
          maxLosses.flatMap { maxLoss =>
            val chosenClusters = evaluationItemResults.map(
              _.find(_._1 <= maxLoss).get._2
            )
            val minClauseProbs = chosenClusters.foldMap(_.map(_._1).toSet).toList.sorted
            minClauseProbs.flatMap { minClauseProb =>
              val chosenClauseSets = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
              val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList.sorted
              minCoindexingProbs.map { minCoindexingProb =>
                val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
                val lbConf = confPairs.foldMap(_._1)
                val ubConf = confPairs.foldMap(_._2)
                FullTuningPoint(vsConfig, maxLoss, minClauseProb, minCoindexingProb, lbConf, ubConf)
              }
            }
          }
        }
        bestTuningPoints <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
          val lbBest = allPoints.maxBy(_.lbConf.f1)
          val ubBest = allPoints.maxBy(_.ubConf.f1)
          val lbResString = SandboxApp.getMetricsString(lbBest.lbConf)
          val ubResString = SandboxApp.getMetricsString(ubBest.ubConf)
          println(s"${vsConfig.modelName} question lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson))
          println(s"${vsConfig.modelName} question lb metrics: " + lbResString)
          println(s"${vsConfig.modelName} question ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson))
          println(s"${vsConfig.modelName} question ub metrics: " + ubResString)

          config.resultsPath(vsConfig).flatMap(path =>
            FileUtil.writeString(path.resolve("full-lb-results.txt"))(lbResString) >>
              FileUtil.writeString(path.resolve("full-ub-results.txt"))(ubResString) >>
              FileUtil.writeJson(path.resolve("full-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
              FileUtil.writeJson(path.resolve("full-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
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
            PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
          }
          val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
            PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
          }

          val lbPlot = ScatterPlot(
		        lbData,
		        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()
          val ubPlot = ScatterPlot(
		        ubData,
		        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	        ).xAxis().yAxis().frame().rightLegend()

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

          // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
          config.globalResultsDir.flatMap(path =>
            IO(lbPlot.render().write(new java.io.File(path.resolve("full-question-lb.png").toString))) >>
              IO(ubPlot.render().write(new java.io.File(path.resolve("full-question-ub.png").toString)))
          )
        }


      } yield bestTuningPoints
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

  val allVerbSenseConfigs = {
    List(VerbSenseConfig.SingleCluster, VerbSenseConfig.EntropyOnly, VerbSenseConfig.ELMoOnly) ++
      List(VerbSenseConfig.Interpolated(0.5))
    // (1 to 9).map(_.toDouble / 10.0).toList.map(VerbSenseConfig.Interpolated(_))
  }

  def runQasrlFrameInduction(config: Config, modelOpt: Option[VerbSenseConfig]): IO[ExitCode] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(allVerbSenseConfigs)
    // TODO read in tuned thresholds (in case of test)
    for {
      verbModelsByConfig <- verbSenseConfigs.traverse(vsConfig =>
        getQasrlVerbClusterModels(config, vsConfig).map(vsConfig -> _)
      ).map(_.toMap)
      goldParaphrases <- config.readGoldParaphrases
      evaluationItems <- config.evaluationItems.get
      tunedThresholds <- logOp(
        "Evaluating and tuning thresholds",
        tuningFullEvaluation(config, verbModelsByConfig, goldParaphrases, evaluationItems)
      )
    } yield ExitCode.Success
  }

  def runPropBankFrameInduction(config: Config, modelOpt: Option[VerbSenseConfig]): IO[ExitCode] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(allVerbSenseConfigs)
    // TODO read in tuned threshold (in case of test)
    for {
      _ <- IO.unit
      // verbModelsByConfig <- verbSenseConfigs.traverse(vsConfig =>
      //   getPropBankVerbClusterModels(config, vsConfig).map(vsConfig -> _)
      // ).map(_.toMap)
      // goldParaphrases <- config.readGoldParaphrases
      // evaluationItems <- config.evaluationItems.get
      // tunedThresholds <- logOp(
      //   "Evaluating and tuning thresholds",
      //   tuningFullEvaluation(config, verbModelsByConfig, goldParaphrases, evaluationItems)
      // )
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

    val isPropbankO = Opts.flag(
      "propbank", help = "run the PropBank verb sense evaluation."
    ).orFalse

    (modeO, verbSenseConfigOptO, isPropbankO).mapN { (mode, verbSenseConfigOpt, isPropbank) =>
      if(isPropbank) runPropBankFrameInduction(Config(mode), verbSenseConfigOpt)
      else runQasrlFrameInduction(Config(mode), verbSenseConfigOpt)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
