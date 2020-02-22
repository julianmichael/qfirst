package qfirst.frame
import qfirst.frame.models._
import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
// import qfirst.frame.browse._
// import qfirst.model.eval.protocols.SimpleQAs
import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.Validated
import cats.implicits._

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}

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

import scala.collection.immutable.Map

import scala.util.Random

import breeze.linalg._
import scala.collection.immutable.Vector

import freelog._
import freelog.implicits._

// TODO applicative
case class Instances[VerbType, A](
  values: Map[VerbType, Map[String, Map[Int, A]]]
) {
  def map[B](f: A => B): Instances[VerbType, B] = {
    Instances(
      values.transform { case (k, v) =>
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
object Instances {
  implicit def instancesMonoid[VT, A]: Monoid[Instances[VT, A]] =
    new Monoid[Instances[VT, A]] {
      def empty: Instances[VT, A] = Instances(Map())
      def combine(x: Instances[VT, A], y: Instances[VT, A]) = {
        Instances(
          (x.values.keySet ++ y.values.keySet).iterator.map { verbType =>
            val xSents = x.values.getOrElse(verbType, Map())
            val ySents = y.values.getOrElse(verbType, Map())
            verbType -> (xSents.keySet ++ ySents.keySet).iterator.map { sid =>
              val xVerbs = xSents.getOrElse(sid, Map())
              val yVerbs = ySents.getOrElse(sid, Map())
              sid -> (xVerbs ++ yVerbs)
            }.toMap
          }.toMap
        )
      }
    }
  type PropBank = Instances[String, FrameInductionApp.QAPairs]
  type PropBankElmo = Instances[String, DenseVector[Float]]
  type PropBankLabels = Instances[String, String]
  type Qasrl = Instances[InflectedForms, FrameInductionApp.QAPairs]
  type QasrlElmo = Instances[InflectedForms, DenseVector[Float]]
}

object FrameInductionApp extends CommandIOApp(
  name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
  header = "Induce verb frames.") {

  implicit val logLevel = LogLevel.Trace

  type QAPairs = Map[SlotBasedLabel[VerbForm], List[List[ESpan]]]
  type ClausalQ = (ArgStructure, ArgumentSlot)

  def makeVerbSpecificClauseVocab(instances: Map[String, Map[Int, QAPairs]]): Vocab[ArgStructure] = {
    Vocab.make(
      instances.values.toList.foldMap(verbMap =>
        verbMap.values.toList.foldMap(qMap =>
          ClauseResolution.getResolvedStructures(qMap.keys.toList).map(_._1).toSet
        )
      )
    )
  }

  def runJointClustering(
    instances: Instances[InflectedForms, QAPairs],
    verbVectors: Instances[InflectedForms, DenseVector[Float]],
    dataset: Dataset)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[InflectedForms, (MergeTree[VerbId], MergeTree[QuestionId])]] = {

    val questionQEntLambda = 2.0
    val questionTokEntLambda = 1.0

    val getLossPenalty = (numClusters: Int) => scala.math.pow(numClusters, 2.0) / 2.0

    val verbEntLambda: Double = 1.0
    val verbVecLambda: Double = 1.0
    val verbNestedLambda: Double = 1.0

    val verbs = instances.values.map { case (verbType, sentMap) =>
      verbType -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
        verbIndices.keys.toVector.filter(vi =>
          instances.values.get(verbType).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty &&
            verbVectors.values.get(verbType).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
        ).map(vi => VerbId(sid, vi))
      }
    }.filter(_._2.nonEmpty)

    verbs.toList.sortBy(-_._2.size).infoBarTraverse("Clustering verbs") { case (verbType, _verbIds) =>
      Log.trace(verbType.stem) >> IO {
        val verbIds = NonEmptyVector.fromVector(_verbIds.take(50)).get

        val verbInstances = instances.values(verbType)
        val clauseVocab = makeVerbSpecificClauseVocab(verbInstances)
        val verbClauseInstances = verbIds.map { verbId =>
          val questions = verbInstances(verbId.sentenceId)(verbId.verbIndex).keySet.toList
          val clauseCounts = ClauseResolution
            .getResolvedStructures(questions).map(_._1)
            .foldMap(c => Map(clauseVocab.getIndex(c) -> 1.0))
          verbId -> clauseCounts
        }.toVector.toMap

        val verbVectorInstances = verbIds.map { verbId =>
          verbId -> verbVectors.values(verbType)(verbId.sentenceId)(verbId.verbIndex)
        }.toVector.toMap

        val questionFeatures = instances.values(verbType).toList.flatMap { case (sid, verbs) =>
          verbs.toList.flatMap { case (verbIndex, qaPairs) =>
            val verbId = VerbId(sid, verbIndex)
            if(!verbIds.exists(_ == verbId)) List() else {
              val qaPairList = qaPairs.toList
              ClauseResolution.getResolvedFramePairs(verbType, qaPairList.map(_._1))
                .zip(qaPairList.map(_._2)).map { case ((frame, slot), answerSpans) =>
                  val qid = QuestionId(verbId, frame, slot)
                  val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
                  val sentenceTokens = dataset.sentences(sid).sentenceTokens
                  val tokenCounts = answerSpans.map(
                    _.foldMap(span =>
                      sentenceTokens.slice(span.begin, span.endExclusive)
                        .map(_.lowerCase)
                        .filter(t => !tokensToSkip.contains(t))
                        .foldMap(t => Map(t -> 1))
                    )
                  )
                  (qid, clauseTemplate -> slot, tokenCounts)
                }
            }
          }
        }

        val verbIdToQuestionIds = questionFeatures
          .groupByNel(_._1.verbId)
          .map { case (verbId, tups) => verbId -> NonEmptyVector.of(tups.head._1, tups.tail.map(_._1): _*) }

        val questionTemplateVocab = Vocab.make(questionFeatures.map(_._2).toSet)
        val tokenVocab = Vocab.make(questionFeatures.foldMap(_._3.foldMap(_.keySet)))

        val questionClauseInstances = questionFeatures.map { case (qid, questionTemplate, tokenCounts) =>
          val questionTemplateCounts = Map(
            questionTemplateVocab.getIndex(questionTemplate) -> 1.0
          )
          qid -> questionTemplateCounts
        }.toMap

        val questionTokenInstances = questionFeatures.map { case (qid, questionTemplate, tokenCounts) =>
          val tokenDists = tokenCounts.foldMap(
            _.map { case (tok, count) =>
              tokenVocab.getIndex(tok) -> (count.toDouble / tokenCounts.size)
            }
          )
          qid -> tokenDists
        }.toMap

        val questionAlgorithm = new CompositeAgglomerativeClusteringAlgorithm {
          val _1 = new MinEntropyClustering(questionClauseInstances, questionTemplateVocab.size)
          val _1Lambda = questionQEntLambda
          val _2 = new MinEntropyClustering(questionTokenInstances, tokenVocab.size)
          val _2Lambda = questionTokEntLambda
        }

        val verbAlgorithm = new CompositeAgglomerativeClusteringAlgorithm {
          val _1 = new CompositeAgglomerativeClusteringAlgorithm {
            val _1 = new MinEntropyClustering(verbClauseInstances, clauseVocab.size)
            val _1Lambda = verbEntLambda
            val _2 = new VectorMeanClustering(verbVectorInstances)
            val _2Lambda = verbVecLambda
          }
          val _1Lambda = 1.0

          val _2 = new JointAgglomerativeClusteringAlgorithm(
            questionAlgorithm,
            verbIdToQuestionIds,
            getLossPenalty
          )
          val _2Lambda = verbNestedLambda
        }
        val (verbClustering, verbParam) = verbAlgorithm.runFullAgglomerativeClustering(verbIds)
        val questionClustering = verbAlgorithm._2.innerAlgorithm
          .runPartialAgglomerativeClustering(verbParam._2).head._1
        verbType -> (verbClustering, questionClustering)
      }
    }.map(_.toMap)
  }

  def runVerbSenseAgglomerativeClustering[VerbType](
    verbSenseConfig: VerbSenseConfig.NonJoint,
    instances: Instances[VerbType, QAPairs],
    elmoVecs: Instances[VerbType, DenseVector[Float]],
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[VerbId]]] = {
    val verbs = instances.values.map { case (verbType, sentMap) =>
      verbType -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
        verbIndices.keys.toVector.filter(vi =>
          instances.values.get(verbType).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty &&
            elmoVecs.values.get(verbType).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
        ).map(vi => VerbId(sid, vi))
      }
    }.filter(_._2.nonEmpty)
    verbs.toList.sortBy(-_._2.size).infoBarTraverse("Clustering verbs") { case (verbType, _verbIds) =>
      Log.trace(renderVerbType(verbType)) >> IO {
        // hack to make it possible to even do the clustering on the really common words. need to generalize later
        val verbIds = NonEmptyVector.fromVector(_verbIds.take(50)).get

        val verbInstances = instances.values(verbType)
        val clauseVocab = makeVerbSpecificClauseVocab(verbInstances)
        val makeInstance = (verbId: VerbId) => {
          val questions = verbInstances(verbId.sentenceId)(verbId.verbIndex).keySet.toList
          val clauseCounts = ClauseResolution
            .getResolvedStructures(questions).map(_._1)
            .foldMap(c => Map(clauseVocab.getIndex(c) -> 1.0))
          val vector = elmoVecs.values(verbType)(verbId.sentenceId)(verbId.verbIndex)
          clauseCounts -> vector
        }
        val verbLabel = renderVerbType(verbType)
        val indices = verbIds.zipWithIndex.map(_._2)
        val clustering = verbSenseConfig match {
          case VerbSenseConfig.SingleCluster =>
            verbIds.reduceLeftTo(id => MergeTree.Leaf(0.0, id): MergeTree[VerbId]) {
              case (tree, next) => MergeTree.Merge(0.0, tree, MergeTree.Leaf(0.0, next))
            }
          case VerbSenseConfig.EntropyOnly =>
            val instances = verbIds.map(v => makeInstance(v)._1)
            new MinEntropyClustering(
              instances.getUnsafe(_), clauseVocab.size
            ).runFullAgglomerativeClustering(indices)._1.map(verbIds.getUnsafe)
          case VerbSenseConfig.ELMoOnly =>
            val instances = verbIds.map(v => makeInstance(v)._2)
            new VectorMeanClustering(instances.getUnsafe(_))
              .runFullAgglomerativeClustering(indices)._1.map(verbIds.getUnsafe)
          case VerbSenseConfig.Interpolated(lambda) =>
            val entropyInstances = verbIds.map(makeInstance).map(_._1)
            val vectorInstances = verbIds.map(makeInstance).map(_._2)
            val algorithm = new CompositeAgglomerativeClusteringAlgorithm {
              val _1 = new MinEntropyClustering(entropyInstances.getUnsafe(_), clauseVocab.size)
              val _2 = new VectorMeanClustering(vectorInstances.getUnsafe(_))

              val _1Lambda = lambda
              val _2Lambda = 1.0 - lambda
            }
            algorithm.runFullAgglomerativeClustering(indices)._1.map(verbIds.getUnsafe(_))
        }
        verbType -> clustering
      }
    }.map(_.toMap)//.flatTap(_ => Log.log(""))
  }

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def getQuestionClustersFromAggregateQAForVerb(
    verbInflectedForms: InflectedForms,
    instances: Map[String, Map[Int, QAPairs]],
    verbCollapsedQAOutputs: Map[(ClausalQ, ClausalQ), Double]
  ): MergeTree[QuestionId] = {
    val qidsByStructure = instances.toList.foldMap { case (sid, verbs) =>
      verbs.toList.foldMap { case (verbIndex, qaPairs) =>
        ClauseResolution.getResolvedFramePairs(verbInflectedForms, qaPairs.keys.toList).foldMap {
          case (frame, slot) =>
            val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
            Map((clauseTemplate -> slot) -> Vector(QuestionId(VerbId(sid, verbIndex), frame, slot)))
        }
      }
    }
    val baseTrees = qidsByStructure.map { case (structure, qids) =>
      structure -> MergeTree.createBalancedTree(NonEmptyList.fromList(qids.toList).get)
    }
    val clauseTemplates = qidsByStructure.keySet
    val structureTree = Coindexing.getCoindexingTree(clauseTemplates, verbCollapsedQAOutputs)
    structureTree.flatMap(baseTrees)
  }

  def getQuestionClustersFromAggregateQA(
    instances: Instances.Qasrl,
    collapsedQAOutputs: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  ): Map[InflectedForms, MergeTree[QuestionId]] = {
    instances.values.map { case (verbInflectedForms, verbInstances) =>
      verbInflectedForms -> getQuestionClustersFromAggregateQAForVerb(
        verbInflectedForms, verbInstances, collapsedQAOutputs(verbInflectedForms)
      )
    }
  }

  val pronouns = Set(
    "he", "him", "it", "she", "her", "they", "them", "we", "us"
  ).map(_.lowerCase)
  val tokensToSkip = pronouns

  def getQuestionClustersForVerb(
    verbInflectedForms: InflectedForms,
    verbIds: Set[VerbId],
    instances: Map[String, Map[Int, QAPairs]],
    data: Dataset,
    interpolationFactor: Double)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[MergeTree[QuestionId]] = {
    val featurefulInstances = instances.toList.flatMap { case (sid, verbs) =>
      verbs.toList.flatMap { case (verbIndex, qaPairs) =>
        val verbId = VerbId(sid, verbIndex)
        if(!verbIds.contains(verbId)) List() else {
          val qaPairList = qaPairs.toList
          ClauseResolution.getResolvedFramePairs(verbInflectedForms, qaPairList.map(_._1))
            .zip(qaPairList.map(_._2)).map { case ((frame, slot), answerSpans) =>
              val qid = QuestionId(verbId, frame, slot)
              val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
              val sentenceTokens = data.sentences(sid).sentenceTokens
              val tokenCounts = answerSpans.map(
                _.foldMap(span =>
                  sentenceTokens.slice(span.begin, span.endExclusive)
                    .map(_.lowerCase)
                    .filter(t => !tokensToSkip.contains(t))
                    .foldMap(t => Map(t -> 1))
                )
              )
              (qid, clauseTemplate -> slot, tokenCounts)
            }
        }
      }
    }
    val questionTemplateVocab = Vocab.make(featurefulInstances.map(_._2).toSet)
    val tokenVocab = Vocab.make(featurefulInstances.foldMap(_._3.foldMap(_.keySet)))

    val indexedInstances = featurefulInstances.map { case (qid, questionTemplate, tokenCounts) =>
      val questionTemplateCounts = Map(
        questionTemplateVocab.getIndex(questionTemplate) -> 1.0
      )
      val tokenDists = tokenCounts.foldMap(
        _.map { case (tok, count) =>
          tokenVocab.getIndex(tok) -> (count.toDouble / tokenCounts.size)
        }
      )
      questionTemplateCounts -> tokenDists
    }.toVector

    val indices = NonEmptyVector.fromVector(indexedInstances.zipWithIndex.map(_._2)).get
    val qInstances = indexedInstances.map(_._1)
    val tokInstances = indexedInstances.map(_._2)

    val algorithm = new CompositeAgglomerativeClusteringAlgorithm {
      val _1 = new MinEntropyClustering(qInstances, questionTemplateVocab.size)
      val _1Lambda = interpolationFactor
      val _2 = new MinEntropyClustering(tokInstances, tokenVocab.size)
      val _2Lambda = 1.0 - interpolationFactor
    }

    IO {
      algorithm.runFullAgglomerativeClustering(indices)._1.map(
        i => featurefulInstances(i)._1
      )
    }
  }

  def getQuestionClusters(
    instances: Instances.Qasrl,
    verbClusters: Map[InflectedForms, MergeTree[VerbId]],
    data: Dataset,
    interpolationFactor: Double = 0.8)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[InflectedForms, MergeTree[QuestionId]]] = {
    instances.values.toList
      .sortBy(-_._2.size)
      .infoBarTraverse("Clustering questions") { case (verbInflectedForms, verbInstances) =>
      Log.trace(verbInflectedForms.stem) >> {
        getQuestionClustersForVerb(
          verbInflectedForms,
          verbClusters(verbInflectedForms).values.foldMap(Set(_)),
          verbInstances,
          data, interpolationFactor
        ).map(verbInflectedForms -> _)
      }
    }.map(_.toMap)
  }

  def getQasrlVerbClusterModels(
    config: Config, verbSenseConfig: VerbSenseConfig)(
    implicit logger: EphemeralTreeLogger[IO, String]
  ): IO[Map[InflectedForms, VerbClusterModel]] = {
    config.cacheVerbModelComputation(verbSenseConfig) {
      for {
        instances <- config.fullInstances.get
        elmoVecs <- config.fullElmo.get
        dataset <- config.full.get
        verbClusters <- verbSenseConfig match {
          case VerbSenseConfig.Joint => runJointClustering(instances, elmoVecs, dataset)
          case vsc: VerbSenseConfig.NonJoint => for {
            verbClusters <- runVerbSenseAgglomerativeClustering(
              verbSenseConfig = vsc,
              instances = instances,
              elmoVecs = elmoVecs,
              renderVerbType = (forms: InflectedForms) => forms.stem
            )
            // collapsedQAOutputs <- config.collapsedQAOutputs.get
            questionClusters <- getQuestionClusters(
              instances, verbClusters, dataset
            )
          } yield (
            verbClusters.keySet intersect questionClusters.keySet
          ).toList.map(v => v -> (verbClusters(v), questionClusters(v))).toMap
        }
        verbClusterModels = verbClusters.map { case (verbInflectedForms, (verbClusterTree, questionClusterTree)) =>
          val clauseSets = instances.values(verbInflectedForms).iterator.flatMap { case (sid, verbMap) =>
            verbMap.iterator.map { case (vi, qas) =>
              val questions = qas.keySet.toList
              val clauseSet = ClauseResolution
                .getResolvedStructures(questions)
                .map(_._1).toSet
              VerbId(sid, vi) -> clauseSet
            }
          }.toMap
          verbInflectedForms -> VerbClusterModel(
            verbInflectedForms,
            verbClusterTree,
            questionClusterTree
            // clauseSets,
            // collapsedQAOutputs(verbInflectedForms).toList
          )
        }
      } yield verbClusterModels
    }
  }

  def getPropBankVerbClusterModels(
    config: Config, verbSenseConfig: VerbSenseConfig)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[String, PropBankVerbClusterModel]] = {
    config.cachePropBankVerbModelComputation(verbSenseConfig) {
      for {
        instances <- config.propBankFullInstances.get
        elmoVecs <- config.propBankFullElmo.get
        verbClusters <- runVerbSenseAgglomerativeClustering(
          verbSenseConfig = verbSenseConfig.asInstanceOf[VerbSenseConfig.NonJoint], // TODO
          instances = instances,
          elmoVecs = elmoVecs,
          renderVerbType = identity[String]
        )
        // TODO include QA outputs after indexing stuff has been fixed
        // collapsedQAOutputs <- config.propBankCollapsedQAOutputs.get
        verbClusterModels = verbClusters.map { case (verbLemma, clusterTree) =>
          val clauseSets = instances.values(verbLemma).iterator.flatMap { case (sid, verbMap) =>
            verbMap.iterator.map { case (vi, qas) =>
              val questions = qas.keySet.toList
              val clauseSet = ClauseResolution
                .getResolvedStructures(questions)
                .map(_._1).toSet
              VerbId(sid, vi) -> clauseSet
            }
          }.toMap
          verbLemma -> PropBankVerbClusterModel(
            verbLemma,
            clusterTree,
            clauseSets
            // collapsedQAOutputs(verbInflectedForms).toList
          )
        }
      } yield verbClusterModels
    }
  }

  case class PropBankEvaluationPoint(
    model: VerbSenseConfig,
    maxLoss: Double,
    precision: Double,
    recall: Double
  ) {
    val f1 = 2 * precision * recall / (precision + recall)
  }
  object PropBankEvaluationPoint {
    implicit val propBankEvaluationPointHasMetrics: HasMetrics[PropBankEvaluationPoint] = {
      new HasMetrics[PropBankEvaluationPoint] {
        def getMetrics(p: PropBankEvaluationPoint) = MapTree.fromPairs(
          "max loss" -> Metric.double(p.maxLoss),
          "precision" -> Metric.double(p.precision),
          "recall" -> Metric.double(p.recall),
          "f1" -> Metric.double(p.f1)
        )
      }
    }
  }

  def evaluatePropBankVerbClusters(
    config: Config,
    verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
    propBankSenseLabels: Instances.PropBankLabels)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbSenseConfig, PropBankEvaluationPoint]] = {
    for {
      instances <- config.propBankFullInstances.get
      fullEvaluationResultsByConfig <- verbModelsByConfig.toList.infoBarTraverse("Tuning all verb models") { case (vsConfig, modelsByVerb) =>
        Log.info(vsConfig.modelName) >>
          modelsByVerb.toList.sortBy(-_._2.clauseSets.size).infoBarTraverse("Tuning verbs") { case (verbLemma, verbModel) =>
            propBankSenseLabels.values.get(verbLemma).foldMapM { verbSenseLabels =>
              Log.trace(verbLemma) >> IO {
                val predictedVerbIds = verbModel.clusterTree.unorderedFoldMap(Set(_))
                // only includes IDs that were covered in the predictions as well
                val verbSenseToIds = verbSenseLabels.toList.foldMap { case (sentenceId, verbMap) =>
                  verbMap.toList.foldMap { case (verbIndex, verbSense) =>
                    Option(VerbId(sentenceId, verbIndex))
                      .filter(predictedVerbIds.contains)
                      .foldMap(vid => Map(verbSense -> Set(vid)))
                  }
                }
                verbSenseLabels.iterator.flatMap { case (sentenceId, verbMap) =>
                  verbMap.iterator.flatMap { case (verbIndex, verbSense) =>
                    verbModel.clusterTree.clustersForValue(VerbId(sentenceId, verbIndex)).iterator.map { clusters =>
                      val goldIds = verbSenseToIds(verbSense)
                      clusters.map { cluster =>
                        val predictedIds = cluster.values.toSet
                        val maxLoss = if(cluster.isLeaf) 0.0 else (cluster.loss / predictedIds.size)
                        val tp = (predictedIds intersect goldIds).size
                        val precision = tp / predictedIds.size
                        val recall = tp / goldIds.size
                        PropBankEvaluationPoint(vsConfig, maxLoss, precision, recall)
                      }
                    }
                  }
                }.toList
              }
            }.map(_.flatten)
          }.map(vsConfig -> _)
      }.map(_.toMap)
      allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
        val lossThresholds = evaluationItemResults.foldMap(_.map(_.maxLoss).toSet).toList.sortBy(-_)
        lossThresholds.map { maxLoss =>
          val chosenResults = evaluationItemResults.map(
            _.find(_.maxLoss <= maxLoss).get
          )
          PropBankEvaluationPoint(
            vsConfig,
            maxLoss,
            chosenResults.map(_.precision).sum / chosenResults.size,
            chosenResults.map(_.recall).sum / chosenResults.size
          )
        }
      }
      bestModels <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
        val best = allPoints.maxBy(_.f1)
        val resString = getMetricsString(best)
        Log.info(s"${vsConfig.modelName} propbank metrics: " + resString) >>
          config.propBankResultsPath(vsConfig).flatMap(path =>
            FileUtil.writeString(path.resolve("propbank-results.txt"))(resString)
          ).as(vsConfig -> best)
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
        def noise = scala.math.abs(rand.nextGaussian / 200.0)

        val data = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
          PRPoint(vsTuningPoint.model, vsTuningPoint.recall + noise, vsTuningPoint.precision + noise)
        }

        val plot = ScatterPlot(
		      data,
		      pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	      ).xAxis().yAxis().frame().rightLegend()

        config.globalPropBankResultsDir.flatMap(path =>
          IO(plot.render().write(new java.io.File(path.resolve("propbank-sense.png").toString)))
        )
      }
    } yield bestModels
  }

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
    evaluationItems: Vector[(InflectedForms, String, Int)])(
    implicit Log: EphemeralTreeLogger[IO, String]
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
            val verbClauseSets = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
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
          val lbResString = getMetricsString(lbBest.lbConf)
          val ubResString = getMetricsString(ubBest.ubConf)
          Log.info(s"${vsConfig.modelName} clause lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
            Log.info(s"${vsConfig.modelName} clause lb metrics: " + lbResString) >>
            Log.info(s"${vsConfig.modelName} clause ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
            Log.info(s"${vsConfig.modelName} clause ub metrics: " + ubResString) >>
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
          def noise = scala.math.abs(rand.nextGaussian / 200.0)

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
    instances: Instances.Qasrl,
    verbSenseModel: VerbSenseTuningPoint,
    verbClusters: Map[InflectedForms, MergeTree[VerbId]],
    fuzzyArgEquivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  ): Map[InflectedForms, VerbFrameset] = {
    verbClusters.transform { case (verbInflectedForms, tree) =>
      val numInstancesForVerb = tree.size
      val verbClauseSets = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
        verbMap.map { case (vi, qMap) =>
          val questions = qMap.keySet.toList
          val clauses = ClauseResolution.getResolvedFramePairs(
            verbInflectedForms, questions
          ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
          VerbId(sid, vi) -> clauses
        }
      }
      val clusterTrees = tree.splitWhile(t => (t.loss / numInstancesForVerb) > verbSenseModel.maxLoss)
      val verbRel = fuzzyArgEquivalences(verbInflectedForms)
      val verbFrames = clusterTrees.map { frameTree =>
        val verbIds = frameTree.values
        val clauseTemplates = verbIds.foldMap { vid =>
          verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
        }.toList.map { case (c, count) => FrameClause(c, (count.toDouble / verbIds.size)) }
        val frameProb = verbIds.size.toDouble / numInstancesForVerb
        val questionClusterTree = getQuestionClustersFromAggregateQAForVerb(
          verbInflectedForms, instances.values(verbInflectedForms), fuzzyArgEquivalences(verbInflectedForms)
        )
        // val coindexingTree = Coindexing.getCoindexingTree(clauseTemplates.map(_.args).toSet, verbRel)
        VerbFrame(verbIds.toSet, clauseTemplates, questionClusterTree, frameProb)
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

  // def tuningParaphraseEvaluation(
  //   config: Config,
  //   verbFramesetsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbFrameset]],
  //   goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
  //   evaluationItems: Vector[(InflectedForms, String, Int)])(
  //   implicit Log: Logger[IO, String]
  // ): IO[Map[VerbSenseConfig, ParaphraseTuningPoint]] = {
  //   val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
  //     goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
  //   }
  //   if(presentEvaluationItems.isEmpty) IO.pure {
  //     verbFramesetsByConfig.transform { case (vsConfig, verbClusters) =>
  //       ParaphraseTuningPoint(vsConfig, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
  //     }
  //   } else {
  //     for {
  //       instances <- config.fullInstances.get
  //       fullEvaluationResultsByConfig = verbFramesetsByConfig.transform { case (vsConfig, framesets) =>
  //         presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
  //           val verbClausalQs = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
  //             verbMap.map { case (vi, qMap) =>
  //               val questions = qMap.keySet.toList
  //               val clausalQs = ClauseResolution.getResolvedStructures(questions).toSet
  //                 .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
  //               VerbId(sid, vi) -> clausalQs
  //             }
  //           }
  //           val verbId = VerbId(sentenceId, verbIndex)
  //           val inputClausalQs = verbClausalQs(verbId)
  //           val verbFrameset = framesets(verbInflectedForms)
  //           val verbFrame = verbFrameset.frames.find(_.verbIds.contains(verbId)).get
  //           // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
  //           val paraphrasingMetricByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = {
  //             verbFrame.clauseTemplates.sortBy(_.probability).tails.toList.map { clauses =>
  //               val minClauseProb = clauses.headOption.fold(1.0)(_.probability)
  //               val clauseTemplates = clauses.map(_.args).toSet
  //               // each list is sorted increasing by minCoindexingProb
  //               val confPairLists = inputClausalQs.toList.map { cq =>
  //                 verbFrame.coindexingTree.clustersForValue(cq).get.map { tree =>
  //                   val clusterSize = tree.size
  //                   val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
  //                   val minCoindexingProb = 1.0 - maxLoss
  //                   val predictedParaphrases = tree.values.toSet - cq
  //                   val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
  //                   val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
  //                     instanceParaphrases.incorrectClauses.flatMap(ct =>
  //                       getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
  //                     )

  //                   val lbConf = BinaryConf.Stats(
  //                     tp = (predictedParaphrases intersect correctParaphrases).size,
  //                     tn = 0, // we don't need this for p/r/f
  //                     fp = (predictedParaphrases -- correctParaphrases).size,
  //                     fn = (correctParaphrases -- predictedParaphrases).size
  //                   )
  //                   val ubConf = BinaryConf.Stats(
  //                     tp = (predictedParaphrases -- incorrectParaphrases).size,
  //                     tn = 0, // we don't need this for p/r/f
  //                     fp = (predictedParaphrases intersect incorrectParaphrases).size,
  //                     fn = (correctParaphrases -- predictedParaphrases).size
  //                   )
  //                   minCoindexingProb -> (lbConf -> ubConf)
  //                 }
  //               }
  //               val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
  //               minClauseProb -> allMinCoindexingProbs.map { minCoindexingProb =>
  //                 val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
  //                 val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
  //                 minCoindexingProb -> (lbConf -> ubConf)
  //               }
  //             }
  //           }
  //           paraphrasingMetricByThresholds
  //         }
  //       }
  //       allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  //         val minClauseProbs = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sorted
  //         minClauseProbs.flatMap { minClauseProb =>
  //           val chosenClauseSets = evaluationItemResults.map(
  //             _.find(_._1 >= minClauseProb).get._2
  //           )
  //           val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList
  //           minCoindexingProbs.map { minCoindexingProb =>
  //             val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
  //             val lbConf = confPairs.foldMap(_._1)
  //             val ubConf = confPairs.foldMap(_._2)
  //             ParaphraseTuningPoint(vsConfig, minClauseProb, minCoindexingProb, lbConf, ubConf)
  //           }
  //         }
  //       }
  //       bestThresholds <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  //         val lbBest = allPoints.maxBy(_.lbConf.f1)
  //         val ubBest = allPoints.maxBy(_.ubConf.f1)
  //         // val ubBest = allPoints.maxBy(_.ubConf.f1)
  //         // val ubBestThresholds = CalibratedVerbSenseModel(
  //         //   vsConfig, verbClustersByConfig(vsConfig), ubBest.maxLoss, ubBest.minClauseProb
  //         // )
  //         // (lbBestModel, lbBest.lbConf, ubBestModel, ubBest.ubConf)
  //         // Log.info(s"${vsConfig.modelName}: " + getMetricsString(ubBest.ubConf))

  //         val lbResString = getMetricsString(lbBest.lbConf)
  //         val ubResString = getMetricsString(ubBest.ubConf)
  //         Log.info(s"${vsConfig.modelName} paraphrase lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} paraphrase lb metrics: " + lbResString) >>
  //           Log.info(s"${vsConfig.modelName} paraphrase ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} paraphrase ub metrics: " + ubResString) >>
  //           config.resultsPath(vsConfig).flatMap(path =>
  //             FileUtil.writeString(path.resolve("questions-lb-results.txt"))(lbResString) >>
  //               FileUtil.writeString(path.resolve("questions-ub-results.txt"))(ubResString) >>
  //               FileUtil.writeJson(path.resolve("questions-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
  //               FileUtil.writeJson(path.resolve("questions-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
  //           ).as(vsConfig -> ubBest)
  //       }.map(_.toMap)
  //       _ <- {
  //         import com.cibo.evilplot._
  //         import com.cibo.evilplot.numeric._
  //         import com.cibo.evilplot.plot._
  //         import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //         import com.cibo.evilplot.plot.renderers._

  //         case class PRPoint(
  //           model: VerbSenseConfig,
  //           recall: Double,
  //           precision: Double) extends Datum2d[PRPoint] {
  //           val x = recall
  //           val y = precision
  //           def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  //         }

  //         // val linePointsByModel = allPointsByModel.transform { case (model, vsTuningPoints) =>
  //         //   NonEmptyList.fromList(vsTuningPoints.sortBy(-_.lbConf.recall)).get
  //         //     .reduceLeftTo(NonEmptyList.of(_)) { (best, next) =>
  //         //       if(next.lbConf.precision > best.head.lbConf.precision) {
  //         //         if(next.lbConf.recall == best.head.lbConf.recall) best
  //         //         else NonEmptyList(next, best.toList)
  //         //       } else best
  //         //     }.toList
  //         // }
  //         // val lbLineData = linePointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //         //   PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall, vsTuningPoint.lbConf.precision)
  //         // }
  //         // val lbLinePlot = LinePlot(
	// 	      //   lbData,
	// 	      //   pathRenderer = Some(PathRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         // ).xAxis().yAxis().frame().rightLegend()


  //         val rand = new scala.util.Random(2643642L)
  //         def noise = scala.math.abs(rand.nextGaussian / 200.0)

  //         val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
  //         }
  //         val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
  //         }

  //         val lbPlot = ScatterPlot(
	// 	        lbData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()
  //         val ubPlot = ScatterPlot(
	// 	        ubData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()

  //         config.globalResultsDir.flatMap(path =>
  //           // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
  //             IO(lbPlot.render().write(new java.io.File(path.resolve("question-lb.png").toString))) >>
  //             IO(ubPlot.render().write(new java.io.File(path.resolve("question-ub.png").toString)))
  //         )
  //       }
  //     } yield bestThresholds
  //   }
  // }

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

  def writeLossGraph[VerbType](
    verbClustersByConfig: Map[VerbSenseConfig, Map[VerbType, MergeTree[VerbId]]],
    getPath: IO[NIOPath]
  ): IO[Unit] = {
    import com.cibo.evilplot._
    import com.cibo.evilplot.numeric._
    import com.cibo.evilplot.plot._
    import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
    import com.cibo.evilplot.plot.renderers.PointRenderer

    case class LossDatum(
      model: VerbSenseConfig,
      numInstances: Long,
      maxLoss: Double,
      val x: Double,
      val y: Double
    ) extends Datum2d[LossDatum] {
      def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
    }
    val lossData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
      clustersByVerb.toList.map { case (_, clusterTree) =>
        val numInstances = clusterTree.size
        LossDatum(vsConfig, numInstances, clusterTree.loss, numInstances.toDouble, clusterTree.loss)
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

  def writeDepthGraph[VerbType](
    verbClustersByConfig: Map[VerbSenseConfig, Map[VerbType, MergeTree[VerbId]]],
    getPath: IO[NIOPath]
  ): IO[Unit] = {
    import com.cibo.evilplot._
    import com.cibo.evilplot.numeric._
    import com.cibo.evilplot.plot._
    import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
    import com.cibo.evilplot.plot.renderers.PointRenderer

    case class DepthDatum(
      model: VerbSenseConfig,
      numInstances: Long,
      avgDepth: Double,
      val x: Double,
      val y: Double
    ) extends Datum2d[DepthDatum] {
      def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
    }
    val depthData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
      clustersByVerb.toList.map { case (_, clusterTree) =>
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
          merge = (_, l, r) => l.merge(r)
        ).avgDepth
        DepthDatum(vsConfig, numInstances, avgDepth, numInstances.toDouble, avgDepth)
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

  // def tuningFullEvaluation(
  //   config: Config,
  //   verbModelsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbClusterModel]],
  //   goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
  //   evaluationItems: Vector[(InflectedForms, String, Int)])(
  //   implicit Log: TreeLogger[IO, String]
  // ): IO[Map[VerbSenseConfig, FullTuningPoint]] = {
  //   val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
  //     goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
  //   }
  //   if(presentEvaluationItems.isEmpty) {
  //     Log.warn("No gold items to evaluate on. Returning empty evaluation.").as(
  //       verbModelsByConfig.transform { case (vsConfig, _) =>
  //         FullTuningPoint(vsConfig, 0.0, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
  //       }
  //     )
  //   } else {
  //     for {
  //       _ <- Log.infoBranch("Writing loss graph")(
  //         writeLossGraph(
  //           verbModelsByConfig.mapValues(_.mapValues(_.verbClusterTree)),
  //           config.globalResultsDir.map(_.resolve("loss-trends.png"))
  //         )
  //       )
  //       _ <- Log.infoBranch("Writing depth graph")(
  //         writeDepthGraph(
  //           verbModelsByConfig.mapValues(_.mapValues(_.verbClusterTree)),
  //           config.globalResultsDir.map(_.resolve("depth-trends.png"))
  //         )
  //       )
  //       instances <- config.fullInstances.get
  //       fullEvaluationResultsByConfig = verbModelsByConfig.transform { case (vsConfig, verbModels) =>
  //         presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
  //           val verbId = VerbId(sentenceId, verbIndex)
  //           val goldQs = instances.values(verbInflectedForms)(sentenceId)(verbIndex).keySet
  //           val goldClausalQs = ClauseResolution.getResolvedStructures(goldQs.toList).toSet
  //             .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
  //           val goldClauseSet = goldClausalQs.map(_._1)
  //           val verbModel = verbModels(verbInflectedForms)
  //           val instanceClusters = verbModel.verbClusterTree.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present

  //           // max verb cluster loss/elt (dec) -> min clause prob (inc) -> min coindexing prob (inc) -> stats
  //           instanceClusters.map { tree =>
  //             val clusterSize = tree.size
  //             val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss / clusterSize
  //             val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
  //               verbModel.clauseSets(vid).toList.foldMap(c => Map(c -> 1))
  //             }
  //             val predictedClausesWithProbsIncreasing = clauseCounts
  //               // .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
  //               .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
  //               .sortBy(_._2)
  //             lossThreshold -> predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
  //               val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
  //               val frameClauses = clausesWithProbs.map(_._1).toList.toSet
  //               val coindexingTreeOpt = if(frameClauses.isEmpty) None else Some(
  //                 Coindexing.getCoindexingTree(frameClauses, verbModel.coindexingScores)
  //               )
  //               val confPairLists = goldClausalQs.toList.map { cq =>
  //                 val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
  //                 val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
  //                   instanceParaphrases.incorrectClauses.flatMap(ct =>
  //                     getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
  //                   )
  //                 coindexingTreeOpt.flatMap(_.clustersForValue(cq)) match {
  //                   case None =>
  //                     val lbConf = BinaryConf.Stats(fn = correctParaphrases.size)
  //                     val ubConf = lbConf
  //                     List(1.0 -> (lbConf -> ubConf))
  //                   case Some(clustersForQuestion) => clustersForQuestion.map { tree =>
  //                     val clusterSize = tree.size
  //                     val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
  //                     val minCoindexingProb = 1.0 - maxLoss
  //                     val predictedParaphrases = tree.values.toSet - cq
  //                     val lbConf = BinaryConf.Stats(
  //                       tp = (predictedParaphrases intersect correctParaphrases).size,
  //                       tn = 0, // we don't need this for p/r/f
  //                       fp = (predictedParaphrases -- correctParaphrases).size,
  //                       fn = (correctParaphrases -- predictedParaphrases).size
  //                     )
  //                     val ubConf = BinaryConf.Stats(
  //                       tp = (predictedParaphrases -- incorrectParaphrases).size,
  //                       tn = 0, // we don't need this for p/r/f
  //                       fp = (predictedParaphrases intersect incorrectParaphrases).size,
  //                       fn = (correctParaphrases -- predictedParaphrases).size
  //                     )
  //                     minCoindexingProb -> (lbConf -> ubConf)
  //                   }
  //                 }
  //               }
  //               val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
  //               clauseThreshold -> allMinCoindexingProbs.map { minCoindexingProb =>
  //                 val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
  //                 val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
  //                 minCoindexingProb -> (lbConf -> ubConf)
  //               }
  //             }.toList
  //           }
  //           // clause stats
  //           // val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
  //           // val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
  //           // val lbConf = BinaryConf.Stats(
  //           //   tp = (predictedClauses intersect correctClauses).size,
  //           //   tn = 0, // we don't need this for p/r/f
  //           //   fp = (predictedClauses -- correctClauses).size,
  //           //   fn = (correctClauses -- predictedClauses).size
  //           // )
  //           // val ubConf = BinaryConf.Stats(
  //           //   tp = (predictedClauses -- incorrectClauses).size,
  //           //   tn = 0, // we don't need this for p/r/f
  //           //   fp = (predictedClauses intersect incorrectClauses).size,
  //           //   fn = (correctClauses -- predictedClauses).size
  //           // )
  //           // clauseThreshold -> (lbConf -> ubConf)
  //         }
  //       }
  //       allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  //         val maxLosses = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
  //         maxLosses.flatMap { maxLoss =>
  //           val chosenClusters = evaluationItemResults.map(
  //             _.find(_._1 <= maxLoss).get._2
  //           )
  //           val minClauseProbs = chosenClusters.foldMap(_.map(_._1).toSet).toList.sorted
  //           minClauseProbs.flatMap { minClauseProb =>
  //             val chosenClauseSets = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
  //             val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList.sorted
  //             minCoindexingProbs.map { minCoindexingProb =>
  //               val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
  //               val lbConf = confPairs.foldMap(_._1)
  //               val ubConf = confPairs.foldMap(_._2)
  //               FullTuningPoint(vsConfig, maxLoss, minClauseProb, minCoindexingProb, lbConf, ubConf)
  //             }
  //           }
  //         }
  //       }
  //       bestTuningPoints <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  //         val lbBest = allPoints.maxBy(_.lbConf.f1)
  //         val ubBest = allPoints.maxBy(_.ubConf.f1)
  //         val lbResString = getMetricsString(lbBest.lbConf)
  //         val ubResString = getMetricsString(ubBest.ubConf)

  //         Log.info(s"${vsConfig.modelName} question lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} question lb metrics: " + lbResString) >>
  //           Log.info(s"${vsConfig.modelName} question ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} question ub metrics: " + ubResString) >>
  //           config.resultsPath(vsConfig).flatMap(path =>
  //             FileUtil.writeString(path.resolve("full-lb-results.txt"))(lbResString) >>
  //               FileUtil.writeString(path.resolve("full-ub-results.txt"))(ubResString) >>
  //               FileUtil.writeJson(path.resolve("full-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
  //               FileUtil.writeJson(path.resolve("full-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
  //           ).as(vsConfig -> ubBest)
  //       }.map(_.toMap)
  //       _ <- {
  //         import com.cibo.evilplot._
  //         import com.cibo.evilplot.numeric._
  //         import com.cibo.evilplot.plot._
  //         import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //         import com.cibo.evilplot.plot.renderers.PointRenderer

  //         case class PRPoint(
  //           model: VerbSenseConfig,
  //           recall: Double,
  //           precision: Double) extends Datum2d[PRPoint] {
  //           val x = recall
  //           val y = precision
  //           def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  //         }

  //         val rand = new scala.util.Random(2643642L)
  //         def noise = scala.math.abs(rand.nextGaussian / 200.0)

  //         val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
  //         }
  //         val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
  //         }

  //         val lbPlot = ScatterPlot(
	// 	        lbData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()
  //         val ubPlot = ScatterPlot(
	// 	        ubData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()

  //         // val linePointsByModel = allPointsByModel.transform { case (model, vsTuningPoints) =>
  //         //   NonEmptyList.fromList(vsTuningPoints.sortBy(-_.lbConf.recall)).get
  //         //     .reduceLeftTo(NonEmptyList.of(_)) { (best, next) =>
  //         //       if(next.lbConf.precision > best.head.lbConf.precision) {
  //         //         if(next.lbConf.recall == best.head.lbConf.recall) best
  //         //         else NonEmptyList(next, best.toList)
  //         //       } else best
  //         //     }.toList
  //         // }
  //         // val lbLineData = linePointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //         //   PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall, vsTuningPoint.lbConf.precision)
  //         // }
  //         // val lbLinePlot = LinePlot(
	// 	      //   lbData,
	// 	      //   pathRenderer = Some(PathRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         // ).xAxis().yAxis().frame().rightLegend()

  //         // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
  //         config.globalResultsDir.flatMap(path =>
  //           IO(lbPlot.render().write(new java.io.File(path.resolve("full-question-lb.png").toString))) >>
  //             IO(ubPlot.render().write(new java.io.File(path.resolve("full-question-ub.png").toString)))
  //         )
  //       }
  //     } yield bestTuningPoints
  //   }
  // }

  def doPropBankClusterDebugging(
    config: Config,
    senseLabels: Instances.PropBankLabels,
    verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
    chosenThresholds: Map[VerbSenseConfig, Double])(
    implicit Log: TreeLogger[IO, String]
  ): IO[Unit] = for {
    _ <- Log.infoBranch("Writing loss graph")(
      writeLossGraph(
        verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
        config.globalResultsDir.map(_.resolve("loss-trends.png"))
      )
    )
    _ <- Log.infoBranch("Writing depth graph")(
      writeDepthGraph(
        verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
        config.globalResultsDir.map(_.resolve("depth-trends.png"))
      )
    )
    _ <- Log.infoBranch("Writing PropBank gold sense graph") {
      import com.cibo.evilplot._
      import com.cibo.evilplot.numeric._
      import com.cibo.evilplot.plot._
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      import com.cibo.evilplot.plot.renderers.PointRenderer

      case class ThisPoint(
        lemma: String,
        numOccurrences: Int,
        numSenses: Int,
        val x: Double,
        val y: Double) extends Datum2d[ThisPoint] {
        def withXY(x: Double = this.x, y: Double = this.x) = this.copy(x = x, y = y)
      }

      val rand = new scala.util.Random(2643642L)
      def noise = scala.math.abs(rand.nextGaussian / 40.0)

      val data = senseLabels.values.iterator.map { case (lemma, sentenceMap) =>
        val instances = sentenceMap.iterator.flatMap(_._2.values.iterator).toList
        val senses = instances.toSet
        ThisPoint(lemma, instances.size, senses.size, scala.math.min(1000, instances.size + noise), senses.size + (noise * 10))
      }.toList

      val plot = ScatterPlot(
		    data,
		    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => "gold"), size = Some(2.0)))
	    ).xAxis().yAxis().frame().rightLegend()

      config.globalPropBankResultsDir.flatMap(path =>
        IO(plot.render().write(new java.io.File(path.resolve("propbank-sense-counts.png").toString)))
      )
    }
    _ <- Log.infoBranch("Writing partition sizes graph") {
      import com.cibo.evilplot._
      import com.cibo.evilplot.numeric._
      import com.cibo.evilplot.plot._
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      import com.cibo.evilplot.plot.renderers.PointRenderer

      case class ThisPoint(
        model: VerbSenseConfig,
        numInstances: Int,
        numClusters: Int,
        x: Double, y: Double
      ) extends Datum2d[ThisPoint] {
        def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
      }

      val rand = new scala.util.Random(2643642L)
      def noise = scala.math.abs(rand.nextGaussian / 40.0)

      val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
        val maxLossPerInstance = chosenThresholds(vsConfig)
        verbModels.values.toList.map { model =>
          val numInstances = model.clusterTree.size.toInt
          val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
          val numClusters = clusters.size
          ThisPoint(vsConfig, numInstances, numClusters, scala.math.min(1000, numInstances.toDouble + noise), numClusters.toDouble + (noise * 10))
        }
      }

      val plot = ScatterPlot(
		    data,
		    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	    ).xAxis().yAxis().frame().rightLegend()

      config.globalPropBankResultsDir.flatMap(path =>
        IO(plot.render().write(new java.io.File(path.resolve("predicted-cluster-counts.png").toString)))
      )
    }
    _ <- Log.infoBranch("Writing partition size comparison graph") {
      import com.cibo.evilplot._
      import com.cibo.evilplot.numeric._
      import com.cibo.evilplot.plot._
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      import com.cibo.evilplot.plot.renderers.PointRenderer

      case class ThisPoint(
        model: VerbSenseConfig,
        numGoldClusters: Int,
        numPredictedClusters: Int,
        x: Double, y: Double
      ) extends Datum2d[ThisPoint] {
        def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
      }

      val rand = new scala.util.Random(2643642L)
      def noise = scala.math.abs(rand.nextGaussian / 4.0)

      val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
        val maxLossPerInstance = chosenThresholds(vsConfig)
        verbModels.toList.flatMap { case (verbLemma, model) =>
          senseLabels.values.get(verbLemma).map { verbSenseLabels =>
            val numGoldClusters = verbSenseLabels.values.iterator.flatMap(_.values.iterator).toSet.size
            val numInstances = model.clusterTree.size
            val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
            val numPredictedClusters = clusters.size
            ThisPoint(vsConfig, numGoldClusters, numPredictedClusters, numGoldClusters.toDouble + noise, numPredictedClusters.toDouble + noise)
          }
        }
      }

      val pearsonR = {
        val num = data.map(d => d.numGoldClusters * d.numPredictedClusters).sum
        val denomGold = data.map(d => d.numGoldClusters * d.numGoldClusters).sum
        val denomPredicted = data.map(d => d.numPredictedClusters * d.numPredictedClusters).sum
        num.toDouble / scala.math.exp((scala.math.log(denomGold) + scala.math.log(denomPredicted)) / 2)
      }

      val plot = ScatterPlot(
		    data,
		    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	    ).xAxis().yAxis().frame().rightLegend()

      Log.info("Pearson's R between gold and predicted number of senses: " + pearsonR) >>
        config.globalPropBankResultsDir.flatMap(path =>
          IO(plot.render().write(new java.io.File(path.resolve("cluster-num-correlation.png").toString)))
        )
    }
    // _ <- verbModelsByConfig.toList.traverse { case (vsConfig, verbModels) =>
    //   val maxLoss = chosenThresholds(vsConfig)
    //   verbModels.toList.traverse { case (verbLemma, verbModel) =>
    //     val clusters = verbModel.clusterTree.splitWhile(_.loss > maxLoss)
    //     IO.unit // TODO
    //   }
    // }
  } yield ()

  // TODO: elmo loss is ~175x greater; tune around this number
  // _ <- {
  //   import qfirst.metrics._
  //   val dist = Numbers(verbClusters.values.toVector.filterNot(_.loss == 0.0).map(tree => tree.loss / tree.size))
  //   Log.info(getMetricsString(dist))
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

  def runQasrlFrameInduction(
    config: Config, modelOpt: Option[VerbSenseConfig])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(allVerbSenseConfigs)
    // TODO read in tuned thresholds (in case of test)
    for {
      _ <- Log.info(s"Running frame induction on QA-SRL. Models: ${verbSenseConfigs.mkString(", ")}")
      verbModelsByConfig <- verbSenseConfigs.traverse(vsConfig =>
        Log.infoBranch(s"Clustering for model: $vsConfig") {
          getQasrlVerbClusterModels(config, vsConfig).map(vsConfig -> _)
        }
      ).map(_.toMap)
      // goldParaphrases <- config.readGoldParaphrases
      // evaluationItems <- config.evaluationItems.get
      // tunedThresholds <- Log.infoBranch("Evaluating and tuning thresholds")(
      //   tuningFullEvaluation(config, verbModelsByConfig, goldParaphrases, evaluationItems)
      // )
    } yield ()
  }

  def runPropBankFrameInduction(
    config: Config, modelOpt: Option[VerbSenseConfig])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(allVerbSenseConfigs)
    // TODO read in tuned threshold (in case of test)
    val chosenThresholds = Option(
      Map[VerbSenseConfig, Double](
        VerbSenseConfig.SingleCluster -> 0.0,
        VerbSenseConfig.EntropyOnly -> 1.115,
        VerbSenseConfig.ELMoOnly -> 0.718,
        VerbSenseConfig.Interpolated(0.5) -> 1.105
      )
    )
    for {
      verbModelsByConfig <- verbSenseConfigs.traverse(vsConfig =>
        getPropBankVerbClusterModels(config, vsConfig).map(vsConfig -> _)
      ).map(_.toMap)
      evalSenseLabels <- config.propBankEvalLabels.get
      tunedThresholds <- Log.infoBranch("Evaluating and tuning on PropBank")(
        evaluatePropBankVerbClusters(config, verbModelsByConfig, evalSenseLabels)
      )
      fullSenseLabels <- config.propBankFullLabels.get
      _ <- chosenThresholds.foldMapM(thresholds =>
        Log.infoBranch("Printing debuggy stuff")(
          doPropBankClusterDebugging(config, fullSenseLabels, verbModelsByConfig, thresholds)
        )
      )
    } yield ()
  }

  def main: Opts[IO[ExitCode]] = {
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
      for {
        implicit0(logger: EphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.create()
        _ <- logger.info(s"Mode: $mode")
        _ <- logger.info(s"Specified verb sense config: $verbSenseConfigOpt")
        _ <- {
          if(isPropbank) runPropBankFrameInduction(Config(mode), verbSenseConfigOpt)
          else runQasrlFrameInduction(Config(mode), verbSenseConfigOpt)
        }
      } yield ExitCode.Success
    }
  }
}
