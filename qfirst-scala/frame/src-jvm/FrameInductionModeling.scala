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

import scala.collection.immutable.Map

import scala.util.Random

import breeze.linalg._
import scala.collection.immutable.Vector

import freelog._
import freelog.implicits._

object FrameInductionModeling {

  implicit val logLevel = LogLevel.Trace

  // implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  // def getQuestionClustersFromAggregateQAForVerb(
  //   verbInflectedForms: InflectedForms,
  //   instances: Map[String, Map[Int, QAPairs]],
  //   verbCollapsedQAOutputs: Map[(ClausalQ, ClausalQ), Double]
  // ): MergeTree[QuestionId] = {
  //   val qidsByStructure = instances.toList.foldMap { case (sid, verbs) =>
  //     verbs.toList.foldMap { case (verbIndex, qaPairs) =>
  //       ClauseResolution.getResolvedFramePairs(verbInflectedForms, qaPairs.keys.toList).foldMap {
  //         case (frame, slot) =>
  //           val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
  //           Map((clauseTemplate -> slot) -> Vector(QuestionId(VerbId(sid, verbIndex), frame, slot)))
  //       }
  //     }
  //   }
  //   val baseTrees = qidsByStructure.map { case (structure, qids) =>
  //     structure -> MergeTree.createBalancedTree(NonEmptyList.fromList(qids.toList).get)
  //   }
  //   val clauseTemplates = qidsByStructure.keySet
  //   val structureTree = Coindexing.getCoindexingTree(clauseTemplates, verbCollapsedQAOutputs)
  //   structureTree.flatMap(baseTrees)
  // }

  // def getQuestionClustersFromAggregateQA(
  //   instances: Instances.Qasrl,
  //   collapsedQAOutputs: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  // ): Map[InflectedForms, MergeTree[QuestionId]] = {
  //   instances.values.map { case (verbInflectedForms, verbInstances) =>
  //     verbInflectedForms -> getQuestionClustersFromAggregateQAForVerb(
  //       verbInflectedForms, verbInstances, collapsedQAOutputs(verbInflectedForms)
  //     )
  //   }
  // }

  // def getPropBankVerbClusterModels(
  //   config: Config, modelConfig: ModelConfig)(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[Map[String, PropBankVerbClusterModel]] = {
  //   config.cachePropBankVerbModelComputation(modelConfig) {
  //     for {
  //       instances <- config.propBankFullInstances.get
  //       elmoVecs <- config.propBankFullElmo.get
  //       verbClusters <- runVerbSenseAgglomerativeClustering(
  //         modelConfig = modelConfig.asInstanceOf[ModelConfig.NonJoint], // TODO
  //         instances = instances,
  //         elmoVecs = elmoVecs,
  //         renderVerbType = identity[String]
  //       )
  //       // TODO include QA outputs after indexing stuff has been fixed
  //       // collapsedQAOutputs <- config.propBankCollapsedQAOutputs.get
  //       verbClusterModels = verbClusters.map { case (verbLemma, clusterTree) =>
  //         val clauseSets = instances.values(verbLemma).iterator.flatMap { case (sid, verbMap) =>
  //           verbMap.iterator.map { case (vi, qas) =>
  //             val questions = qas.keySet.toList
  //             val clauseSet = ClauseResolution
  //               .getResolvedStructures(questions)
  //               .map(_._1).toSet
  //             VerbId(sid, vi) -> clauseSet
  //           }
  //         }.toMap
  //         verbLemma -> PropBankVerbClusterModel(
  //           verbLemma,
  //           clusterTree,
  //           clauseSets
  //           // collapsedQAOutputs(verbInflectedForms).toList
  //         )
  //       }
  //     } yield verbClusterModels
  //   }
  // }
  // // TODO restrict coindexing to clauses above minimum probability threshold?
  // def runCollapsedCoindexing(
  //   instances: Instances.Qasrl,
  //   verbSenseModel: VerbSenseTuningPoint,
  //   verbClusters: Map[InflectedForms, MergeTree[VerbId]],
  //   fuzzyArgEquivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  // ): Map[InflectedForms, VerbFrameset] = {
  //   verbClusters.transform { case (verbInflectedForms, tree) =>
  //     val numInstancesForVerb = tree.size
  //     val verbClauseSets = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
  //       verbMap.map { case (vi, qMap) =>
  //         val questions = qMap.keySet.toList
  //         val clauses = ClauseResolution.getResolvedFramePairs(
  //           verbInflectedForms, questions
  //         ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
  //         VerbId(sid, vi) -> clauses
  //       }
  //     }
  //     val clusterTrees = tree.splitWhile(t => (t.loss / numInstancesForVerb) > verbSenseModel.maxLoss)
  //     val verbRel = fuzzyArgEquivalences(verbInflectedForms)
  //     val verbFrames = clusterTrees.map { frameTree =>
  //       val verbIds = frameTree.values
  //       val clauseTemplates = verbIds.foldMap { vid =>
  //         verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
  //       }.toList.map { case (c, count) => FrameClause(c, (count.toDouble / verbIds.size)) }
  //       val frameProb = verbIds.size.toDouble / numInstancesForVerb
  //       val questionClusterTree = getQuestionClustersFromAggregateQAForVerb(
  //         verbInflectedForms, instances.values(verbInflectedForms), fuzzyArgEquivalences(verbInflectedForms)
  //       )
  //       // val coindexingTree = Coindexing.getCoindexingTree(clauseTemplates.map(_.args).toSet, verbRel)
  //       VerbFrame(verbIds.toSet, clauseTemplates, questionClusterTree, frameProb)
  //     }.toList
  //     VerbFrameset(verbInflectedForms, verbFrames)
  //   }
  // }
}
