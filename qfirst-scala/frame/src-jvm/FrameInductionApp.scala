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

object FrameInductionApp extends CommandIOApp(
  name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
  header = "Induce verb frames.") {

  implicit val logLevel = LogLevel.Trace

  val allModelConfigs = {
    List(ModelConfig.SingleCluster, ModelConfig.EntropyOnly, ModelConfig.ELMoOnly) ++
      List(ModelConfig.Interpolated(0.5))
    // (1 to 9).map(_.toDouble / 10.0).toList.map(ModelConfig.Interpolated(_))
  }

  def getVerbClusterModels[VerbType: Encoder : Decoder](
    features: Features[VerbType], modelConfig: ModelConfig,
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType]]]] = {
    features.modelDir.map (modelDir =>
      FileCached[Map[VerbType, VerbClusterModel[VerbType]]](
        s"QA-SRL cluster model: $modelConfig")(
        path = modelDir.resolve(s"$modelConfig.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType])](path)
          .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
        write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
        features.verbIdsByType.full.get >>= (
          _.toList.infoBarTraverse("Clustering verbs") { case (verbType, _verbIds) =>
          // hack to make it possible to even do the clustering on the really common words. need to generalize later
          val verbIds = NonEmptyVector.fromVector(_verbIds.toVector.take(50)).get
          Log.trace(renderVerbType(verbType)) >> {
            modelConfig match {
              case ModelConfig.Joint =>
                val questionModel = Composite(
                  Composite(
                    QuestionEntropy -> 1.0,
                    AnswerEntropy -> 2.0
                  ) -> 1.0,
                  AnswerNLL -> 2.0
                )
                val model = Composite(
                  Composite(
                    VerbClauseEntropy -> 2.0,
                    VerbSqDist -> (1.0 / 175),
                  ) -> 1.0,
                  Joint(questionModel) -> 1.0
                )
                for {
                  questionAlgorithm <- questionModel.create(features, verbType)
                  algorithm <- model.create(features, verbType)
                  (verbClusterTree, finalParams) = algorithm.runFullAgglomerativeClustering(verbIds)
                  questionClusterTree = questionAlgorithm.finishAgglomerativeClustering(finalParams._2)._1
                } yield verbType -> VerbClusterModel(
                  verbType,
                  verbClusterTree,
                  questionClusterTree
                )
              case _ => ???
            }
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
    // TODO read in tuned thresholds (in case of test)
    for {
      _ <- Log.info(s"Running frame induction on QA-SRL. Models: ${modelConfigs.mkString(", ")}")
      verbModelsByConfig <- modelConfigs.traverse(vsConfig =>
        Log.infoBranch(s"Clustering for model: $vsConfig") {
          getVerbClusterModels[InflectedForms](features, vsConfig, _.allForms.mkString(", ")) >>= (
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

  // def runPropBankFrameInduction(
  //   features: PropBankFeatures, modelOpt: Option[ModelConfig])(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[Unit] = {
  //   val modelConfigs = modelOpt.map(List(_)).getOrElse(allModelConfigs)
  //   // TODO read in tuned threshold (in case of test)
  //   val chosenThresholds = Option(
  //     Map[ModelConfig, Double](
  //       ModelConfig.SingleCluster -> 0.0,
  //       ModelConfig.EntropyOnly -> 1.115,
  //       ModelConfig.ELMoOnly -> 0.718,
  //       ModelConfig.Interpolated(0.5) -> 1.105
  //     )
  //   )
  //   for {
  //     verbModelsByConfig <- modelConfigs.traverse(vsConfig =>
  //       getPropBankVerbClusterModels(config, vsConfig).map(vsConfig -> _)
  //     ).map(_.toMap)
  //     evalSenseLabels <- config.propBankEvalLabels.get
  //     tunedThresholds <- Log.infoBranch("Evaluating and tuning on PropBank")(
  //       evaluatePropBankVerbClusters(config, verbModelsByConfig, evalSenseLabels)
  //     )
  //     fullSenseLabels <- config.propBankFullLabels.get
  //     _ <- chosenThresholds.foldMapM(thresholds =>
  //       Log.infoBranch("Printing debuggy stuff")(
  //         doPropBankClusterDebugging(config, fullSenseLabels, verbModelsByConfig, thresholds)
  //       )
  //     )
  //   } yield ()
  // }

  def main: Opts[IO[ExitCode]] = {
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

    val isPropbankO = Opts.flag(
      "propbank", help = "run the PropBank verb sense evaluation."
    ).orFalse


    (modeO, modelConfigOptO, isPropbankO).mapN { (mode, modelConfigOpt, isPropbank) =>
      for {
        implicit0(logger: EphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.create()
        _ <- logger.info(s"Mode: $mode")
        _ <- logger.info(s"Specified verb sense config: $modelConfigOpt")
        _ <- {
          if(isPropbank) ??? // runPropBankFrameInduction(new PropbankFeatures(mode), modelConfigOpt)
          else runQasrlFrameInduction(new GoldQasrlFeatures(mode), modelConfigOpt)
        }
      } yield ExitCode.Success
    }
  }
}
