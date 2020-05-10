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

  def getVerbClusterModels[VerbType: Encoder : Decoder, Arg: Encoder : Decoder](
    features: Features[VerbType, Arg], modelConfig: ModelConfig,
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    features.modelDir.map (modelDir =>
      FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]](
        s"QA-SRL cluster model: $modelConfig")(
        path = modelDir.resolve(s"$modelConfig.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType, Arg])](path)
          .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
        write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
        import ClusteringModel._
        val argumentModel = QuestionEntropy
        val model = Joint(argumentModel)
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
            _.toList.infoBarTraverse("Clustering verbs") { case (verbType, _verbs) =>
              // hack to make it possible to even do the clustering on the really common words. need to generalize later
              val verbIds = NonEmptyVector.fromVector(_verbs.value.keySet.toVector.take(50)).get
              Log.trace(renderVerbType(verbType)) >> {
                for {
                  argumentAlgorithm <- argumentModel.create(features, verbType)
                  algorithm <- model.create(features, verbType)
                  (verbClusterTree, finalParams) = algorithm.runFullAgglomerativeClustering(verbIds)
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
      _ <- Log.infoBranch("Running setup steps.")(
        features.writeQGInputs
      )
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
          case "qasrl" => runQasrlFrameInduction(new GoldQasrlFeatures(mode), modelConfigOpt)
          case "propbank-span" => runPropBankGoldSpanFrameInduction(new PropBankGoldSpanFeatures(mode, assumeGoldVerbSense = false), modelConfigOpt)
          case _ => ???
        }
      } yield ExitCode.Success
    }
  }
}
