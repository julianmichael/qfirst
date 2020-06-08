package qfirst.frame

import qfirst.frame.clustering._
import qfirst.frame.util.Duad
import qfirst.frame.util.FileCached
import qfirst.frame.util.NonMergingMap

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.OptionT
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

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import scala.annotation.tailrec

import freelog._
import freelog.implicits._

object FrameInductionApp extends CommandIOApp(
  name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
  header = "Induce verb frames.") {

  implicit val logLevel = LogLevel.Trace

  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(s"$splitName/$model"))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]](
            s"Argument cluster model: $model. Clustering data from $splitName")(
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, MergeTree[Set[ArgumentId[Arg]]])](path)
              .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getArgumentClusters(features)
          }
        )
    }
  }

  def getVerbClusterModels[VerbType: Encoder : Decoder, Arg: Encoder : Decoder](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    features.modelDir.map(modelDir =>
      FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]](
        s"QA-SRL cluster model: XXX")(
        path = modelDir.resolve(s"XXX.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType, Arg])](path)
          .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
        write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
        import ArgumentModel._
        val argumentModel = QuestionEntropy

        // val model = Joint(argumentModel)

        ???

        // val argumentModel = Composite.argument(
        //   QuestionEntropy -> 1.0,
        //   AnswerEntropy -> 1.0
        // )
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

        // Log.infoBranch("Initializing model features")(model.init(features)) >>
        //   features.verbArgSets.full.get >>= (
        //     _.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        //       val verbIds = NonEmptyVector.fromVector(verbs.value.keySet.toVector).get
        //       Log.trace(features.renderVerbType(verbType)) >> {
        //         for {
        //           argumentAlgorithm <- argumentModel.create(features, verbType)
        //           algorithm <- model.create(features, verbType)
        //           (verbClusterTree, finalParams) <- IO(algorithm.runFullAgglomerativeClustering(verbIds))
        //           argumentClusterTree = argumentAlgorithm.finishAgglomerativeClustering(finalParams)._1
        //         } yield verbType -> VerbClusterModel(
        //           verbType,
        //           verbClusterTree,
        //           argumentClusterTree
        //         )
        //       }
        //     }.map(_.toMap)
        //   )
      }
    )
  }

  def runQasrlFrameInduction(
    features: GoldQasrlFeatures)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      _ <- IO.unit
      // vocabs <- Log.infoBranch(s"Initializing question template vocabularies")(
      //   features.questionTemplateVocabsByVerb.get
      // )
      // _ <- Log.info(s"Total question templates: " + vocabs.unorderedFoldMap(_.items.toSet).size)
      // _ <- vocabs.toList.sortBy(-_._2.size).take(30).reverse.traverse { case (verb, vocab) =>
      //   Log.info(vocab.size + " " + verb.allForms.mkString(", "))
      // }
      // _ <- Log.info(s"Running frame induction on QA-SRL. Models: ${modelConfigs.mkString(", ")}")
      // verbModelsByConfig <- modelConfigs.traverse(vsConfig =>
      //   Log.infoBranch(s"Clustering for model: $vsConfig") {
      //     getVerbClusterModels[InflectedForms, ClausalQuestion](features, vsConfig, _.allForms.mkString(", ")) >>= (
      //       _.get.map(vsConfig -> _)
      //     )
      //   }
      // ).map(_.toMap)
      // goldParaphrases <- config.readGoldParaphrases
      // evaluationItems <- config.evaluationItems.get
      // tunedThresholds <- Log.infoBranch("Evaluating and tuning thresholds")(
      //   tuningFullEvaluation(config, verbModelsByConfig, goldParaphrases, evaluationItems)
      // )
    } yield ()
  }

  def runArgumentRoleInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      // _ <- Log.info(s"Running frame induction on PropBank with gold argument spans.")
      // _ <- Log.info(s"Assume gold verb sense? " + (if(features.assumeGoldVerbSense) "yes" else "no"))
      // _ <- Log.info(s"Model: $model")
      argTrees <- Log.infoBranch(s"Clustering arguments") {
        getArgumentClusters[VerbType, Arg](model, features).flatMap(_.get)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(s"$splitName/$model")).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        val argTreesRefined = argTrees.asInstanceOf[Map[String, MergeTree[Set[ArgumentId[Arg]]]]]
        features.argRoleLabels.get >>= (argRoleLabels =>
          if(features.mode.shouldEvaluate) {
            if(features.assumeGoldVerbSense) {
              Log.infoBranch("Evaluating argument clustering")(
                Evaluation.evaluateArgumentClusters(
                  evalDir, model.toString,
                  argTreesRefined, argRoleLabels, useSenseSpecificRoles = true
                )
              )
            } else {
              Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
                Evaluation.evaluateArgumentClusters(
                  evalDir.resolve("sense-specific"),
                  s"$model (sense-specific roles)",
                  argTreesRefined, argRoleLabels, useSenseSpecificRoles = true
                )
              ) >> Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
                Evaluation.evaluateArgumentClusters(
                  evalDir.resolve("sense-agnostic"),
                  s"$model (sense-agnostic roles)",
                  argTreesRefined, argRoleLabels, useSenseSpecificRoles = false
                )
              )
            }
          } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
        )
      }
    } yield ()
  }

  def runModeling[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = model match {
    case argModel @ ArgumentModel(_) => runArgumentRoleInduction(argModel, features)
  }

  sealed trait DataSetting {
    type VerbType; type Arg
    def getFeatures(
      mode: RunMode)(
      implicit Log: EphemeralTreeLogger[IO, String]
    ): Features[VerbType, Arg]
  }
  object DataSetting {
    case object Qasrl extends DataSetting {
      type VerbType = InflectedForms; type Arg = ClausalQuestion
      override def toString = "qasrl"
      override def getFeatures(
        mode: RunMode)(
        implicit Log: EphemeralTreeLogger[IO, String]
      ) = new GoldQasrlFeatures(mode)
    }
    case class Ontonotes5(assumeGoldVerbSense: Boolean) extends DataSetting {
      type VerbType = String; type Arg = ESpan
      override def toString = {
        val senseLemma = if(assumeGoldVerbSense) "sense" else "lemma"
        s"ontonotes-$senseLemma"
      }
      override def getFeatures(
        mode: RunMode)(
        implicit Log: EphemeralTreeLogger[IO, String]
      ) = new Ontonotes5GoldSpanFeatures(mode, assumeGoldVerbSense)
    }
    case class CoNLL08(assumeGoldVerbSense: Boolean) extends DataSetting {
      type VerbType = String; type Arg = Int
      override def toString = {
        val senseLemma = if(assumeGoldVerbSense) "sense" else "lemma"
        s"conll08-$senseLemma"
      }
      override def getFeatures(
        mode: RunMode)(
        implicit Log: EphemeralTreeLogger[IO, String]
      ) = new CoNLL08GoldDepFeatures(mode, assumeGoldVerbSense)
    }

    def all = List[DataSetting](
      Qasrl, Ontonotes5(false), Ontonotes5(true),
      CoNLL08(false), CoNLL08(true)
    )

    def fromString(x: String): Option[DataSetting] = {
      all.find(_.toString == x)
    }
  }

  val dataO = Opts.option[String](
    "data", metavar = DataSetting.all.mkString("|"), help = "Data setting to run in."
  ).mapValidated { setting =>
    DataSetting.fromString(setting).map(Validated.valid).getOrElse(
      Validated.invalidNel(s"Invalid data setting $setting: must be one of ${DataSetting.all.mkString(", ")}")
    )
  }

  val modeO = Opts.option[String](
    "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
  ).mapValidated { string =>
    RunMode.fromString(string).map(Validated.valid).getOrElse(
      Validated.invalidNel(s"Invalid mode $string: must be setup, sanity, dev, or test.")
    )
  }

  // TODO: get any model (verb or joint)
  val modelO = Opts.option[String](
    "model", metavar = "loss spec", help = "Argument clustering model configuration."
  ).mapValidated { string =>
    ArgumentModel.fromString(string)
      .map(Validated.valid)
      .getOrElse(Validated.invalidNel(s"Invalid model $string. Still working on parsing error reporting."))
  }

  val setup = Opts.subcommand(
    name = "setup",
    help = "Run feature setup.")(
    dataO.orNone.map { dataSettingOpt =>
      for {
        implicit0(logger: SequentialEphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.debounced()
        _ <- logger.info(s"Mode: setup")
        dataSettings = dataSettingOpt.fold(DataSetting.all)(List(_))
        _ <- logger.info(s"Data: " + dataSettings.mkString(", "))
        _ <- dataSettings.traverse(_.getFeatures(RunMode.Sanity).setup).as(ExitCode.Success)
      } yield ExitCode.Success
    }
  )

  val run = Opts.subcommand(
    name = "run",
    help = "Run clustering / frame induction.")(
    (dataO, modeO, modelO).mapN { (data, mode, model) =>
      for {
        implicit0(logger: SequentialEphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.debounced()
        _ <- logger.info(s"Mode: $mode")
        _ <- logger.info(s"Data: $data")
        _ <- logger.info(s"Model: $model")
        // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
        _ <- data match {
          case d @ DataSetting.Qasrl =>
            runModeling(model, d.getFeatures(mode))
          case d @ DataSetting.Ontonotes5(_) =>
            runModeling(model, d.getFeatures(mode))
          case d @ DataSetting.CoNLL08(_) =>
            runModeling(model, d.getFeatures(mode))
        }
      } yield ExitCode.Success
    }
  )

  def main: Opts[IO[ExitCode]] = setup.orElse(run)
}
