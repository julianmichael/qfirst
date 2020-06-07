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

  val allModelConfigs = {
    List(ModelConfig.SingleCluster, ModelConfig.EntropyOnly, ModelConfig.ELMoOnly) ++
      List(ModelConfig.Interpolated(0.5))
    // (1 to 9).map(_.toDouble / 10.0).toList.map(ModelConfig.Interpolated(_))
  }

  // TODO organize models into subdirs and cache
  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    modelName: String, features: Features[VerbType, Arg],
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(s"$splitName/$modelName"))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]](
            s"Argument cluster model: $modelName. Clustering data from $splitName")(
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, MergeTree[Set[ArgumentId[Arg]]])](path)
              .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            import ClusteringModel._
            val model = ArgMLMEntropy("masked")
            // val model = QuestionEntropy
            // val model = Composite.argument(
            //   QuestionEntropy -> 1.0,
            //   AnswerEntropy -> 1.0
            // )
            for {
              _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
              _ <- model.init(features)
              allVerbArgSets <- features.verbArgSets.get
              allArgs <- features.args.get
              results <- allVerbArgSets.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
                Log.info(renderVerbType(verbType)) >> {
                  // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
                  NonEmptyVector.fromVector(allArgs(verbType).toVector).traverse { args =>
                    model.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
                      val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
                      Clustering.runCombinedClustering(args, flatAlgorithm, agglomAlgorithm).map {
                        case (argTree, _) => verbType -> argTree
                      }
                    }
                  }
                }
              }
            } yield results.flatten.toMap
          }
        )
    }
  }

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
        //       Log.trace(renderVerbType(verbType)) >> {
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
    features: Ontonotes5GoldSpanFeatures, modelOpt: Option[ModelConfig])(
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
      // _ <- Log.infoBranch("Running feature setup.")(features.setup)
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

  def runPropBankArgumentRoleInduction[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val modelName = "test"
    for {
      _ <- Log.info(s"Running frame induction on PropBank with gold argument spans.")
      _ <- Log.info(s"Assume gold verb sense? " + (if(features.assumeGoldVerbSense) "yes" else "no"))
      // _ <- Log.infoBranch("Running feature setup.")(features.setup)
      _ <- Log.info(s"Model name: $modelName")
      argTrees <- Log.infoBranch(s"Clustering arguments") {
        getArgumentClusters[String, Arg](modelName, features, identity[String]).flatMap(_.get)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(s"$splitName/$modelName")).flatTap(createDir)
      argRoleLabels <- features.argRoleLabels.get
      _ <- if(features.mode.shouldEvaluate) {
        if(features.assumeGoldVerbSense) {
          Log.infoBranch("Evaluating argument clustering")(
            Evaluation.evaluateArgumentClusters(
              evalDir, modelName,
              argTrees, argRoleLabels, useSenseSpecificRoles = true
            )
          )
        } else {
          Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
            Evaluation.evaluateArgumentClusters(
              evalDir.resolve("sense-specific"),
              s"$modelName (sense-specific roles)",
              argTrees, argRoleLabels, useSenseSpecificRoles = true
            )
          ) >> Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
            Evaluation.evaluateArgumentClusters(
              evalDir.resolve("sense-agnostic"),
              s"$modelName (sense-agnostic roles)",
              argTrees, argRoleLabels, useSenseSpecificRoles = false
            )
          )
        }
      } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
    } yield ()
  }

  def main: Opts[IO[ExitCode]] = {
    val dataSettings = List(
      "qasrl",
      "ontonotes-lemma-args", "ontonotes-sense-args",
      "conll08-lemma-args", "conll08-sense-args"
    )

    val dataO = Opts.option[String](
      "data", metavar = dataSettings.mkString("|"), help = "Data setting to run in."
    )

    val modeOptO = Opts.option[String](
      "mode", metavar = "setup|sanity|dev|test", help = "Which mode to run in."
    ).mapValidated { string =>
      RunMode.fromString(string)
        .map(mode => Validated.valid(Option(mode)))
        .orElse(if(string == "setup") Some(Validated.valid(None)) else None)
        .getOrElse(Validated.invalidNel(s"Invalid mode $string: must be setup, sanity, dev, or test."))
    }

    val modelConfigOptO = Opts.option[String](
      "model", metavar = "entropy|elmo|<float>", help = "Verb sense model configuration."
    ).mapValidated { string =>
      ModelConfig.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid model $string: must be entropy, elmo, or a float (interpolation param)."))
    }.orNone

    (dataO, modeOptO, modelConfigOptO).mapN { (data, modeOpt, modelConfigOpt) =>
      val mode = modeOpt.getOrElse(RunMode.Sanity)
      val setup = modeOpt.isEmpty
      val modeString = modeOpt.fold("setup")(_.toString)
      for {
        implicit0(logger: SequentialEphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.debounced()
        _ <- logger.info(s"Data: $data")
        _ <- logger.info(s"Mode: $modeString")
        _ <- (if(setup) IO.unit else logger.info(s"Model configuration: $modelConfigOpt"))
        _ <- data match {
          case "qasrl" =>
            val feats = new GoldQasrlFeatures(mode)
            if(setup) feats.setup else runQasrlFrameInduction(feats, modelConfigOpt)
          case "ontonotes-sense-args" => // assume gold verb sense, only cluster/evaluate arguments
            val feats = new Ontonotes5GoldSpanFeatures(mode, assumeGoldVerbSense = true)
            if(setup) feats.setup else runPropBankArgumentRoleInduction(feats)
          case "ontonotes-lemma-args" => // don't assume gold verb sense, only cluster arguments
            val feats = new Ontonotes5GoldSpanFeatures(mode, assumeGoldVerbSense = false)
            if(setup) feats.setup else runPropBankArgumentRoleInduction(feats)
          case "conll08-sense-args" => // assume gold verb sense, only cluster/evaluate arguments
            val feats = new CoNLL08GoldDepFeatures(mode, assumeGoldVerbSense = true)
            if(setup) feats.setup else runPropBankArgumentRoleInduction(feats)
          case "conll08-lemma-args" => // don't assume gold verb sense, only cluster arguments
            val feats = new CoNLL08GoldDepFeatures(mode, assumeGoldVerbSense = false)
            if(setup) feats.setup else runPropBankArgumentRoleInduction(feats)
          case _ => throw new IllegalArgumentException(
            "--data must be one of the following: " + dataSettings.mkString(", ")
          )
        }
      } yield ExitCode.Success
    }
  }
}
