package qfirst.frame

import qfirst.frame.clustering._
import qfirst.frame.eval._
import qfirst.frame.features._
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

  var shouldRecomputeModel: Boolean = false

  def maybeGetFromCache[A](fc: FileCached[A])(implicit Log: EphemeralTreeLogger[IO, String]): IO[A] = {
    if(shouldRecomputeModel) fc.compute
    else fc.get
  }

  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
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

  def runArgumentRoleInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      argTrees <- Log.infoBranch(s"Getting argument clusters") {
        getArgumentClusters[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        val argTreesRefined = argTrees.asInstanceOf[Map[String, MergeTree[Set[ArgumentId[Arg]]]]]
        features.argRoleLabels.get >>= (argRoleLabels =>
          if(features.mode.shouldEvaluate) {
            Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
              Evaluation.evaluateArgumentClusters(
                evalDir.resolve("sense-specific"),
                s"$model (sense-specific roles)",
                argTreesRefined, argRoleLabels,
                tuningSpecs,
                useSenseSpecificRoles = true
              )
            ) >> IO.pure(features.assumeGoldVerbSense).ifM(
              IO.unit, Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
                Evaluation.evaluateArgumentClusters(
                  evalDir.resolve("sense-agnostic"),
                  s"$model (sense-agnostic roles)",
                  argTreesRefined, argRoleLabels,
                  tuningSpecs,
                  useSenseSpecificRoles = false
                )
              )
            )
          } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
        )
      }
    } yield ()
  }

  def getVerbClusters[VerbType: Encoder : Decoder, Arg](
    model: VerbModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[Set[VerbId]]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, MergeTree[Set[VerbId]]]](
            s"Verb cluster model: $model. Clustering data from $splitName")(
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, MergeTree[Set[VerbId]])](path)
              .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getVerbClusters(features)
          }
        )
    }
  }

  def runVerbSenseInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: VerbModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      verbTrees <- Log.infoBranch(s"Getting verb clusters") {
        getVerbClusters[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        features.verbSenseLabels.get >>= { verbSenseLabels =>
          val verbTreesRefined = verbTrees.asInstanceOf[Map[String, MergeTree[Set[VerbId]]]]
          if(features.mode.shouldEvaluate) {
            if(features.assumeGoldVerbSense) Log.info(s"Skipping verb sense evaluation since gold senses are assumed") else {
              Log.infoBranch("Evaluating verb clustering")(
                Evaluation.evaluateClusters(
                  evalDir, model.toString,
                  verbTreesRefined, verbSenseLabels,
                  tuningSpecs
                )
              )
            }
          } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
        }
      }
    } yield ()
  }

  def getVerbFrames[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: JointModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]](
            s"Joint model: $model. Clustering data from $splitName")(
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType, Arg])](path)
              .infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getJointClusters(features)
          }
        )
    }
  }

  def runJointFrameInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: JointModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      verbClusterModels <- Log.infoBranch(s"Getting verb clusters") {
        getVerbFrames[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        val verbClusterModelsRefined = verbClusterModels.asInstanceOf[Map[String, VerbClusterModel[String, Arg]]]
        if(!features.mode.shouldEvaluate) Log.info(s"Skipping evaluation for run mode ${features.mode}") else {
          features.argRoleLabels.get.flatMap((argRoleLabels: Map[String,NonMergingMap[ArgumentId[Arg],PropBankRoleLabel]]) =>
            Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
              Evaluation.evaluateArgumentClusters[String, Arg](
                evalDir.resolve("sense-specific"),
                s"$model (sense-specific roles)",
                // verbClusterModelsRefined.mapVals(_.argumentClusterTree.map(Set(_))),
                verbClusterModelsRefined.flatMap { case (vt, model) =>
                  model.argumentClusterTreeOpt.map(_.map(Set(_))).map(vt -> _)
                },
                argRoleLabels,
                tuningSpecs,
                useSenseSpecificRoles = true
              )
            ) >> IO.pure(features.assumeGoldVerbSense).ifM(
              IO.unit, Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
                Evaluation.evaluateArgumentClusters(
                  evalDir.resolve("sense-agnostic"),
                  s"$model (sense-agnostic roles)",
                  // verbClusterModelsRefined.mapVals(_.argumentClusterTree.map(Set(_))),
                  verbClusterModelsRefined.flatMap { case (vt, model) =>
                    model.argumentClusterTreeOpt.map(_.map(Set(_))).map(vt -> _)
                  },
                  argRoleLabels,
                  tuningSpecs,
                  useSenseSpecificRoles = false
                )
              )
            )
          ) >> (
            if(features.assumeGoldVerbSense) Log.info(s"Skipping verb sense evaluation since gold senses are assumed") else {
              features.verbSenseLabels.get >>= { (verbSenseLabels: String => VerbId => String) =>
                Log.infoBranch("Evaluating verb clustering")(
                  Evaluation.evaluateClusters[String, VerbId, String](
                    evalDir, model.toString,
                    verbClusterModelsRefined.mapVals(_.verbClusterTree), verbSenseLabels,
                    tuningSpecs
                  )
                )
              }
            }
          )
        }
      }
    } yield ()
  }

  def runModeling[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ClusteringModel,
    features: Features[VerbType, Arg],
    tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = model match {
    // case argBaselineModel @ ArgumentBaselineModel(_) =>
    //   runBaselineArgumentRoleInduction(argBaselineModel, features)
    case argModel: ArgumentModel =>
      runArgumentRoleInduction(argModel, features, tuningSpecs)
    case verbModel: VerbModel =>
      runVerbSenseInduction(verbModel, features, tuningSpecs)
    case jointModel: JointModel =>
      runJointFrameInduction(jointModel, features, tuningSpecs)
  }

  def runSummarize[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    modelDir <- features.modelDir
    _ <- fileExists(modelDir.resolve("arg")).ifM(
      getSubdirs(modelDir.resolve("arg")) >>= ((modelDirs: List[NIOPath]) =>
        modelDirs.infoBarFoldMapM("Reading models and model info") { (modelSubdir: NIOPath) =>
          val model = modelSubdir.getFileName.toString
          for {
            // reading manually to avoid having to construct the model, allowing for comparison with old models
            // after changing code, etc.
            // argTree <- getArgumentClusters(model, features).flatMap(_.read).flatMap(x => IO(x.get))
            argTrees <- FileUtil.readJsonLines[(String, MergeTree[Set[ArgumentId[Arg]]])](
              modelSubdir.resolve("model.jsonl.gz")
            ).infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap)
            evalModeSubdirs <- getSubdirs(modelSubdir)
            evalModeResults <- evalModeSubdirs.foldMapM { (evalDir: NIOPath) =>
              getSubdirs(evalDir).flatMap((metricDirs: List[NIOPath]) =>
                metricDirs.foldMapM { (metricDir: NIOPath) =>
                  for {
                    metric <- IO(ClusterPRMetric.fromString(metricDir.getFileName.toString).get)
                    tuningSpecStr <- FileUtil.readString(metricDir.resolve("best-setting.txt"))
                    tuningSpec <- IO(SplitTuningSpec.fromString(tuningSpecStr).get).map(spec =>
                      if(!features.mode.isTest) spec.copy(thresholdsOverride = None) else spec
                    )
                  } yield Map(evalDir.getFileName.toString -> Map(metric -> NonMergingMap(model.toString -> tuningSpec)))
                }
              )
            }
          } yield (NonMergingMap(model.toString -> argTrees), evalModeResults)
        }
      ) >>= {
        case (models, argModelSpecs) =>
          val goldVerbSenseLabel = if(features.assumeGoldVerbSense) "by-sense" else "by-lemma"
          features.argRoleLabels.get.flatMap(argRoleLabels =>
            (features.outDir, features.splitName).mapN((out, split) => out.resolve(s"eval/$split/$goldVerbSenseLabel")) >>= (parentResultsDir =>
              argModelSpecs.toList.traverse { case (evalMode, metricSpecs) =>
                val useSenseSpecificRoles = evalMode == "sense-specific"
                val resultsDir = parentResultsDir.resolve(evalMode)
                metricSpecs.toList.infoBarTraverse(s"Recording stats ($evalMode)") { case (metric, modelSpecs) =>
                  val metricDir = resultsDir.resolve(metric.name).resolve("arg")
                  Log.info(s"Metric: $metric") >> createDir(metricDir) >> Evaluation.evaluateArgumentModels(
                    metricDir, metric,
                    models.value.zipValues(modelSpecs.value),
                    argRoleLabels,
                    useSenseSpecificRoles = evalMode == "sense-specific",
                    includeOracle = !features.mode.isTest
                  )
                }
              }
            )
          ).void
      }, IO.unit
    )
    _ <- fileExists(modelDir.resolve("verb")).ifM(
      getSubdirs(modelDir.resolve("verb")) >>= ((modelDirs: List[NIOPath]) =>
        modelDirs.infoBarFoldMapM("Reading models and model info") { (modelSubdir: NIOPath) =>
          val model = modelSubdir.getFileName.toString
          for {
            // reading manually to avoid having to construct the model, allowing for comparison with old models
            // after changing code, etc.
            // argTree <- getArgumentClusters(model, features).flatMap(_.read).flatMap(x => IO(x.get))
            argTrees <- FileUtil.readJsonLines[(String, MergeTree[Set[VerbId]])](
              modelSubdir.resolve("model.jsonl.gz")
            ).infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap)
            results <- getSubdirs(modelSubdir).flatMap((metricDirs: List[NIOPath]) =>
              metricDirs.foldMapM { (metricDir: NIOPath) =>
                for {
                  _ <- IO(System.err.println(metricDir))
                  metric <- IO(ClusterPRMetric.fromString(metricDir.getFileName.toString).get)
                  tuningSpecStr <- FileUtil.readString(metricDir.resolve("best-setting.txt"))
                  tuningSpec <- IO(SplitTuningSpec.fromString(tuningSpecStr).get).map(spec =>
                    if(!features.mode.isTest) spec.copy(thresholdsOverride = None) else spec
                  )
                } yield Map(metric -> NonMergingMap(model.toString -> tuningSpec))
              }
            )
          } yield (NonMergingMap(model.toString -> argTrees), results)
        }
      ) >>= {
        case (models, metricSpecs) =>
          val goldVerbSenseLabel = if(features.assumeGoldVerbSense) "by-sense" else "by-lemma"
          // System.err.println(models)
          // System.err.println(modelSpecs)
          features.verbSenseLabels.get.flatMap(verbSenseLabels =>
            (features.outDir, features.splitName)
              .mapN((out, split) => out.resolve(s"eval/$split/$goldVerbSenseLabel")) >>= { parentResultsDir =>
              // val useSenseSpecificRoles = evalMode == "sense-specific"
              val resultsDir = parentResultsDir.resolve("verb")
              metricSpecs.toList.infoBarTraverse(s"Recording stats (verb)") { case (metric, modelSpecs) =>
                val metricDir = resultsDir.resolve(metric.name)
                Log.info(s"Metric: $metric") >> createDir(metricDir) >> Evaluation.evaluateVerbModels(
                  metricDir, metric,
                  models.value.zipValues(modelSpecs.value),
                  verbSenseLabels,
                  includeOracle = !features.mode.isTest
                )
              }
            }
          ).void
      }, IO.unit
    )
  } yield ()

  def getFeatures(
    setting: DataSetting, mode: RunMode)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): Features[setting.VerbType, setting.Arg] = setting match {
    case DataSetting.Qasrl => new GoldQasrlFeatures(mode)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
    case DataSetting.Ontonotes5(assumeGoldVerbSense) => new OntoNotes5Features(mode, assumeGoldVerbSense)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
    case DataSetting.CoNLL08(assumeGoldVerbSense) => new CoNLL08Features(mode, assumeGoldVerbSense)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
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
      Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test.")
    )
  }

  val modelO = Opts.option[String](
    "model", metavar = "loss spec", help = "Clustering model configuration."
  ).mapValidated { string =>
    ClusteringModel.fromString(string)
      .map(Validated.valid)
      .getOrElse(Validated.invalidNel(s"Invalid model $string. (todo: better error reporting)"))
  }

  val defaultTuningSpecs = NonEmptyList.of(
    OracleCriterion,
    NumClustersCriterion,
    TotalEntropyCriterion
  ).map(SplitTuningSpec(_))

  val tuningO = Opts.options[String](
    "tune", help = "tuning spec, e.g., num-clusters=23"
  ).mapValidated(
    _.traverse(arg =>
      SplitTuningSpec.fromString(arg).map(Validated.valid).getOrElse(
        Validated.invalidNel(s"Invalid tuning spec $arg. (todo: better error reporting)")
      )
    )
  ).orNone.map(_.getOrElse(defaultTuningSpecs))

  val recomputeO = Opts.flag(
    "recompute", help = "recompute clustering model even if it is cached."
  ).orFalse

  def withLogger[A](run: SequentialEphemeralTreeLogger[IO, String] => IO[A]): IO[A] = {
    freelog.loggers.TimingEphemeralTreeFansiLogger.debounced() >>= { Log =>
      loggerUnsafe = Log
      val res = run(Log)
      res.handleErrorWith { e =>
        import java.io.{PrintWriter,StringWriter}
        val sw = new StringWriter
        val pw = new PrintWriter(sw)
        e.printStackTrace(pw)
        Log.error(sw.toString) >> Log.flush >> IO.raiseError[A](e)
      }
    }
  }

  val setup = Opts.subcommand(
    name = "setup",
    help = "Run feature setup.")(
    dataO.orNone.map { dataSettingOpt =>
      withLogger { logger =>
        implicit val Log = logger
        for {
          _ <- Log.info(s"Mode: setup")
          dataSettings = dataSettingOpt.fold(DataSetting.all)(List(_))
          _ <- Log.info(s"Data: " + dataSettings.mkString(", "))
          _ <- dataSettings.traverse(d => getFeatures(d, RunMode.Sanity).setup)
        } yield ExitCode.Success
      }
    }
  )

  val run = Opts.subcommand(
    name = "run",
    help = "Run clustering / frame induction.")(
    (dataO, modeO, modelO, tuningO, recomputeO).mapN { (data, mode, model, tuning, recompute) =>
      withLogger { logger =>
        implicit val Log = logger
        for {
          _ <- Log.info(s"Mode: $mode")
          _ <- Log.info(s"Data: $data")
          _ <- Log.info(s"Model: $model")
          _ <- IO(shouldRecomputeModel = recompute)
          // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
          _ <- data match {
            case d @ DataSetting.Qasrl =>
              runModeling(model, getFeatures(d, mode), tuning)
            case d @ DataSetting.Ontonotes5(_) =>
              runModeling(model, getFeatures(d, mode), tuning)
            case d @ DataSetting.CoNLL08(_) =>
              runModeling(model, getFeatures(d, mode), tuning)
          }
        } yield ExitCode.Success
      }
    }
  )

  val summarize = Opts.subcommand(
    name = "summarize",
    help = "Aggregate and analyze results from all models.")(
    (dataO, modeO).mapN { (data, mode) =>
      withLogger { logger =>
        implicit val Log = logger
        for {
          _ <- Log.info(s"Mode: $mode")
          _ <- Log.info(s"Data: $data")
          // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
          _ <- data match {
            case d @ DataSetting.Qasrl =>
              IO.raiseError(new IllegalArgumentException("Cannot evaluate on QA-SRL."))
            case d @ DataSetting.Ontonotes5(_) =>
              runSummarize(getFeatures(d, mode).getIfPropBank.get)
            case d @ DataSetting.CoNLL08(_) =>
              runSummarize(getFeatures(d, mode).getIfPropBank.get)
          }
        } yield ExitCode.Success
      }
    }
  )

  def main: Opts[IO[ExitCode]] =
    setup
      .orElse(run)
      .orElse(summarize)
}
