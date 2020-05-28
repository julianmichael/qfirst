package qfirst.frame

import qfirst.frame.models._
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

  import breeze.linalg.DenseVector
  import breeze.stats.distributions.Multinomial

  val numFlatClusters = 100 // TODO small for testing. make big.

  // soft EM hyperparams
  val flatClusteringSoftStoppingDelta = 1e-9
  val flatClusteringTempSched = (x: Int) => scala.math.pow(0.8, x)
  val flatClusteringPriorEstimatorDense = (counts: DenseVector[Double]) => {
    // smoothes with dirichlet, then inverts the odds.
    invertOdds(dirichletPosteriorFromDense(counts, 1000))
  }
  // hard EM hyperparams
  val flatClusteringHardStoppingDelta = 1e-10
  val flatClusteringPriorEstimatorSparse = (counts: Map[Int, Int], numClusters: Int) => {
    // smoothes with dirichlet, then inverts the odds.
    // invertOdds(dirichletPosteriorFromSparseNew(counts, numClusters, 1000))
    // always assume uniform prior --- seems to work just as well if not better
    Multinomial(DenseVector.ones[Double](numClusters))
  }

  def runCombinedClustering[I, FP, AP](
    indices: NonEmptyVector[I],
    flatAlgorithm: FlatClusteringAlgorithm { type Index = I; type ClusterParam = FP },
    agglomAlgorithm: AgglomerativeClusteringAlgorithm { type Index = I; type ClusterParam = AP })(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[(MergeTree[Set[I]], AP)] = {
    if(indices.size <= numFlatClusters) { // we can immediately do agglom. clustering
      IO {
        val (argClusterTree, params) = agglomAlgorithm.runFullAgglomerativeClustering(indices)
        argClusterTree.map(Set(_)) -> params
      }
    } else { // we need a flat pre-clustering step
      val indicesVec = indices.toVector
      for {
        _ <- Log.info(s"Pre-clustering ${indices.size} items.")
        allEMTrials <- (1 to 3).toList.traverse(i =>
          Log.traceBranch(s"Flat clustering trial $i")(
            for {
              initModel <- IO(flatAlgorithm.initPlusPlus(indicesVec, numFlatClusters))
              // softEMModel <- flatAlgorithm.runSoftEM(
              //   initModel, argIds,
              //   flatClusteringSoftStoppingDelta,
              //   flatClusteringTempSched,
              //   flatClusteringPriorEstimatorDense
              // ).map(_._1)
              res <- flatAlgorithm.runHardEM(
                initModel /*softEMModel*/, indicesVec,
                flatClusteringHardStoppingDelta,
                flatClusteringPriorEstimatorSparse
              )
            } yield res
          )
        )
        hardEMAssignments = allEMTrials.filterNot(_._3.isNaN).minBy(_._3)._2
        hardEMClusters = hardEMAssignments.zipWithIndex.groupBy(_._1).toVector.map {
          case (_, is) => is.map(i => indicesVec(i._2)).toSet
        }
        setClusteringAlg = new AgglomerativeSetClustering(agglomAlgorithm)
      } yield setClusteringAlg.runFullAgglomerativeClustering(
        NonEmptyVector.fromVector(hardEMClusters).get
      )
    }
  }

  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder](
    name: String, features: Features[VerbType, Arg],
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]] = {
    // TODO organize models into subdirs
    features.modelDir.map(modelDir =>
      FileCached[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]](
        s"Argument cluster model: $name")(
        path = modelDir.resolve(s"$name.jsonl.gz"),
        read = path => FileUtil.readJsonLines[(VerbType, MergeTree[Set[ArgumentId[Arg]]])](path)
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
              Log.info(renderVerbType(verbType)) >> {
                features.args.full.get.map(_.apply(verbType)) >>= { argIdSet =>
                  model.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
                    // some of them arg empty, gotta skip
                    NonEmptyVector.fromVector(argIdSet.toVector).traverse { nonEmptyArgIds =>
                      runCombinedClustering(nonEmptyArgIds, flatAlgorithm, agglomAlgorithm).map {
                        case (argSetClusterTree, finalParams) => verbType -> argSetClusterTree
                      }
                    }
                  }
                }
              }
            }.map(_.flatten.toMap)
          )
      }
    )
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
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
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
            Evaluation.evaluateArgumentClusters(modelName, features, argTrees, useSenseSpecificRoles = true)
          )
        } else {
          Log.infoBranch("Evaluating argument clustering (verb sense specific)")(
            Evaluation.evaluateArgumentClusters(s"$modelName-sense", features, argTrees, useSenseSpecificRoles = true)
          ) >> Log.infoBranch("Evaluating argument clustering (verb sense agnostic)")(
            Evaluation.evaluateArgumentClusters(s"$modelName-nosense", features, argTrees, useSenseSpecificRoles = false)
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
        implicit0(logger: SequentialEphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeFansiLogger.debounced()
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
