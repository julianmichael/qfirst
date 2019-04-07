package qfirst.paraphrase
import qfirst.paraphrase.browse._
import qfirst._
import qfirst.protocols.SimpleQAs
import qfirst.metrics._

import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank._

import qasrl._
import qasrl.data._
import qasrl.labeling.SlotBasedLabel

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import HasMetrics.ops._

import ClauseResolution.ArgStructure

object EvalApp extends IOApp {

  type ParaphraseAnnotations = Map[
    // sentence
    String, Map[
      // verb index
      Int, VerbParaphraseLabels
    ]
  ]

  type QABeam = List[SimpleQAs.BeamItem[SlotBasedLabel[VerbForm]]]

  val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)

  val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc
    )
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  def runEvaluation(
    evalSet: Dataset,
    evaluationItems: Set[(InflectedForms, String, Int)],
    predictions: Stream[IO, SentencePrediction[QABeam]],
    filter: SimpleQAs.Filter,
    frameInductionResults: FrameInductionResults,
    paraphraseAnnotations: ParaphraseAnnotations
  ): IO[Unit] = for {
    results <- predictions.map { predSentence =>
      val goldSentence = evalSet.sentences(predSentence.sentenceId)
      predSentence.verbs
        .filter(verb => evaluationItems.contains((verb.verbInflectedForms, predSentence.sentenceId, verb.verbIndex)))
        .flatMap { verb =>
          paraphraseAnnotations.get(predSentence.sentenceId)
            .flatMap(_.get(verb.verbIndex))
          .map(verb -> _)
        }.foldMap { case (verb, goldParaphrases) =>
        val goldVerb = goldSentence.verbEntries(verb.verbIndex)
        val predictedQAs = protocol.filterBeam(filter, verb)
        val verbFrameset = frameInductionResults.frames(verb.verbInflectedForms)
        val frameProbabilities = frameInductionResults
          .assignments(verb.verbInflectedForms)(predSentence.sentenceId)(verb.verbIndex)
        Evaluation.getVerbResults(goldVerb, predictedQAs, goldParaphrases, verbFrameset, frameProbabilities, 0.3, false) // TODO take thresholds as input
      }
    }.compile.foldMonoid
    _ <- IO(println(getMetricsString(results)))
  } yield ()

  def getEvaluationItems(evalSet: Dataset, evaluationItemsPath: NIOPath) = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    if(Files.exists(evaluationItemsPath)) FileUtil.readJsonLines[(InflectedForms, String, Int)](evaluationItemsPath).compile.toList
    else logOp(
      s"Creating new sample for evaluation at $evaluationItemsPath", {
        val rand = new scala.util.Random(86735932569L)
        val allItems = rand.shuffle(
          evalSet.sentences.values.iterator.flatMap(sentence =>
            sentence.verbEntries.values.toList.map(verb =>
              (verb.verbInflectedForms, sentence.sentenceId, verb.verbIndex)
            )
          ).take(1000).toVector
        )
        FileUtil.writeJsonLines(evaluationItemsPath, io.circe.Printer.noSpaces)(allItems).as(allItems)
      }
    )
  }

  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))

  def program(
    qasrlBankPath: NIOPath,
    predDir: NIOPath,
    relativeFramesDir: String,
    testOnTest: Boolean,
    launchBrowser: Boolean
  ): IO[ExitCode] = {
    val evalSetName = if(testOnTest) "test" else "dev"
    val evalSetFilename = s"$evalSetName.jsonl.gz"
    val evalSetPath = qasrlBankPath.resolve(s"dense/$evalSetFilename")

    val predFilename = if(testOnTest) predDir.resolve("predictions-test.jsonl") else predDir.resolve("predictions.jsonl")
    val paraphraseGoldPath = predDir.resolve("gold-paraphrases.json")

    val outDir = predDir.resolve(relativeFramesDir)

    val resultsFilename = if(testOnTest) "results-test.json" else "results.json"
    val resultsPath = outDir.resolve(resultsFilename)

    val evaluationItemsPath = predDir.resolve(s"eval-sample-$evalSetName.jsonl")

    for {
      evalSet <- logOp(s"Reading $evalSetName set", qasrl.bank.Data.readDataset(evalSetPath))
      evaluationItems <- getEvaluationItems(evalSet, evaluationItemsPath)
      filter <- {
        import io.circe.generic.auto._
        FileUtil.readJson[SimpleQAs.Filter](predDir.resolve("filter.json"))
      }
      predictions = {
        import qasrl.data.JsonCodecs._
        import io.circe.generic.auto._
        FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename)
      }
      paraphraseGold <- {
        if(!Files.exists(paraphraseGoldPath)) {
          IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
            IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
        } else FileUtil.readJson[EvalApp.ParaphraseAnnotations](paraphraseGoldPath)
      }
      frameInductionResults <- logOp(
        "Loading frames",
        FileUtil.readJson[FrameInductionResults](resultsPath)
      )
      _ <- runEvaluation(
        evalSet, evaluationItems.toSet, predictions, filter, frameInductionResults, paraphraseGold
      )
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val goldPath = Opts.option[NIOPath](
      "qasrl-gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val predPath = Opts.option[NIOPath](
      "qasrl-pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val relativeFramesDir = Opts.option[String](
      "out", metavar = "path", help = "Relative path to the directory with frame induction results."
    )
    val testOnTest = Opts.flag(
      "test", help = "Evaluate on the test set instead of dev."
    ).orFalse

    val launchBrowser = Opts.flag(
      "browse", help = "Whether to launch the web server for browsing/annotating the results."
    ).orFalse

    (goldPath, predPath, relativeFramesDir, testOnTest, launchBrowser).mapN(program)
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
