package qfirst.paraphrase
import qfirst._
import qfirst.browse._
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

@JsonCodec case class QuestionParaphraseLabels(
  correct: Set[String],
  incorrect: Set[String],
)
object ParaphraseLabels

@JsonCodec case class VerbParaphraseLabels(
  correctClauses: Set[ArgStructure],
  incorrectClauses: Set[ArgStructure],
  questionParaphrases: Map[String, QuestionParaphraseLabels]
)
object VerbParaphraseLabels {
  def empty = VerbParaphraseLabels(Set(), Set(), Map())
}

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

  // import qfirst.{Instances => I}
  // import qfirst.metrics.{Transformers => M}
  import qfirst.metrics._
  import shapeless._
  import shapeless.syntax.singleton._
  import shapeless.record._
  import monocle.function.{all => Optics}

  def getVerbResults(
    gold: VerbEntry,
    predictedQAs: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])],
    goldParaphrases: VerbParaphraseLabels,
    verbFrameset: VerbFrameset,
    frameProbabilities: Vector[Double],
  ) = {
    val (goldInvalid, goldValid) = filterGoldDense(gold)
    val questionBoundedAcc = predictedQAs.values.toList.map(_._1).foldMap { predQuestion =>
      val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
      if(goldInvalid.contains(predQString)) BoundedAcc.incorrect(predQuestion)
      else if(goldValid.contains(predQString)) BoundedAcc.correct(predQuestion)
      else BoundedAcc.uncertain(predQuestion)
    }
    val predictedParaphrases = verbFrameset.getParaphrases(
      frameProbabilities, questionBoundedAcc.correct.toSet
    )
    val paraphrasingBoundedAcc = questionBoundedAcc.correct.foldMap { predQuestion =>
      val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
      predictedParaphrases(predQuestion).toList.foldMap(predParaphrase =>
        goldParaphrases.questionParaphrases.get(predQString).fold(BoundedAcc.uncertain(predQuestion -> predParaphrase))(goldParaphraseLabels =>
          if(goldParaphraseLabels.correct.contains(predParaphrase)) BoundedAcc.correct(predQuestion -> predParaphrase)
          else if(goldParaphraseLabels.incorrect.contains(predParaphrase)) BoundedAcc.incorrect(predQuestion -> predParaphrase)
          else BoundedAcc.uncertain(predQuestion -> predParaphrase)
        )
      )
    }
    val clauseParaphrasingBoundedAcc = verbFrameset.getParaphrasingClauses(frameProbabilities).toList.foldMap(clauseTemplate =>
      if(goldParaphrases.correctClauses.contains(clauseTemplate)) BoundedAcc.correct(clauseTemplate)
      else if(goldParaphrases.incorrectClauses.contains(clauseTemplate)) BoundedAcc.incorrect(clauseTemplate)
      else BoundedAcc.uncertain(clauseTemplate)
    )
    "question accuracy" ->> questionBoundedAcc ::
      "question paraphrasing accuracy (correct questions)" ->> paraphrasingBoundedAcc ::
      "clause paraphrasing accuracy" ->> clauseParaphrasingBoundedAcc ::
      HNil
  }

  def runEvaluation(
    evalSet: Dataset,
    predictions: Stream[IO, SentencePrediction[QABeam]],
    filter: SimpleQAs.Filter,
    frameInductionResults: FrameInductionResults,
    paraphraseAnnotations: ParaphraseAnnotations
  ): IO[Unit] = for {
    results <- predictions.map { predSentence =>
      val goldSentence = evalSet.sentences(predSentence.sentenceId)
      predSentence.verbs.foldMap { verb =>
        val goldVerb = goldSentence.verbEntries(verb.verbIndex)
        val predictedQAs = protocol.filterBeam(filter, verb)
        val goldParaphrases = paraphraseAnnotations.get(predSentence.sentenceId)
          .flatMap(_.get(verb.verbIndex))
          .getOrElse(VerbParaphraseLabels.empty)
        val verbFrameset = frameInductionResults.frames(verb.verbInflectedForms)
        val frameProbabilities = frameInductionResults
          .assignments(verb.verbInflectedForms)(predSentence.sentenceId)(verb.verbIndex)
        getVerbResults(goldVerb, predictedQAs, goldParaphrases, verbFrameset, frameProbabilities)
      }
    }.compile.foldMonoid
    _ <- IO(println(getMetricsString(results)))
  } yield ()

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
    val paraphraseAnnotationsPath = predDir.resolve("gold-paraphrases.json")

    val outDir = predDir.resolve(relativeFramesDir)

    val resultsFilename = if(testOnTest) "results-test.json" else "results.json"
    val resultsPath = outDir.resolve(resultsFilename)

    for {
      evalSet <- logOp(s"Reading $evalSetName set", qasrl.bank.Data.readDataset(evalSetPath))
      filter <- {
        import io.circe.generic.auto._
        FileUtil.readJson[SimpleQAs.Filter](predDir.resolve("filter.json"))
      }
      predictions = {
        import qasrl.data.JsonCodecs._
        import io.circe.generic.auto._
        FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename)
      }
      paraphraseAnnotations <- logOp(
        "Reading paraphrase annotations",
        FileUtil.readJson[ParaphraseAnnotations](paraphraseAnnotationsPath)
      )
      frameInductionResults <- logOp(
        "Loading frames",
        FileUtil.readJson[FrameInductionResults](resultsPath)
      )
      _ <- runEvaluation(
        evalSet, predictions, filter, frameInductionResults, paraphraseAnnotations
      )
      // TODO launch browser
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val predPath = Opts.option[NIOPath](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val relativeFramesDir = Opts.option[String](
      "frames", metavar = "path", help = "Relative path to the directory with frame induction results."
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
