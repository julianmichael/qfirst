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
    verbFramesets: Map[InflectedForms, VerbFrameset],
    paraphraseAnnotations: ParaphraseAnnotations
  ) = {
    val results = evalSet.sentences.toList.foldMap { case (sentenceId, sentence) =>
      import shapeless._
      import shapeless.syntax.singleton._
      import shapeless.record._
      import qfirst.metrics._
      sentence.verbEntries.values.toList
        .filter(verb => evaluationItems.contains((verb.verbInflectedForms, sentenceId, verb.verbIndex)))
        .flatMap { verb =>
          paraphraseAnnotations.get(sentenceId)
            .flatMap(_.get(verb.verbIndex))
            .map(verb -> _)
        }.flatMap { case (verb, goldParaphrases) =>
            val verbFrameset = verbFramesets(verb.verbInflectedForms)
            val paraphrasingFilter = ParaphrasingFilter.TwoThreshold(0.3, 0.4) // TODO optimize over multiple filters
            verbFrameset.instances
              .get(VerbId(sentenceId, verb.verbIndex))
              .map(frameProbabilities =>
                Evaluation.getVerbResults(
                  verb, goldParaphrases,
                  verbFrameset, frameProbabilities, paraphrasingFilter
                )
              )
        }.combineAll
    }
    val fullResults = {
      import shapeless._
      import shapeless.syntax.singleton._
      import shapeless.record._
      val numPredictedParaphrases = results("question template paraphrasing accuracy (correct QAs)").stats.predicted
      val numQuestions = results("number of questions")

      val numPredictedClauses = results("clause paraphrasing accuracy").stats.predicted
      val numVerbs = results("number of verbs")

      results +
        ("paraphrases per question" ->> (numPredictedParaphrases.toDouble / numQuestions)) +
        ("paraphrase clauses per verb" ->> (numPredictedClauses.toDouble / numVerbs))
    }
    IO(println(getMetricsString(fullResults))).as(fullResults)
  }

  // def getEvaluationItems(evalSet: Dataset, evaluationItemsPath: NIOPath) = {
  //   import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  //   if(Files.exists(evaluationItemsPath)) FileUtil.readJsonLines[(InflectedForms, String, Int)](evaluationItemsPath).compile.toList
  //   else logOp(
  //     s"Creating new sample for evaluation at $evaluationItemsPath", {
  //       val rand = new scala.util.Random(86735932569L)
  //       val allItems = rand.shuffle(
  //         filterDatasetNonDense(evalSet).sentences.values.iterator.flatMap(sentence =>
  //           sentence.verbEntries.values.toList.map(verb =>
  //             (verb.verbInflectedForms, sentence.sentenceId, verb.verbIndex)
  //           )
  //         )
  //       ).take(1000).toVector
  //       FileUtil.writeJsonLines(evaluationItemsPath, io.circe.Printer.noSpaces)(allItems).as(allItems)
  //     }
  //   )
  // }

  // def program(
  //   experimentName: String,
  //   testOnTest: Boolean
  // ): IO[ExitCode] = {
  //   for {
  //     config <- Config(RunMode.Sanity, VerbSenseConfig.EntropyOnly)
  //     evalSet <- config.readEvalSet
  //     evaluationItems <- config.getEvaluationItems
  //     paraphraseGold <- config.readGoldParaphrases
  //     verbFramesets <- config.readFramesets
  //     _ <- runEvaluation(
  //       evalSet, evaluationItems.toSet, verbFramesets, paraphraseGold
  //     )
  //   } yield ExitCode.Success
  // }

  // val runFrameInduction = Command(
  //   name = "mill qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
  //   header = "Induce verb frames."
  // ) {
  //   val experimentNameO = Opts.option[String](
  //     "name", metavar = "path", help = "Relative path to the directory with frame induction results."
  //   )
  //   val testOnTestO = Opts.flag(
  //     "test", help = "Evaluate on the test set instead of dev."
  //   ).orFalse

  //   (experimentNameO, testOnTestO).mapN(program)
  // }

  def run(args: List[String]): IO[ExitCode] = {
    // runFrameInduction.parse(args) match {
    //   case Left(help) => IO { System.err.println(help); ExitCode.Error }
    //   case Right(main) => main
    // }
    IO.pure(ExitCode.Success)
  }
}
