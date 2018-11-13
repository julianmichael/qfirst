package qfirst

import qfirst.metrics._

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.{AnswerJudgment, Answer, InvalidQuestion}
import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.QuestionLabel
import qasrl.data.Sentence
import qasrl.data.VerbEntry

import HasMetrics.ops._

object SandboxApp extends App {
  val train = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz"))

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

  import qfirst.{Instances => I}
  import qfirst.metrics.{Transformers => M}
  import shapeless._
  import shapeless.syntax.singleton._
  import shapeless.record._
  import monocle.function.{all => Optics}

  import qfirst.frames.TemplateStateMachine.allPrepositions

  def getPrepMetrics(sentence: Sentence, verb: VerbEntry) = (qLabel: QuestionLabel) => {
    val hasPrep = qLabel.answerJudgments
      .flatMap(_.judgment.getAnswer)
      .flatMap(_.spans)
      .map(s =>
      (
        if(allPrepositions.contains(sentence.sentenceTokens(s.begin).lowerCase)) 3
        else if(sentence.sentenceTokens.lift(s.begin - 1).map(_.lowerCase).exists(allPrepositions.contains)) 2
        else 1
      )
    ).max match {
      case 1 => "N"
      case 2 => "-"
      case 3 => "Y"
    }
    Bucketed(
      Map(
        Map("wh" -> qLabel.questionSlots.wh.toString) ->
          Bucketed(
            Map(
              Map("has prep" -> hasPrep) -> 1
            )
          )
      )
    )
  }

  val prepMetrics = train.sentences.values.toList.foldMap { sentence =>
    sentence.verbEntries.values.toList.foldMap { verb =>
      verb.questionLabels.values.toList.foldMap {
        getPrepMetrics(sentence, verb)
      }
    }
  }

  val getSpanLengthMetrics = (aj: AnswerJudgment) => aj match {
    case InvalidQuestion => Counts(0)
    case Answer(spans) => spans.toList.foldMap(s => Counts(s.end - s.begin))
  }

  val spanLengthMetrics = train.sentences.values.toList.foldMap { sentence =>
    sentence.verbEntries.values.toList.foldMap { verb =>
      verb.questionLabels.values.toList.foldMap { qLabel =>
        qLabel.answerJudgments.toList.map(_.judgment).foldMap(getSpanLengthMetrics)
      }
    }
  }

  println(getMetricsString(spanLengthMetrics))
  println(spanLengthMetrics.histogramString(75))
}
