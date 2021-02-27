package qfirst.cafe.browse

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import scala.collection.immutable.SortedSet

import scala.concurrent.ExecutionContext.Implicits.global

import cats.Id
import cats.Order
import cats.data.NonEmptyList
import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._

import jjm.OrWrapped
import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ui._
import jjm.implicits._

import qasrl.bank.AnnotationRound
import qasrl.bank.AnswerSource
import qasrl.bank.ConsolidatedSentence
import qasrl.bank.DataIndex
import qasrl.bank.DatasetPartition
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.DocumentMetadata
import qasrl.bank.Domain
import qasrl.bank.QuestionSource
import qasrl.bank.SentenceId

import qasrl.bank.service.DocumentService
// import qasrl.bank.service.Search

import qasrl.data.AnswerLabel
import qasrl.data.AnswerJudgment
import qasrl.data.Answer
import qasrl.data.InvalidQuestion
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel
import jjm.ling.en.InflectedForms

object Syntactilizer {

  import qfirst.cafe._

  case class Props(
    sentence: ConsolidatedSentence
  )

  // case class QuestionStructure(
  //   pred: Predication.Clausal,
  //   path: ArgumentPath.Descent[Extraction]
  // )

  // case class QAStructure(
  //   questionPreds: Set[QuestionStructure],
  //   answerSpans: Set[ESpan]
  // )
  // object QAStructure {
  //   def fromQA(verb: InflectedForms, q: QuestionLabel) = {
  //     val spans = q.answerJudgments
  //       .flatMap(_.judgment.getAnswer)
  //       .flatMap(_.spans.toList)
  //     QAStructure(
  //       Conversion.getAlignedPredications(verb, q)
  //         .map(Function.tupled(QuestionStructure(_, _))),
  //       spans
  //     )
  //   }
  // }

  // case class VerbStructure(
  //   qaStructures: Map[String, QAStructure]
  // )
  // object VerbStructure {
  //   def fromVerb(verb: VerbEntry) = {
  //     VerbStructure(
  //       verb.questionLabels
  //         .mapVals(q => QAStructure.fromQA(verb.verbInflectedForms, q))
  //     )
  //   }
  // }

  case class State(
    // spans: Map[SpanId, ESpan],
    // predicates: Map[PredicateId, Int],
    // verbStructures: Map[Int, VerbStructure]
  )
  object State {
    def fromSentence(sent: ConsolidatedSentence): State = {
      State()
      // State(Map(), Map(), Map())
    }
  }

  class Backend(scope: BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val S = BrowserStyles
      val V = View
      import props.sentence

      <.div(
        <.div(S.sentenceTextContainer)(
          <.span(S.sentenceText)(
            V.Spans.renderHighlightedPassage(
              sentence.sentenceTokens,
              Nil, Map()
              // answerSpansWithColors.toList,
              // verbColorMap.collect { case (verbIndex, color) =>
              //   verbIndex -> (
              //     (v: VdomTag) => <.a(
              //       S.verbAnchorLink,
              //       ^.href := s"#verb-$verbIndex",
              //       v(
              //         ^.color := color.copy(a = 1.0).toColorStyleString,
              //         ^.fontWeight := "bold",
              //         ^.onMouseMove --> (
              //           if(highlightedVerbIndex.value == Some(verbIndex)) {
              //             Callback.empty
              //           } else highlightedVerbIndex.setState(Some(verbIndex))
              //         ),
              //         ^.onMouseOut --> highlightedVerbIndex.setState(None)
              //       )
              //     )
              //   )
              // }
            )
          )
        ),
        <.div(S.verbEntriesContainer)(
          props.sentence.verbEntries.toList.toVdomArray { case (verbIndex, verb) =>
            <.div(S.verbEntryDisplay)(
              <.div(S.verbHeading)(
                <.span(S.verbHeadingText)(
                  // ^.color := color.copy(a = 1.0).toColorStyleString,
                  sentence.sentenceTokens(verbIndex)
                )
              ),
              <.table(S.verbQAsTable)()
            )
          }
        )
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("Syntactilizer")
    .initialStateFromProps(p => State.fromSentence(p.sentence))
    .renderBackend[Backend]
    .build

  def make(
    sentence: ConsolidatedSentence
  ) = Component(Props(sentence)
  )
}
