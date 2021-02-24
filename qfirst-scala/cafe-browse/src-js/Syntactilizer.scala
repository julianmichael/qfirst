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

object Syntactilizer {

  import qfirst.cafe._

  case class Props(
    sentence: ConsolidatedSentence
  )

  case class QuestionStructure(
    pred: Predication.Clausal,
    path: ArgumentPath.Descent[Extraction]
  )


  case class QAStructure(
    questionPreds: Set[QuestionStructure],
    answerArgs: Set[Argument]
  )

  case class State(
    qaStructures: Map[String, QAStructure] = Map()
  )
  object State {
    // def fromQuestion(q: QuestionLabel) = {
    // }
  }

  class Backend(scope: BackendScope[Props, State]) {
    def render(props: Props, state: State) = <.div()
  }

  val Component = ScalaComponent.builder[Props]("Syntactilizer")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def make(sentence: ConsolidatedSentence) = Component(Props(sentence))
}
