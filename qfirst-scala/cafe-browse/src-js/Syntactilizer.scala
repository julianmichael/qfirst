package qfirst.cafe
package browse

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
import scala.collection.immutable.SortedMap
import japgolly.scalajs.react.vdom.TagOf
import cats.data.Validated
import qasrl.Tense

import qfirst.parsing.ScoredStream
import qfirst.parsing.EvaluationBlock

import cats.collections.Heap
import qfirst.parsing.Scored

class LazyLoadingList[A] {

  case class Context(
    values: List[A],
    loadMore: Option[Int => Callback]
  )

  case class Props(
    source: Stream[A],
    initSize: Int,
    render: Context => VdomElement
  )

  case class State(
    values: List[A],
    done: Boolean
  )
  object State{
    def fromSource(source: Stream[A], size: Int) = {
      val values = source.take(size).toList
      val nowDone = values.size < size
      State(values, nowDone)
    }
  }

  class Backend(scope: BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      props.render(
        Context(
          state.values,
          if(state.done) None else Some { (i: Int) =>
            scope.setState(
              State.fromSource(props.source, state.values.size + i)
            )
          }
        )
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("Paginator")
    .initialStateFromProps(p => State.fromSource(p.source, p.initSize))
    .renderBackend[Backend]
    .build

  def make(
    source: Stream[A],
    initSize: Int)(
    render: Context => VdomElement
  )= Component(Props(source, initSize, render))
}

object Syntactilizer {

  val S = BrowserStyles
  val V = View

  val QuestionExpander = new LazyLoadingList[
    Scored[Either[EvaluationBlock, Argument.Clausal.FiniteQuestion]]
  ]

  def treeEltAux(
    tree: SyntaxTree[Argument, ArgContent],
    widthOffset: Int,
    curDepth: Int,
    maxDepth: Int
  ): Vector[TagOf[html.Div]] = tree match {
    case SyntaxTree.Leaf(content) => Vector(
      <.div(S.treeGridItem)(
        ^.gridColumn := s"$widthOffset",
        ^.gridRowStart := s"$curDepth",
        ^.gridRowEnd := s"${maxDepth + 1}",
      ),
      <.div(S.treeGridItemContent)(
        ^.gridColumn := s"$widthOffset",
        ^.gridRowStart := s"$maxDepth",
        ^.gridRowEnd := s"${maxDepth + 1}",
        content.text
      )
    )
    case node @ SyntaxTree.Node(arg, children) =>
      val nodeWidth = node.cata(_ => 1)((_, children) => math.max(1, children.sum))
      <.div(S.treeGridItem)(
        ^.gridColumnStart := s"$widthOffset",
        ^.gridColumnEnd := s"${widthOffset + nodeWidth}",
        ^.gridRow := s"$curDepth",
        <.div(S.treeGridItemContent)(
          arg.symbol
        )
      ) +: children.foldLeft((Vector[TagOf[html.Div]](), widthOffset)) {
        case ((prev, offset), child) =>
          val cur = prev ++ treeEltAux(child, offset, curDepth + 1, maxDepth)
          val childWidth = child.cata(_ => 1)((_, children) => math.max(1, children.sum))
          (cur, offset + childWidth)
      }._1
  }

  def argTreeElt(tree: SyntaxTree[Argument, ArgContent]) = {
    val depth = tree.depth
    val width = tree.cata(_ => 1)((_, children) => math.max(1, children.sum))
    <.div(
      ^.display.grid,
      ^.gridTemplateColumns := s"repeat($width, 1fr)",
      ^.gridTemplateRows := s"repeat($depth, 1fr) 2fr",
      ^.width := "50%",
      ^.gridGap := "5px",
      treeEltAux(tree, 1, 1, depth + 1).toVdomArray // TODO keys
    )
  }


  case class Props(
    sentence: ConsolidatedSentence
  ) {
    val parsing = new Parsing(sentence)
  }

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

  @Lenses case class State(
    questions: Map[Int, Map[String, ScoredStream[Either[EvaluationBlock, Argument.Clausal.FiniteQuestion]]]]
    // predsByIndex: Map[Int, Set[Predication]],
    // predsBySpan: Map[ESpan, Set[Predication]],
    // propsByVerbIndex: SortedMap[Int, Vector[Argument]]
    // spans: Map[SpanId, ESpan],
    // predicates: Map[PredicateId, Int],
    // verbStructures: Map[Int, VerbStructure]
  )
  object State {
    def fromProps(props: Props): State = {
      val qs = props.sentence.verbEntries.mapVals { verb =>
        verb.questionLabels.mapVals { question =>
          val form = SurfaceForm.fromQuestionSlots(
            verb.verbIndex,
            verb.verbInflectedForms,
            question.questionSlots
          )
          props.parsing.parse(form).map {
            case Left(block) => Left(block)
            case Right(deriv) => Right(deriv.item)
          }
        }
      }
      State(
        qs
      //   Map(), Map(),
      //   sent.verbEntries.map { case (k, v) => k -> Vector() }
      )
      // State(Map(), Map(), Map())
    }
  }

  val StateLocal = new LocalState[State]
  val ArgLocal = new LocalState[Argument]

  def unsafeOptionGet[A]: Lens[Option[A], A] = {
    Lens[Option[A], A](_.get)(a => opt => Some(a))
  }

  // def transitiveClause(index: Int, verbForms: InflectedForms) = {
  //   Argument.Finite(
  //     Some(
  //       Predication.Verbal(
  //         index = Some(index),
  //         subject = Argument.ProForm.what,
  //         verb = Lexicon.Verb(verbForms),
  //         isPassive = false,
  //         arguments = Vector(Argument.ProForm.what),
  //         tan = TAN(Some(Tense.Finite.Present), false, false, false)
  //       )
  //     )
  //   )
  // }

  def clauseEditor(clause: StateSnapshot[Argument]) = {
    clause.value.render(ArgPosition.Arg(0)) match {
      case Validated.Valid(tree) => argTreeElt(tree)
      case Validated.Invalid(err) => <.div(err.toString)
    }
  }

  def clauseViewer(clause: Argument) = {
    clause.render(ArgPosition.Arg(0)) match {
      case Validated.Valid(tree) => argTreeElt(tree)
      case Validated.Invalid(err) => <.div(err.toString)
    }
  }

  def verbPropEditor(
    sentence: ConsolidatedSentence,
    verbIndex: Int,
    state: State
    // verbState: StateSnapshot[Vector[Argument]]
  ) = {
    <.div(
      state.questions(verbIndex).toList.toVdomArray { case (q, cqs) =>
        <.div(
          <.div(q),
          <.div("Parses: ")(
            QuestionExpander.make(cqs.toStreamScored, 1) {
              case QuestionExpander.Context(items, expandOpt) =>
                <.div(
                  items.toVdomArray {
                    case Scored(Left(block), score) =>
                      <.div(f"No parses with score < $score%.2f")
                    case Scored(Right(question), score) =>
                      <.div(f"Score: $score%.2f")(
                        clauseViewer(question)
                      )
                  },
                  expandOpt match {
                    case None => <.div("all loaded")
                    case Some(expand) => <.button("more")(
                      ^.onClick --> expand(0)
                    )
                  }
                )
            }
          )
        )
      }

      // state.
      // verbState.value.indices.flatMap(index =>
      //   verbState.zoomStateO(
      //     Optics.index[Vector[Argument], Int, Argument](index)
      //   )
      // ).toVdomArray(clauseEditor(_)),
      // ArgLocal.make(
      //   transitiveClause(verbIndex, sentence.verbEntries(verbIndex).verbInflectedForms)
      // ) { clause =>
      //   clauseEditor(clause)
      // }
    )
  }

  def render(props: Props) = {
    import props.sentence
    println(props)
    <.div(
      StateLocal.make(State.fromProps(props)) { state =>
        println(state.value)
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

                verbPropEditor(
                  sentence,
                  verbIndex,
                  state.value,
                  // state.zoomStateL(
                  //   State.propsByVerbIndex
                  //     .composeLens(Optics.at(verbIndex))
                  //     .composeLens(unsafeOptionGet[Vector[Argument]])
                  // )
                )
              )
            }
          )
        )
      }
    )
  }

  val Component = ScalaComponent.builder[Props]("Syntactilizer")
    .render_P(render)
    .build

  def make(
    sentence: ConsolidatedSentence
  ) = Component(Props(sentence))
}
