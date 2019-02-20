package qfirst.frames.annotation

import cats.Id
import cats.Order
import cats.data.NonEmptyList
import cats.implicits._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

// import qasrl.apps.util.Rgba

import qasrl.bank.AnswerSource
import qasrl.bank.AnnotationRound
import qasrl.bank.DataIndex
import qasrl.bank.DatasetPartition
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.DocumentMetadata
import qasrl.bank.Domain
import qasrl.bank.QuestionSource
import qasrl.bank.SentenceId

import qasrl.bank.service.DocumentService

import qasrl.data.AnswerLabel
import qasrl.data.AnswerJudgment
import qasrl.data.AnswerSpan
import qasrl.data.Answer
import qasrl.data.InvalidQuestion
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import scala.collection.immutable.SortedSet

import radhoc._

import io.circe._

import scala.concurrent.Future

object FrameAnnClient {

  val ParseJsonFetch = new CacheCallContent[String, Json]
  val OptIntLocal = new LocalState[Option[Int]]

  val S = FrameAnnStyles

  case class Props(
    docService: DocumentService[CacheCall],
    annService: ClauseAnnotationService[Future]
  )

  case class State()
  object State {
    val initial = State()
  }

  // @Lenses case class Span(
  //   start: Int,
  //   end: Int,
  //   text: String
  // )

  // @Lenses case class QAPair(
  //   question: String,
  //   spans: List[Span]
  // )

  // @Lenses case class Verb(
  //   verb: String,
  //   qa_pairs: List[QAPair],
  //   index: Int
  // )

  // @Lenses case class Parse(
  //   words: Vector[String],
  //   verbs: List[Verb]
  // )

  // @Lenses case class State(
  //   curText: String,
  //   curQuery: Option[String],
  //   curResult: Option[Parse]
  // )
  // object State {
  //   def initial = State("", None, None)
  // }


  // def makeStateValForState[P, S](
  //   scope: BackendScope[P, S],
  //   state: S
  // ) = StateVal[S](state, s => scope.setState(s))

  // val transparent = Rgba(255, 255, 255, 0.0)
  // val queryKeywordHighlightLayer = Rgba(255, 255, 0, 0.4)

  // val highlightLayerColors = List(
  //   // Rgba(255, 255,   0, 0.2), // yellow
  //   Rgba(  0, 128, 255, 0.1), // green-blue
  //   Rgba(255,   0, 128, 0.1), // magenta?
  //   Rgba( 64, 192,   0, 0.1), // something. idk
  //   Rgba(128,   0, 255, 0.1), // mystery
  //   Rgba(  0, 255, 128, 0.1)  // blue-green
  // )

  // def checkboxToggle[A](
  //   label: String,
  //   isValueActive: StateVal[Boolean]
  // ) = <.div(
  //   <.input(S.checkbox)(
  //     ^.`type` := "checkbox",
  //     ^.value := label,
  //     ^.checked := isValueActive.get,
  //     ^.onChange --> isValueActive.modify(!_)
  //   ),
  //   <.span(S.checkboxLabel)(
  //     label
  //   )
  // )

  // val helpModalId = "help-modal"
  // val helpModalLabelId = "help-modal-label"
  // val dataToggle = VdomAttr("data-toggle")
  // val dataTarget = VdomAttr("data-target")
  // val ariaLabelledBy = VdomAttr("aria-labelledby")
  // val ariaHidden = VdomAttr("aria-hidden")
  // val dataDismiss = VdomAttr("data-dismiss")
  // val ariaLabel = VdomAttr("aria-label")

  // import cats.Order.catsKernelOrderingForOrder

  // implicit val answerSpanOrder: Order[AnswerSpan] = Order.whenEqual(
  //   Order.by[AnswerSpan, Int](_.begin),
  //   Order.by[AnswerSpan, Int](_.end)
  // )

  // def spanOverlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
  //   x.begin < y.end && y.begin < x.end
  // }
  // def spanContains(s: AnswerSpan, q: Int): Boolean = {
  //   q >= s.begin && q < s.end
  // }

  // sealed trait SpanColoringSpec {
  //   def spansWithColors: List[(AnswerSpan, Rgba)]
  // }
  // case class RenderWholeSentence(val spansWithColors: List[(AnswerSpan, Rgba)]) extends SpanColoringSpec
  // case class RenderRelevantPortion(spansWithColorsNel: NonEmptyList[(AnswerSpan, Rgba)]) extends SpanColoringSpec {
  //   def spansWithColors = spansWithColorsNel.toList
  // }

  // def renderSentenceWithHighlights(
  //   sentenceTokens: Vector[String],
  //   coloringSpec: SpanColoringSpec,
  //   wordRenderers : Map[Int, VdomTag => VdomTag] = Map()
  // ) = {
  //   val containingSpan = coloringSpec match {
  //     case RenderWholeSentence(_) =>
  //       AnswerSpan(0, sentenceTokens.size)
  //     case RenderRelevantPortion(swcNel) =>
  //       val spans = swcNel.map(_._1)
  //       AnswerSpan(spans.map(_.begin).minimum, spans.map(_.end).maximum)
  //   }
  //   val wordIndexToLayeredColors = (containingSpan.begin until containingSpan.end).map { i =>
  //     i -> coloringSpec.spansWithColors.collect {
  //       case (span, color) if spanContains(span, i) => color
  //     }
  //   }.toMap
  //   val indexAfterToSpaceLayeredColors = ((containingSpan.begin + 1) to containingSpan.end).map { i =>
  //     i -> coloringSpec.spansWithColors.collect {
  //       case (span, color) if spanContains(span, i - 1) && spanContains(span, i) => color
  //     }
  //   }.toMap
  //   Text.render[Int, List, List[VdomElement]](
  //     words = sentenceTokens.indices.toList,
  //     getToken = (index: Int) => sentenceTokens(index),
  //     spaceFromNextWord = (nextIndex: Int) => {
  //       if(!spanContains(containingSpan, nextIndex) || nextIndex == containingSpan.begin) List() else {
  //         val colors = indexAfterToSpaceLayeredColors(nextIndex)
  //         val colorStr = NonEmptyList[Rgba](transparent, colors)
  //           .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
  //         List(
  //           <.span(
  //             ^.key := s"space-$nextIndex",
  //             ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
  //             " "
  //           )
  //         )
  //       }
  //     },
  //     renderWord = (index: Int) => {
  //       if(!spanContains(containingSpan, index)) List() else {
  //         val colorStr = NonEmptyList(transparent, wordIndexToLayeredColors(index))
  //           .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
  //         val render: (VdomTag => VdomTag) = wordRenderers.get(index).getOrElse((x: VdomTag) => x)
  //         val element: VdomTag = render(
  //           <.span(
  //             ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
  //             Text.normalizeToken(sentenceTokens(index))
  //           )
  //         )
  //         List(element(^.key := s"word-$index"))
  //       }
  //     }
  //   ).toVdomArray(x => x)
  // }

  // def makeAllHighlightedAnswer(
  //   sentenceTokens: Vector[String],
  //   spans: NonEmptyList[Span],
  //   color: Rgba
  // ): VdomArray = {
  //   val orderedSpans = spans.map(s => AnswerSpan(s.start, s.end + 1)).sorted
  //   case class GroupingState(
  //     completeGroups: List[NonEmptyList[AnswerSpan]],
  //     currentGroup: NonEmptyList[AnswerSpan]
  //   )
  //   val groupingState = orderedSpans.tail.foldLeft(GroupingState(Nil, NonEmptyList.of(orderedSpans.head))) {
  //     case (GroupingState(groups, curGroup), span) =>
  //       if(curGroup.exists(s => spanOverlaps(s, span))) {
  //         GroupingState(groups, span :: curGroup)
  //       } else {
  //         GroupingState(curGroup :: groups, NonEmptyList.of(span))
  //       }
  //   }
  //   val contigSpanLists = NonEmptyList(groupingState.currentGroup, groupingState.completeGroups)
  //   val answerHighlighties = contigSpanLists.reverse.map(spanList =>
  //     List(
  //       <.span(
  //         renderSentenceWithHighlights(sentenceTokens, RenderRelevantPortion(spanList.map(_ -> color)))
  //       )
  //     )
  //   ).intercalate(List(<.span(" / ")))
  //   answerHighlighties.zipWithIndex.toVdomArray { case (a, i) =>
  //     a(^.key := s"answerString-$i")
  //   }
  // }

  // val colspan = VdomAttr("colspan")

  // def qaLabelRow(
  //   sentenceTokens: Vector[String],
  //   qaPair: QAPair,
  //   color: Rgba
  // ) = {
  //   <.tr(S.qaPairRow)(
  //     <.td(S.questionCell)(
  //       <.span(S.questionText)(
  //         qaPair.question
  //       )
  //     ),
  //     <.td(S.answerCell)(
  //       <.span(S.answerText) {
  //         NonEmptyList.fromList(qaPair.spans).whenDefined { spansNel =>
  //           makeAllHighlightedAnswer(sentenceTokens, spansNel, color)
  //         }
  //       }
  //     )
  //   )
  // }

  // def verbEntryDisplay(
  //   sentenceTokens: Vector[String],
  //   verb: Verb,
  //   color: Rgba
  // ) = {
  //   <.div(S.verbEntryDisplay)(
  //     <.div(
  //       <.a(
  //         ^.name := s"verb-${verb.index}",
  //         ^.display := "block",
  //         ^.position := "relative",
  //         ^.visibility := "hidden"
  //       )
  //     ),
  //     <.div(S.verbHeading)(
  //       <.span(S.verbHeadingText)(
  //         ^.color := color.copy(a = 1.0).toColorStyleString,
  //         sentenceTokens(verb.index)
  //       )
  //     ),
  //     <.table(S.verbQAsTable)(
  //       <.tbody(S.verbQAsTableBody) {
  //         verb.qa_pairs.toVdomArray { qaPair =>
  //           qaLabelRow(sentenceTokens, qaPair, color)(^.key := s"short-${qaPair.question}")
  //         }
  //       }
  //     )
  //   )
  // }

  class Backend(scope: BackendScope[Props, State]) {

  //   def getParse(parserUrl: String, sentence: String): CacheCall[Json] = {
  //     import scala.concurrent.ExecutionContext.Implicits.global
  //     import io.circe.parser.parse
  //     Remote(org.scalajs.dom.ext.Ajax.post(url = parserUrl, data = sentence).map(r => parse(r.responseText).right.get))
  //   }

    def render(props: Props, state: State) = {
      <.div(S.mainContainer)(
        // <.div(S.fixedRowContainer, S.headyContainer)(
        //   <.h1(S.mainTitle)("QA-SRL Parser Demo")
        // ),
        // <.div(S.queryInputContainer, S.headyContainer)(
        //   <.input(S.queryInput)(
        //     ^.`type` := "text",
        //     ^.placeholder := "Type a sentence",
        //     ^.value := state.curText,
        //     ^.onChange ==> ((e: ReactEventFromInput) => scope.modState(State.curText.set(e.target.value))),
        //     ^.onKeyDown ==> (
        //       (e: ReactKeyboardEvent) => {
        //         CallbackOption.keyCodeSwitch(e) {
        //           case KeyCode.Enter => scope.modState(State.curQuery.set(Some(state.curText)))
        //         }
        //       }
        //     )
        //   ),
        //   <.button(S.querySubmitButton)(
        //     ^.disabled := state.curText.isEmpty,
        //     ^.onClick --> scope.modState(State.curQuery.set(Some(state.curText))),
        //     "Submit"
        //   )
        // ),
        // state.curQuery.whenDefined { curQuery =>
        //   ParseJsonFetch.make(request = curQuery, sendRequest = query => getParse(props.demoUrl, query)) {
        //     case ParseJsonFetch.Loading => <.div(S.loadingNotice)("Waiting for parse...")
        //     case ParseJsonFetch.Loaded(parseJson) =>
        //       import io.circe.generic.auto._
        //       parseJson.as[Parse] match {
        //         case Left(err) => <.div(S.loadingNotice)("Error parsing parse JSON: " + err.toString)
        //         case Right(parse) =>
        //           val sortedVerbs = parse.verbs.toList.sortBy(_.index)
        //           OptIntLocal.make(initialValue = None) { highlightedVerbIndex =>
        //             val answerSpansWithColors = for {
        //               (verb, index) <- sortedVerbs.zipWithIndex
        //               if highlightedVerbIndex.get.forall(_ == verb.index)
        //               qa <- verb.qa_pairs
        //               span <- qa.spans
        //             } yield AnswerSpan(span.start, span.end + 1) -> highlightLayerColors(index % highlightLayerColors.size)
        //             val verbColorMap = sortedVerbs
        //               .zipWithIndex.map { case (verb, index) =>
        //                 verb.index -> highlightLayerColors(index % highlightLayerColors.size)
        //             }.toMap

        //             <.div(S.flexyBottomContainer, S.flexColumnContainer)(
        //               <.div(S.fixedRowContainer, S.sentenceTextContainer, S.headyContainer)(
        //                 <.span(S.sentenceText)(
        //                   renderSentenceWithHighlights(
        //                     parse.words,
        //                     RenderWholeSentence(answerSpansWithColors),
        //                     verbColorMap.collect { case (verbIndex, color) =>
        //                       verbIndex -> (
        //                         (v: VdomTag) => <.a(
        //                           S.verbAnchorLink,
        //                           ^.href := s"#verb-$verbIndex",
        //                           v(
        //                             ^.color := color.copy(a = 1.0).toColorStyleString,
        //                             ^.fontWeight := "bold",
        //                             ^.onMouseMove --> (
        //                               if(highlightedVerbIndex.get == Some(verbIndex)) {
        //                                 Callback.empty
        //                               } else highlightedVerbIndex.set(Some(verbIndex))
        //                             ),
        //                             ^.onMouseOut --> highlightedVerbIndex.set(None)
        //                           )
        //                         )
        //                       )
        //                     }
        //                   )
        //                 )
        //               ),
        //               <.div(S.scrollPane)(
        //                 sortedVerbs.toVdomArray { verb =>
        //                   verbEntryDisplay(parse.words, verb, verbColorMap(verb.index))(
        //                     S.hoverHighlightedVerbTable.when(highlightedVerbIndex.get.exists(_ == verb.index)),
        //                     ^.key := verb.index,
        //                     ^.onMouseMove --> (
        //                       if(highlightedVerbIndex.get == Some(verb.index)) {
        //                         Callback.empty
        //                       } else highlightedVerbIndex.set(Some(verb.index))
        //                     ),
        //                     ^.onMouseOut --> highlightedVerbIndex.set(None)
        //                   )
        //                 }
        //               )
        //             )
        //           }
        //       }
        //   }
        // }
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("FrameAnnClient")
    .initialState(State.initial)
    .renderBackend[Backend]
    .build

}
