package qfirst.frames.crowd
import qfirst.frames._

import qasrl.crowd._
import qasrl.crowd.util.dollarsToCents
import qasrl.crowd.util.MultiContigSpanHighlightableSentenceComponent
import qasrl.crowd.util.Styles
import qasrl.crowd.util.implicits._

import qasrl.data.AnswerSpan

import spacro.tasks._
import spacro.ui._
import spacro.util.Span

import cats.implicits._

import nlpdata.util.Text

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

import radhoc._

class ClausalClient[SID : Writer : Reader](instructions: VdomTag)(
  implicit settings: QASRLEvaluationSettings,
  promptReader: Reader[ClausalPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[QASRLValidationAnswer]], // same as above
  ajaxRequestWriter: Writer[ClausalAjaxRequest[SID]] // "
) extends TaskClient[ClausalPrompt[SID], List[QASRLValidationAnswer], ClausalAjaxRequest[SID]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val AsyncContentComponent = new AsyncContentComponent[ClausalAjaxResponse]
  import AsyncContentComponent._
  val SpanHighlightingComponent = new SpanHighlightingComponent[(Int, Int)] // clause group, question
  import SpanHighlightingComponent._

  import MultiContigSpanHighlightableSentenceComponent._

  case class Question(
    verbIndex: Int,
    string: String
  )

  @Lenses case class State(
    curFocus: (Int, Int),
    isInterfaceFocused: Boolean,
    answers: List[List[QASRLValidationAnswer]]
  )

  object State {
    def initial(numQs: List[Int]) = State((0, 0), false, numQs.map(n => List.fill(n)(Answer(List.empty[Span]))))
  }

  val StateLocal = new LocalState[State]
  val DoubleLocal = new LocalState[Double]

  def validationAnswerOptics(focus: (Int, Int)) = State.answers
    .composeOptional(Optics.index(focus._1))
    .composeOptional(Optics.index(focus._2))

  def answerSpanOptics(focus: (Int, Int)) = validationAnswerOptics(focus)
    .composePrism(QASRLValidationAnswer.answer)
    .composeLens(Answer.spans)

  def updateResponse(state: State): Callback = Callback(setResponse(state.answers.flatten))

  def updateCurrentAnswers(
    state: StateVal[State])(
    highlightingState: SpanHighlightingState
  ) = state.modify(
    answerSpanOptics(state.get.curFocus).set(
      highlightingState.spans(state.get.curFocus)
    )
  )

  def toggleInvalidAtFocus(
    state: StateVal[State]) (
    highlightedAnswers: Map[(Int, Int), Answer])(
    focus: (Int, Int)
  ) = state.modify(
    validationAnswerOptics(focus).modify(va =>
      if (va.isInvalid) highlightedAnswers(focus)
      else InvalidQuestion
    )
  )

  def handleKey(
    state: StateVal[State])(
    questions: List[Question])(
    highlightedAnswers: Map[(Int, Int), Answer])(e: ReactKeyboardEvent): Callback = {
    Callback.empty // TODO
    // def nextQuestion = scope.modState(State.curQuestion.modify(i => (i + 1) % questions.size))
    // def prevQuestion =
    //   scope.modState(State.curQuestion.modify(i => (i + questions.size - 1) % questions.size))
    // def toggleInvalid =
    //   scope.zoomStateL(State.curQuestion).state >>= toggleInvalidAtIndex(highlightedAnswers)

    // if (isNotAssigned) {
    //   Callback.empty
    // } else
    //   CallbackOption.keyCodeSwitch(e) {
    //     case KeyCode.Up | KeyCode.W   => prevQuestion
    //     case KeyCode.Down | KeyCode.S => nextQuestion
    //     case KeyCode.Space            => toggleInvalid
    //   } >> e.preventDefaultCB
  }

  def qaField(
    s: StateVal[State], sentence: Vector[String], verbIndex: Int, question: String, prob: Double, highlightedAnswers: Map[(Int, Int), Answer])(
    index: (Int, Int)
  ) = {
    val isFocused = s.get.curFocus == index
    val answer = validationAnswerOptics(index).getOption(s.get).get

    <.div(
      ^.overflow := "hidden",
      <.div(
        Styles.unselectable,
        ^.float := "left",
        ^.minHeight := "1px",
        ^.border := "1px solid",
        ^.borderRadius := "2px",
        ^.textAlign := "center",
        ^.width := "55px",
        (^.backgroundColor := "#E01010").when(answer.isInvalid),
        ^.onClick --> toggleInvalidAtFocus(s)(highlightedAnswers)(index),
        "Invalid"
      ),
      <.span(
        Styles.bolded.when(isFocused),
        Styles.unselectable,
        ^.float := "left",
        ^.margin := "1px",
        ^.padding := "1px",
        ^.onClick --> s.modify(State.curFocus.set(index)),
        f"($prob%.2f) $question%s"
      ),
      <.div(
        Styles.answerIndicator,
        Styles.unselectable,
        ^.float := "left",
        ^.minHeight := "1px",
        ^.width := "25px",
        "-->".when(isFocused)
      ),
      <.div(
        ^.float := "left",
        ^.margin := "1px",
        ^.padding := "1px",
        answer match {
          case InvalidQuestion =>
            <.span(
              ^.color := "#CCCCCC",
              "N/A"
            )
          case Answer(spans) if spans.isEmpty && isFocused =>
            <.span(^.color := "#CCCCCC", "Highlight answer above, move with arrow keys or mouse")
          case Answer(spans) if isFocused => // spans nonempty
            (spans.flatMap { span =>
               List(
                 <.span(Text.renderSpan(sentence, (span.begin to span.end).toSet)),
                 <.span(" / ")
               )
             } ++ List(<.span(^.color := "#CCCCCC", "Highlight to add an answer"))).toVdomArray
          case Answer(spans) =>
            spans.map(s => Text.renderSpan(sentence, (s.begin to s.end).toSet)).mkString(" / ")
        }
      )
    )
  }

  case class ClauseGroup(
    frame: Frame,
    verbIndex: Int,
    slotProbs: Map[ArgumentSlot, Double],
    slotInvalidProbs: Map[ArgumentSlot, Double],
    slotSpans: Map[ArgumentSlot, List[(AnswerSpan, Double)]]
  ) {
    def aggregateProb = slotProbs.map(_._2).sum
    def bestSpansAboveThreshold(thresh: Double) = slotSpans.flatMap {
      case (slot, spans) => spans.filter(_._2 >= thresh)
          .sortBy(-_._2).headOption.map(_._1)
          .map(slot -> _)
    }.toMap
  }

  class FullUIBackend(scope: BackendScope[Unit, Unit]) {
    def render = {
      AsyncContent(
        AsyncContentProps(
          getContent = () => makeAjaxRequest(ClausalAjaxRequest(workerIdOpt, prompt.sentenceId)),
          render = {
            case Loading => <.div("Retrieving data...")
            case Loaded(ClausalAjaxResponse(workerInfoSummaryOpt, sentence)) =>
              val clauseGroups = sentence.verbs.toList.sortBy(_.verbIndex).flatMap { verb =>
                verb.questions.groupBy(pred => pred.questionSlots.structure -> pred.questionSlots.tan).toList
                  .sortBy(-_._2.map(_.questionProb).sum)
                  .map { case ((structure, tan), qPreds) =>
                    ClauseGroup(
                      Frame(structure, verb.verbInflectedForms, tan),
                      verb.verbIndex,
                      qPreds.map(p => p.questionSlots.answerSlot -> p.questionProb).toMap,
                      qPreds.map(p => p.questionSlots.answerSlot -> p.invalidProb).toMap,
                      qPreds.map(p => p.questionSlots.answerSlot -> p.answerSpans).toMap
                    )
                }
              }

              val questions = sentence.verbs.flatMap { verb =>
                val argMapping = verb.questions.flatMap { qPred =>
                  qPred.answerSpans.sortBy(-_._2).filter(_._2 > 0.4).map(_._1).headOption.map(span =>
                    qPred.questionSlots.answerSlot -> Text.renderSpan(
                      sentence.sentenceTokens, (span.begin until span.end).toSet
                    )
                  )
                }.toMap
                verb.questions.map { qPred =>
                  val question = qPred.questionSlots
                  val frame = Frame(question.structure, verb.verbInflectedForms, question.tan)
                  val questionString = frame.questionsForSlotWithArgs(question.answerSlot, argMapping).head
                  Question(verb.verbIndex, questionString)
                }
              }

              def getRemainingInAgreementGracePeriodOpt(summary: QASRLValidationWorkerInfoSummary) =
                Option(settings.validationAgreementGracePeriod - summary.numAssignmentsCompleted)
                  .filter(_ > 0)

              DoubleLocal.make(0.4) { clauseInclusionThreshold =>
                DoubleLocal.make(0.0) { questionInclusionThreshold =>
                  DoubleLocal.make(0.5) { spanInclusionThreshold =>
                    StateLocal.make(State.initial(clauseGroups.map(_.slotProbs.size))) { state =>
                      import state.get._
                      SpanHighlighting(
                        SpanHighlightingProps(
                          isEnabled = !isNotAssigned && answers(curFocus._1)(curFocus._2).isAnswer,
                          enableSpanOverlap = true,
                          update = updateCurrentAnswers(state),
                          render = {
                            case (hs @ SpanHighlightingState(spans, status), SpanHighlightingContext(_, hover, touch, cancelHighlight)) =>
                              val curVerbIndex = clauseGroups(curFocus._1).verbIndex
                              val inProgressAnswerOpt =
                                SpanHighlightingStatus.highlighting.getOption(status).map {
                                  case Highlighting(_, anchor, endpoint) => Span(anchor, endpoint)
                                }
                              val curAnswers = spans(curFocus)
                              val otherAnswers = (spans - curFocus).values.flatten
                              val highlightedAnswers = clauseGroups.zipWithIndex.flatMap {
                                case (cg, cIndex) => cg.slotProbs.toList.indices.map { qIndex =>
                                  (cIndex, qIndex) -> Answer(spans(cIndex -> qIndex))
                                }
                              }.toMap

                              val isCurrentInvalid = answers(curFocus._1)(curFocus._2).isInvalid
                              val touchWord = touch(curFocus)

                              <.div(
                                ^.classSet1("container-fluid"),
                                ^.onClick --> cancelHighlight,
                                <.div(
                                  instructions,
                                  ^.margin := "5px"
                                ),
                                workerInfoSummaryOpt.whenDefined(
                                  summary =>
                                  <.div(
                                    ^.classSet1("card"),
                                    ^.margin := "5px",
                                    <.p( // TODO
                                      <.span(
                                        Styles.bolded,
                                        """Proportion of questions you marked invalid: """,
                                        <.span(
                                          if (summary.proportionInvalid < settings.invalidProportionEstimateLowerBound ||
                                                summary.proportionInvalid > settings.invalidProportionEstimateUpperBound) {
                                            Styles.badRed
                                          } else {
                                            Styles.goodGreen
                                          },
                                          f"""${summary.proportionInvalid * 100.0}%.1f%%"""
                                        ),
                                        "."
                                      ),
                                      s""" This should generally be between
                                   ${(settings.invalidProportionEstimateLowerBound * 100).toInt}% and
                                   ${(settings.invalidProportionEstimateUpperBound * 100).toInt}%.""",
                                      (if (summary.proportionInvalid < 0.15)
                                         " Please be harsher on bad questions. "
                                       else "")
                                    ).when(!summary.proportionInvalid.isNaN),
                                    <.p(
                                      <.span(
                                        Styles.bolded,
                                        "Agreement score: ",
                                        <.span(
                                          if (summary.agreement <= settings.validationAgreementBlockingThreshold) {
                                            Styles.badRed
                                          } else if (summary.agreement <= settings.validationAgreementBlockingThreshold + 0.025) {
                                            TagMod(Styles.uncomfortableOrange, Styles.bolded)
                                          } else {
                                            Styles.goodGreen
                                          },
                                          f"""${summary.agreement * 100.0}%.1f%%"""
                                        ),
                                        "."
                                      ),
                                      f""" This must remain above ${settings.validationAgreementBlockingThreshold * 100.0}%.1f%%""",
                                      getRemainingInAgreementGracePeriodOpt(summary).fold(".")(
                                        remaining =>
                                        s" after the end of a grace period ($remaining HITs remaining)."
                                      )
                                    ).when(!summary.agreement.isNaN)
                                  )
                                ),
                                <.div(
                                  ^.classSet1("card"),
                                  ^.margin := "5px",
                                  ^.padding := "5px",
                                  ^.tabIndex := 0,
                                  ^.onFocus --> state.modify(State.isInterfaceFocused.set(true)),
                                  ^.onBlur --> state.modify(State.isInterfaceFocused.set(false)),
                                  ^.onKeyDown ==> (
                                    (e: ReactKeyboardEvent) =>
                                    handleKey(state)(questions)(highlightedAnswers)(e) >> cancelHighlight
                                  ),
                                  ^.position := "relative",
                                  // TODO fix this
                                  // <.div(
                                  //   ^.position := "absolute",
                                  //   ^.top := "20px",
                                  //   ^.left := "0px",
                                  //   ^.width := "100%",
                                  //   ^.height := "100%",
                                  //   ^.textAlign := "center",
                                  //   ^.color := "rgba(48, 140, 20, .3)",
                                  //   ^.fontSize := "48pt",
                                  //   (if (isNotAssigned) "Accept assignment to start"
                                  //    else "Click here to start")
                                  // ).when(!isInterfaceFocused),
                                  MultiContigSpanHighlightableSentence(
                                    MultiContigSpanHighlightableSentenceProps(
                                      sentence = sentence.sentenceTokens,
                                      styleForIndex = i =>
                                      TagMod(Styles.specialWord, Styles.niceBlue).when(i == curVerbIndex),
                                      highlightedSpans =
                                        (inProgressAnswerOpt.map(_ -> (^.backgroundColor := "#FF8000")) ::
                                           curAnswers
                                           .map(_ -> (^.backgroundColor := "#FFFF00"))
                                           .map(Some(_))).flatten,
                                      hover = hover(state.get.curFocus),
                                      touch = touch(state.get.curFocus),
                                      render = (
                                        elements =>
                                        <.p(Styles.largeText, Styles.unselectable, elements.toVdomArray)
                                      )
                                    )
                                  ),
                                  <.div(
                                    <.span("Clause inclusion threshold: "),
                                    <.input(
                                      ^.value := f"${clauseInclusionThreshold.get}%.2f",
                                      ^.onChange ==> ((e: ReactEventFromInput) =>
                                        scala.util.Try(e.target.value.toDouble).toOption.fold(Callback.empty)(clauseInclusionThreshold.set)
                                      )
                                    )
                                  ),
                                  <.div(
                                    <.span("Question inclusion threshold: "),
                                    <.input(
                                      ^.value := f"${questionInclusionThreshold.get}%.2f",
                                      ^.onChange ==> ((e: ReactEventFromInput) =>
                                        scala.util.Try(e.target.value.toDouble).toOption.fold(Callback.empty)(questionInclusionThreshold.set)
                                      )
                                    )
                                  ),
                                  <.div(
                                    <.span("Span inclusion threshold: "),
                                    <.input(
                                      ^.value := f"${spanInclusionThreshold.get}%.2f",
                                      ^.onChange ==> ((e: ReactEventFromInput) =>
                                        scala.util.Try(e.target.value.toDouble).toOption.fold(Callback.empty)(spanInclusionThreshold.set)
                                      )
                                    )
                                  ),
                                  clauseGroups.zipWithIndex
                                    .filter(_._1.aggregateProb >= clauseInclusionThreshold.get)
                                    .toVdomArray { case (clauseGroup, clauseIndex) =>
                                      val slotsWithProbs = clauseGroup.slotProbs.toList.sortBy(-_._2)
                                      val argValues = clauseGroup.bestSpansAboveThreshold(spanInclusionThreshold.get).map { case (slot, span) =>
                                        slot -> Text.renderSpan(sentence.sentenceTokens, (span.begin until span.end).toSet)
                                      }

                                      <.div(
                                        <.h5(f"(${clauseGroup.aggregateProb}%.2f) " + clauseGroup.frame.clausesWithArgs(argValues).head),
                                        <.ul(
                                          ^.classSet1("list-unstyled"),
                                          slotsWithProbs.zipWithIndex
                                            .filter(_._1._2 >= questionInclusionThreshold.get)
                                            .toVdomArray { case ((slot, prob), questionIndex) =>
                                            val questionString = clauseGroup.frame.questionsForSlotWithArgs(slot, argValues).head
                                              <.li(
                                                ^.key := s"question-$clauseIndex-$questionIndex",
                                                ^.display := "block",
                                                qaField(state, sentence.sentenceTokens, clauseGroup.verbIndex, questionString, prob, highlightedAnswers)(
                                                  (clauseIndex, questionIndex)
                                                ),
                                                <.div(
                                                  clauseGroup.slotSpans(slot).filter(_._2 >= spanInclusionThreshold.get).sortBy(-_._2).map {
                                                    case (span, prob) => Text.renderSpan(sentence.sentenceTokens, (span.begin until span.end).toSet) +
                                                        f" ($prob%.2f)"
                                                  }.mkString(" / ")
                                                )
                                              )
                                          }
                                        )
                                      )
                                  },
                                  <.p(
                                    s"Bonus: ${dollarsToCents(settings.validationBonus(questions.size))}c"
                                  )
                                ),
                                <.div(
                                  ^.classSet1("form-group"),
                                  ^.margin := "5px",
                                  <.textarea(
                                    ^.classSet1("form-control"),
                                    ^.name := FieldLabels.feedbackLabel,
                                    ^.rows := 3,
                                    ^.placeholder := "Feedback? (Optional)"
                                  )
                                ),
                                <.input(
                                  ^.classSet1("btn btn-primary btn-lg btn-block"),
                                  ^.margin := "5px",
                                  ^.`type` := "submit",
                                  ^.disabled := !answers.forall(_.forall(_.isComplete)),
                                  ^.id := FieldLabels.submitButtonLabel,
                                  ^.value := (
                                    if (isNotAssigned) "You must accept the HIT to submit results"
                                    else if (!answers.forall(_.forall(_.isComplete)))
                                      "You must respond to all questions to submit results"
                                    else "Submit"
                                  )
                                )
                              )
                          }
                        )
                      )
                    }
                  }
                }
              }
          }
        )
      )
    }
  }

  val FullUI = ScalaComponent
    .builder[Unit]("Full UI")
    .renderBackend[FullUIBackend]
    // .componentDidUpdate(_.backend.updateResponse) // TODO make response update properly
    .build

}
