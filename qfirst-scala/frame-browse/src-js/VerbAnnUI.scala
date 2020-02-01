package qfirst.frame.browse

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
import qfirst.frame._
import qfirst.frame.math._
import qfirst.model.eval.filterGoldNonDense
import qfirst.model.eval.filterOrigAnnotationRound

import cats.Id
import cats.Monoid
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._

import qasrl._
import qasrl.data._
import qasrl.labeling._

import qasrl.bank._

import qasrl.bank.service.DocumentService
import qasrl.bank.service.Search

import jjm.LowerCaseString
import jjm.OrWrapped
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.implicits._

import scala.collection.immutable.SortedSet

import radhoc._

import io.circe._

import scala.concurrent.Future

case class Rgba(r: Double, g: Double, b: Double, a: Double) {
  def add(that: Rgba) = {
    if(this.a == 0.0) that else if(that.a == 0.0) this else {
      val alpha = 1.0 - ((1.0 - a) * (1.0 - that.a))
      Rgba(
        (a * r / alpha) + ((1.0 - a) * (that.r * that.a) / alpha),
        (a * g / alpha) + ((1.0 - a) * (that.g * that.a) / alpha),
        (a * b / alpha) + ((1.0 - a) * (that.b * that.a) / alpha),
        alpha
      )
    }
  }
  def toColorStyleString = f"rgba(${scala.math.round(r)}%d, ${scala.math.round(g)}%d, ${scala.math.round(b)}%d, $a%.4f)"
}

sealed trait NavQuery
case class DatasetQuery(
  verbFormsMatch: Set[LowerCaseString],
  docMatch: Set[LowerCaseString],
  sentenceMatch: Set[LowerCaseString]
) extends NavQuery {
  def matchesVerb(forms: InflectedForms): Boolean = {
    val allForms = forms.allForms
    verbFormsMatch.forall(allForms.contains)
  }
  def matchesDoc(docMeta: DocumentMetadata): Boolean = {
    val docStr = docMeta.toString.lowerCase
    docMatch.forall(docStr.contains)
  }
  def matchesSentence(sentence: Sentence): Boolean = {
    val sentenceStr = (sentence.sentenceId :: sentence.sentenceTokens.toList).mkString(" ").lowerCase
    sentenceMatch.forall(sentenceStr.contains)
  }
}
case class EvalQuery(index: Int) extends NavQuery
object NavQuery {
  def default = DatasetQuery(Set(), Set(), Set())
  def fromString(path: String) = {
    scala.util.Try(path.toInt).toOption.map(EvalQuery(_): NavQuery).getOrElse {
      val verbMatch = Option(path).filter(_.nonEmpty).foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
      val verbRemainderOpt = Option(path.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
      val docMatch = verbRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
      val docRemainderOpt = verbRemainderOpt.map(_.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
      val sentMatch = docRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
      DatasetQuery(verbMatch, docMatch, sentMatch)
    }
  }
}

object VerbAnnUI {

  val S = VerbAnnStyles

  val DataFetch = new CacheCallContent[Unit, (DataIndex, Map[InflectedForms, Int])]
  val DocFetch = new CacheCallContent[DocumentId, Document]
  val DocOptFetch = new CacheCallContent[Option[DocumentId], Option[Document]]
  val SearchFetch = new CacheCallContent[Search.Query, Set[DocumentId]]
  // val FramesetFetch = new CacheCallContent[InflectedForms, VerbFrameset]
  val EvalItemFetch = new CacheCallContent[Int, ParaphrasingInfo]
  val IntLocal = new LocalState[Int]
  val VerbLocal = new LocalState[InflectedForms]
  val VerbModelLocal = new LocalState[Option[VerbClusterModel]]
  val DocMetaOptLocal = new LocalState[Option[DocumentMetadata]]
  val SentOptLocal = new LocalState[Option[Sentence]]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]
  val IntSetLocal = new LocalState[Set[Int]]
  val FrameChoiceLocal = new LocalState[Set[(QuestionId, ArgStructure, ArgumentSlot)]]
  val QuestionSetLocal = new LocalState[Set[QuestionId]]
  val BoolLocal = new LocalState[Boolean]
  val StringLocal = new LocalState[String]

  val (inflToString, inflFromString) = {
    import io.circe.syntax._
    import io.circe.parser.decode
    val printer = io.circe.Printer.noSpaces
    val toStr = (f: InflectedForms) => printer.pretty(f.asJson)
    val fromStr = (s: String) => decode[InflectedForms](s).right.get
    (toStr, fromStr)
  }

  def makeQidMap(sentenceId: String, verb: VerbEntry): Map[String, QuestionId] = {
    val questionSlotsWithStrings = verb.questionLabels.toList.map { case (qString, qLabel) => qLabel.questionSlots -> qString }
    val questionToQid = questionSlotsWithStrings.map(_._2).zip(
      qfirst.clause.ClauseResolution.getResolvedFramePairs(
        verb.verbInflectedForms, questionSlotsWithStrings.map(_._1)
      ).map { case (frame, slot) => QuestionId(VerbId(sentenceId, verb.verbIndex), frame, slot) }
    ).toMap
    questionToQid
  }

  case class Props(
    docService: DocumentService[OrWrapped[AsyncCallback, ?]],
    verbService: VerbFrameService[AsyncCallback],
    urlNavQuery: NavQuery,
    mode: RunMode
  )

  case class State()
  object State {
    val initial = State()
  }

  // TODO add typed style argument
  def checkboxToggle(
    label: String,
    isValueActive: StateSnapshot[Boolean]
  ) = <.span(S.checkboxSpan)(
    <.input(S.checkbox)(
      ^.`type` := "checkbox",
      ^.value := label,
      ^.checked := isValueActive.value,
      ^.onChange --> isValueActive.modState(!_)
    ),
    <.span(S.checkboxLabel)(
      label
    )
  )

  // TODO add style, also improve name
  def typedTextField[A](
    label: Option[String],
    value: StateSnapshot[A],
    makeValue: String => Option[A]
  ) = {
    <.span(
      label.whenDefined, // TODO more styling
      StringLocal.make(initialValue = value.value.toString) { inputText =>
        <.input(S.textField)(
          ^.`type` := "text",
          ^.value := inputText.value,
          ^.onChange ==> ((e: ReactEventFromInput) => inputText.setState(e.target.value)),
          ^.onKeyDown ==> ((e: ReactKeyboardEventFromInput) =>
            CallbackOption.keyCodeSwitch(e) {
              case KeyCode.Enter =>
                makeValue(inputText.value).fold(Callback.empty)(value.setState)
            }
          )
        )
      }
    )
  }

  // TODO more styling
  def liveTextField[A](style: TagMod)(
    label: Option[String],
    value: StateSnapshot[A],
    makeValue: String => Option[A]
  ) = {
    <.span(
      label.whenDefined, " ",
      StringLocal.make(initialValue = value.value.toString) { inputText =>
        BoolLocal.make(initialValue = false) { isInvalid =>
          <.input(S.textField, S.invalidTextBackground.when(isInvalid.value), style)(
            ^.`type` := "text",
            ^.value := inputText.value,
            ^.onChange ==> ((e: ReactEventFromInput) =>
              inputText.setState(e.target.value) >>
                makeValue(e.target.value).fold(isInvalid.setState(true))(v =>
                  isInvalid.setState(false) >> value.setState(v)
                )
            )
          )
        }
      }
    )
  }

  def doubleTextField(style: TagMod)(
    label: Option[String],
    value: StateSnapshot[Double]
  ): VdomElement = liveTextField[Double](style)(
    label, value, (s: String) => scala.util.Try(s.toDouble).toOption
  )
  // def doubleTextField(style: TagMod)(
  //   label: String,
  //   value: StateSnapshot[Double]
  // ): VdomElement = doubleTextField(style)(Some(label), value)

  def intArrowField(
    label: String,
    value: StateSnapshot[Int]
  ) = {
    <.span(
      s"$label: ",
      <.input(S.intArrowFieldInput)(
        ^.`type` := "number",
        ^.min := 1,
        ^.value := value.value,
        ^.onChange ==> ((e: ReactEventFromInput) =>
          value.setState(e.target.value.toInt)
        )
      )
    )
  }

  def clusterCriterionField(
    label: String,
    numClustersFallback: Int,
    maxLossFallback: Double,
    criterion: StateSnapshot[ClusterSplittingCriterion]
  ): VdomElement = {
    <.div(
      zoomStateP(criterion, ClusterSplittingCriterion.number) match {
        case Some(numClusters) => intArrowField(s"$label clusters", numClusters)
        case None => <.span(
          s"$label clusters: ",
          <.input(
            ^.`type` := "text",
            ^.onClick --> criterion.setState(ClusterSplittingCriterion.Number(numClustersFallback))
          )
        )
      },
      zoomStateP(criterion, ClusterSplittingCriterion.loss)(Reusability.double(1e-3)) match {
        case Some(maxLoss) => doubleTextField(S.shortTextField)(Some(s"$label max loss"), maxLoss)
        case None => <.span(
          s"$label max loss: ",
          <.input(
            ^.`type` := "text",
            ^.onClick --> criterion.setState(ClusterSplittingCriterion.Loss(maxLossFallback))
          )
        )
      }
    )
  }

  def sliderField(
    label: String,
    min: Double,
    max: Double,
    value: StateSnapshot[Double],
    numSigFigs: Int = 3
  ) = {
    val magnitude = scala.math.pow(10, numSigFigs).toInt
    def int(x: Double) = (x * magnitude).toInt
    def double(n: Int) = n.toDouble / magnitude
    <.span(
      doubleTextField(S.shortTextField)(Some(label), value),
      " ",
      <.input(/*TODO style*/)(
        ^.`type` := "range",
        ^.min := int(min),
        ^.max := int(max),
        ^.value := int(value.value),
        ^.onChange ==> ((e: ReactEventFromInput) =>
          value.setState(double(e.target.value.toInt))
        )
      )
    )
  }

  val transparent = Rgba(255, 255, 255, 0.0)
  val queryKeywordHighlightLayer = Rgba(255, 255, 0, 0.4)

  val highlightLayerColors = List(
    // Rgba(255, 255,   0, 0.2), // yellow
    Rgba(  0, 128, 255, 0.1), // green-blue
    Rgba(255,   0, 128, 0.1), // magenta?
    Rgba( 64, 192,   0, 0.1), // something. idk
    Rgba(128,   0, 255, 0.1), // mystery
    Rgba(  0, 255, 128, 0.1)  // blue-green
  )

  def getCurSentences(
    allSentences: SortedSet[Sentence],
    query: Search.Query
  ) = {
    val searchFilteredSentences = if(query.isEmpty) {
      allSentences
    } else {
      allSentences.filter { sent =>
        qasrl.bank.service.Search.getQueryMatchesInSentence(sent, query).nonEmpty
      }
    }
    searchFilteredSentences
  }

  def getRoundForQuestion(label: QuestionLabel) = {
    val qSource = label.questionSources.map(s => scala.util.Try(QuestionSource.fromString(s): QuestionSource).toOption).min
    qSource.map {
      case QuestionSource.Turker(_) => AnnotationRound.Original
      case QuestionSource.Model(_)  =>
        val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
          AnswerSource.fromString(s).round == AnnotationRound.Expansion
        )
        if(hasAnswersInExpansion) AnnotationRound.Expansion else AnnotationRound.Eval
    }
  }

  import cats.Order.catsKernelOrderingForOrder

  implicit val qasrlDataQuestionLabelOrder: Order[QuestionLabel] = Order.whenEqual(
    Order.by[QuestionLabel, Option[AnnotationRound]](getRoundForQuestion _),
    Order.by[QuestionLabel, String](_.questionString)
  )

  sealed trait SpanColoringSpec {
    def spansWithColors: List[(ESpan, Rgba)]
  }
  case class RenderWholeSentence(val spansWithColors: List[(ESpan, Rgba)]) extends SpanColoringSpec
  case class RenderRelevantPortion(spansWithColorsNel: NonEmptyList[(ESpan, Rgba)]) extends SpanColoringSpec {
    def spansWithColors = spansWithColorsNel.toList
  }

  def renderSentenceWithHighlights(
    sentenceTokens: Vector[String],
    coloringSpec: SpanColoringSpec,
    wordRenderers : Map[Int, VdomTag => VdomTag] = Map()
  ) = {
    val containingSpan = coloringSpec match {
      case RenderWholeSentence(_) =>
        ESpan(0, sentenceTokens.size)
      case RenderRelevantPortion(swcNel) =>
        val spans = swcNel.map(_._1)
        ESpan(spans.map(_.begin).minimum, spans.map(_.end).maximum)
    }
    val wordIndexToLayeredColors = (containingSpan.begin until containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if span.contains(i) => color
      }
    }.toMap
    val indexAfterToSpaceLayeredColors = ((containingSpan.begin + 1) to containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if span.contains(i - 1) && span.contains(i) => color
      }
    }.toMap
    Text.renderTokens[Int, List, List[VdomElement]](
      words = sentenceTokens.indices.toList,
      getToken = (index: Int) => sentenceTokens(index),
      spaceFromNextWord = (nextIndex: Int) => {
        if(!containingSpan.contains(nextIndex) || nextIndex == containingSpan.begin) List() else {
          val colors = indexAfterToSpaceLayeredColors(nextIndex)
          val colorStr = NonEmptyList[Rgba](transparent, colors)
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          List(
            <.span(
              ^.key := s"space-$nextIndex",
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              " "
            )
          )
        }
      },
      renderWord = (index: Int) => {
        if(!containingSpan.contains(index)) List() else {
          val colorStr = NonEmptyList(transparent, wordIndexToLayeredColors(index))
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          val render: (VdomTag => VdomTag) = wordRenderers.get(index).getOrElse((x: VdomTag) => x)
          val element: VdomTag = render(
            <.span(
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              Text.normalizeToken(sentenceTokens(index))
            )
          )
          List(element(^.key := s"word-$index"))
        }
      }
    ).toVdomArray(x => x)
  }

  def makeAllHighlightedAnswer(
    sentenceTokens: Vector[String],
    answers: NonEmptyList[Answer],
    color: Rgba
  ): VdomArray = {
    val orderedSpans = answers.flatMap(a => NonEmptyList.fromList(a.spans.toList).get).sorted
    case class GroupingState(
      completeGroups: List[NonEmptyList[ESpan]],
      currentGroup: NonEmptyList[ESpan]
    )
    val groupingState = orderedSpans.tail.foldLeft(GroupingState(Nil, NonEmptyList.of(orderedSpans.head))) {
      case (GroupingState(groups, curGroup), span) =>
        if(curGroup.exists(s => s.overlaps(span))) {
          GroupingState(groups, span :: curGroup)
        } else {
          GroupingState(curGroup :: groups, NonEmptyList.of(span))
        }
    }
    val contigSpanLists = NonEmptyList(groupingState.currentGroup, groupingState.completeGroups)
    val answerHighlighties = contigSpanLists.reverse.map(spanList =>
      List(
        <.span(
          renderSentenceWithHighlights(sentenceTokens, RenderRelevantPortion(spanList.map(_ -> color)))
        )
      )
    ).intercalate(List(<.span(" / ")))
    answerHighlighties.zipWithIndex.toVdomArray { case (a, i) =>
      a(^.key := s"answerString-$i")
    }
  }

  def isQuestionValid(
    label: QuestionLabel
  ): Boolean = {
    val includedJudgments = label.answerJudgments.filter(aj =>
      !(aj.sourceId.endsWith("-expansion") || aj.sourceId.endsWith("-eval"))
    )
    val numValidJudgments = includedJudgments.count(_.judgment.isAnswer)
    numValidJudgments.toDouble / includedJudgments.size > (4.99 / 6.0)
  }

  val colspan = VdomAttr("colspan")

  def qaLabelRow(
    sentence: Sentence,
    label: QuestionLabel,
    color: Rgba,
    qid: QuestionId
  ) = {
    val answerJudgments = label.answerJudgments.filter(aj =>
      !(aj.sourceId.endsWith("-expansion") || aj.sourceId.endsWith("-eval"))
    )
    val qSource = label.questionSources.map(s => scala.util.Try(QuestionSource.fromString(s): QuestionSource).toOption).min
    val roundIndicatorStyle = qSource match {
      case Some(QuestionSource.Turker(_)) => S.originalRoundIndicator
      case Some(QuestionSource.Model(_))  =>
        val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
          AnswerSource.fromString(s).round == AnnotationRound.Expansion
        )
        if(hasAnswersInExpansion) S.expansionRoundIndicator else S.evalRoundIndicator
      case None => S.predictionRoundIndicator
    }

    <.tr(S.qaPairRow)(
      <.td(roundIndicatorStyle),
      <.td(S.questionCell)(
        <.span(S.questionText)(
          <.span(label.questionString)
        )
      ),
      <.td(S.validityCell) {
        val numJudgments = answerJudgments.size
        val numValidJudgments = answerJudgments.count(_.judgment.isAnswer)
        val isConsideredValid = isQuestionValid(label)
          <.span(if(isConsideredValid) S.validValidityText else S.invalidValidityText)(
            s"$numValidJudgments/$numJudgments"
          )
      },
      <.td(S.answerCell)(
        <.span(S.answerText) {
          NonEmptyList.fromList(
            answerJudgments.toList.collect {
              case AnswerLabel(sourceId, Answer(spans)) => Answer(spans)
            }
          ).whenDefined { answersNel =>
            makeAllHighlightedAnswer(sentence.sentenceTokens, answersNel, color)
          }
        }
      )
    )
  }

  val ArgStructureOptLocal = new LocalState[Option[(ArgStructure, ArgumentSlot)]]

  def paraphrasingHighlightStyle(
    structure: (ArgStructure, ArgumentSlot),
    referenceOpt: Option[(ArgStructure, ArgumentSlot)],
    goldParaphrasesOpt: Option[VerbParaphraseLabels]
  ) = {
    (if(referenceOpt.exists(_ == structure)) Some(S.argStructureChoiceIsChosen)
     else (referenceOpt, goldParaphrasesOpt).mapN { (reference, goldParaphrases) =>
       if(goldParaphrases.paraphrases.equal(reference, structure)) Some(S.argStructureChoiceIsCorrectParaphrase)
       else if(goldParaphrases.paraphrases.apart(reference, structure)) Some(S.argStructureChoiceIsIncorrectParaphrase)
       else None
     }.flatten
    ).whenDefined
  }

  def tagModForStructureLabel(
    structure: (ArgStructure, ArgumentSlot),
    argStructureChoiceOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    argStructureHoverOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    goldParaphrasesOpt: Option[StateSnapshot[VerbParaphraseLabels]]
  ) = {
    TagMod(
      argStructureChoiceOpt.value match {
        case None => ^.onClick --> argStructureChoiceOpt.setState(Some(structure))
        case Some(`structure`) => ^.onClick --> argStructureChoiceOpt.setState(None)
        case Some(otherStructure) => goldParaphrasesOpt.whenDefined { goldParaphrases =>
          val paraphrases = goldParaphrases.zoomStateL(VerbParaphraseLabels.paraphrases)
            ^.onClick ==> (
              (e: ReactMouseEvent) => e.preventDefaultCB >> (
                if(e.altKey) {
                  if(paraphrases.value.apart(otherStructure, structure)) paraphrases.modState(_.unseparate(otherStructure, structure))
                  else paraphrases.modState(_.separate(otherStructure, structure))
                } else {
                  if(paraphrases.value.equal(otherStructure, structure)) paraphrases.modState(_.unequate(otherStructure, structure))
                  else paraphrases.modState(_.equate(otherStructure, structure))
                }
              )
            )
        }
      },
      paraphrasingHighlightStyle(structure, argStructureChoiceOpt.value.orElse(argStructureHoverOpt.value), goldParaphrasesOpt.map(_.value)),
      (^.onMouseMove --> argStructureHoverOpt.setState(Some(structure))).when(argStructureHoverOpt.value.isEmpty),
      ^.onMouseOut --> argStructureHoverOpt.setState(None)
    )
  }

  def verbEntryDisplay(
    curSentence: Sentence,
    verb: VerbEntry,
    // predictionsOpt: Option[Map[String, (SlotBasedLabel[VerbForm], Set[ESpan])]],
    goldStructureRelation: FiniteRelation[QuestionId, (ArgStructure, ArgumentSlot)],
    goldParaphrasesOpt: Option[StateSnapshot[VerbParaphraseLabels]],
    predictedParaphrasesOpt: Option[Map[QuestionId, Set[(ArgStructure, ArgumentSlot)]]],
    color: Rgba,
    argStructureChoiceOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    argStructureHoverOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    navQuery: StateSnapshot[NavQuery],
    displayQAs: Boolean,
    showInvalidQuestions: Boolean
  )(mods: TagMod*) = {
    val makeQid = makeQidMap(curSentence.sentenceId, verb)
    val getQString = makeQid.toList.map(_.swap).toMap
    def makeSurrogateFrame(structure: ArgStructure, useModal: Boolean) = {
      Frame(
        verb.verbInflectedForms, structure.args,
        tense = (if(useModal) Modal("might".lowerCase) else PresentTense),
        isPassive = structure.isPassive,
        isPerfect = false,
        isProgressive = false,
        isNegated = false)
    }

    val qaTable = if(displayQAs) Some {
      val goldTable = <.table(S.verbQAsTable)(
        <.tbody(S.verbQAsTableBody) {
          val allQuestionLabels = verb.questionLabels.toList.map(_._2)
            .filter(l => l.questionSources.exists(_.startsWith("turk-qasrl2.0-")))
            .sorted
          val allQuestionStructures = ClauseResolution.getResolvedStructures(allQuestionLabels.map(_.questionSlots))
          val questionLabelsWithStructures = allQuestionLabels.zip(allQuestionStructures)
            .filter(p => showInvalidQuestions || isQuestionValid(p._1))
          if(questionLabelsWithStructures.isEmpty) {
            TagMod(<.tr(<.td(<.span(S.loadingNotice)("All questions have been filtered out."))))
          } else {
            TagMod(
              questionLabelsWithStructures.flatMap { case (label, structure) =>
                val qid = makeQid(label.questionString)
                val exFrame = Frame(
                  verbInflectedForms = verb.verbInflectedForms, args = structure._1.args,
                  tense = label.tense, isPerfect = label.isPerfect, isProgressive = label.isProgressive,
                  isPassive = structure._1.isPassive,
                  isNegated = label.isNegated)
                List[TagMod](
                  qaLabelRow(curSentence, label, color, qid)(
                    ^.key := s"short-${label.questionString}",
                    tagModForStructureLabel(structure, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt)
                  ),
                  predictedParaphrasesOpt.whenDefined(predictedParaphrases =>
                    predictedParaphrases.getOrElse(qid, Set()).filter(_ != structure).toVdomArray { paraphraseStructure =>
                      val clauseIncorrect = goldParaphrasesOpt.exists(_.value.incorrectClauses.contains(paraphraseStructure._1))
                      val paraphraseFrame = exFrame.copy(
                        args = paraphraseStructure._1.args, isPassive = paraphraseStructure._1.isPassive
                      )
                      val paraphraseQString = paraphraseFrame.questionsForSlot(paraphraseStructure._2).head

                      <.tr(S.predictedParaphraseRow, S.incorrectClausePredictedParaphraseRow.when(clauseIncorrect))(
                        ^.key := s"predicted-paraphrase-${label.questionString}-$paraphraseQString",
                        <.td(), // to skip round indicator in previous row
                        <.td(S.predictedParaphraseCell)(
                          goldParaphrasesOpt.whenDefined { goldParaphrases =>
                            val clauseIncorrectness = goldParaphrases.zoomStateL(
                              VerbParaphraseLabels.incorrectClauses.composeLens(Optics.at(paraphraseStructure._1))
                            )
                            val paraphraseCorrectness = goldParaphrases.zoomStateL(
                              VerbParaphraseLabels.clauseAddingParaphraseLens(structure, paraphraseStructure)
                            )
                            val paraphraseIncorrectness = goldParaphrases.zoomStateL(
                              VerbParaphraseLabels.paraphrases.composeLens(EquivalenceWithApartness.apart(structure, paraphraseStructure))
                            )

                            <.div(S.goldClauseMarkerDisplay)(
                              if(clauseIncorrectness.value) {
                                <.label(S.extendedXLabel)(
                                  <.input(
                                    ^.`type` := "checkbox",
                                    ^.value := clauseIncorrectness.value,
                                    ^.onChange ==> ((e: ReactEventFromInput) =>
                                      clauseIncorrectness.setState(false)
                                    )
                                  ),
                                  <.div(S.extendedX)
                                )
                              } else {
                                TagMod(
                                  <.label(S.goldClauseCheckLabel)(
                                    <.input(
                                      ^.`type` := "checkbox",
                                      ^.value := paraphraseCorrectness.value,
                                      ^.onChange ==> ((e: ReactEventFromInput) =>
                                        if(paraphraseCorrectness.value) paraphraseCorrectness.setState(false)
                                        else paraphraseCorrectness.setState(true)
                                      )
                                    ),
                                    <.div(S.goldClauseCheck, S.goldClauseCheckCorrect.when(paraphraseCorrectness.value))
                                  ),
                                  <.label(S.goldClauseXLabel)(
                                    <.input(
                                      ^.`type` := "checkbox",
                                      ^.value := paraphraseIncorrectness.value,
                                      ^.onChange ==> ((e: ReactEventFromInput) =>
                                        if(paraphraseIncorrectness.value) paraphraseIncorrectness.setState(false)
                                        else paraphraseIncorrectness.setState(true)
                                      )
                                    ),
                                    <.div(S.goldClauseX, S.goldClauseXIncorrect.when(paraphraseIncorrectness.value))
                                  )
                                )
                              }
                            )
                          },
                          <.span(S.predictedParaphraseText, S.shiftedClauseTemplateDisplay.when(goldParaphrasesOpt.nonEmpty))(
                            tagModForStructureLabel(paraphraseStructure, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt),
                            paraphraseQString
                          )
                        )
                      )
                    }
                  )
                )
              }: _*
            )
          }
        }
      )

      val goldClausesOpt = goldParaphrasesOpt.map { goldParaphrases =>
        <.div(S.goldClausesDisplay)(
          <.div(S.goldClausesHeading)(
            <.span(S.goldClausesHeadingText)(
              "Full Gold Paraphrases:"
            )
          ),
          goldParaphrases.value.correctClauses.toList.toVdomArray { clauseTemplate =>
            val surrogateFrame = makeSurrogateFrame(clauseTemplate, true)
              <.div(S.goldClauseDisplay)(
                <.span(S.goldClauseText)(
                  surrogateFrame.clausesWithArgMarkers.head.map {
                    case Left(s) => <.span(s)
                    case Right(argSlot) => <.span(
                      BoolLocal.make(initialValue = false) { isEditingSlot =>
                        // val sigilSuffix = surrogateFrame.args.get(argSlot).get match {
                        //   case Noun(_) => ""
                        //   case Prep(p, _) =>
                        //     if(p.toString.contains(" do")) "[inf]"
                        //     else if(p.toString.contains(" doing")) "[ng]"
                        //     else ""
                        //   case Locative => "[where]"
                        // }
                        val thisArgChoice = clauseTemplate -> argSlot

                        // TODO add adverbial whs listed after each clause

                        <.span(
                          S.argPlaceholder,
                          S.goldMatchingArgMarker.when(goldStructureRelation.range.contains(thisArgChoice)))(
                          Option(
                            goldStructureRelation.preimage(goldParaphrases.value.paraphrases.equivalenceClass(thisArgChoice))
                              .filter(_.verbId.verbIndex == verb.verbIndex).map(getQString)
                          ).filter(_.nonEmpty).flatMap(
                            _.unorderedFoldMap(qString =>
                              verb.questionLabels(qString).answerJudgments.toList
                                .flatMap(_.judgment.getAnswer).flatMap(_.spans.toList)
                                .foldMap(s => Map(s -> 1))
                            ).toList.sortBy(-_._2).headOption.map(_._1).map(span => TagMod(Text.renderSpan(curSentence.sentenceTokens, span)))
                          ).getOrElse {
                            val prefix = surrogateFrame.args.get(argSlot) match {
                              case Some(Prep(p, _)) if p.endsWith(" doing".lowerCase) => "doing "
                              case Some(Prep(p, _)) if p.endsWith(" do".lowerCase) => "do "
                              case _ => ""
                            }
                            TagMod(prefix + surrogateFrame.args.get(argSlot).get.placeholder.mkString(" "))
                          },
                          tagModForStructureLabel(thisArgChoice, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt)
                        )
                      }
                    )
                  }.map(List(_)).intercalate(List(<.span(" "))).zipWithIndex.map(p => p._1(^.key := "frame-clause-tok-" + p._2.toString)).toVdomArray
                )
              )
          }
        )
      }

      <.div(goldTable, goldClausesOpt.whenDefined)
      // predictionsOpt.fold(<.div(goldTable, goldClausesOpt.whenDefined)) { predictions =>
      //   <.div(
      //     <.div(S.goldQAsIndicatorDisplay)(<.span(S.goldQAsIndicatorText)("Gold:")),
      //     goldTable,
      //     <.div(S.predQAsIndicatorDisplay)(<.span(S.predQAsIndicatorText)("Predicted:")),
      //     <.table(S.verbQAsTable)(
      //       <.tbody(S.verbQAsTableBody) {
      //         val questions = predictions.toList.sortBy(_._1)
      //         val questionStructures = ClauseResolution.getResolvedStructures(questions.map(_._2._1))
      //         if(questions.isEmpty) {
      //           <.tr(<.td(<.span(S.loadingNotice)("No predictions.")))
      //         } else TagMod(
      //           (questions.zip(questionStructures)).flatMap { case ((qString, (qSlots, spans)), structure) =>
      //             val qid = QuestionId(SentenceId.fromString(curSentence.sentenceId), verb.verbIndex, qString)
      //             val resolutions = ClauseResolution.getFramesWithAnswerSlots(verb.verbInflectedForms, qSlots)
      //             val exFrame = resolutions.head._1
      //             val questionLabel = QuestionLabel(
      //               qString, Set("afirst-model"), Set(AnswerLabel("afirst-model", qasrl.data.Answer(spans))), qSlots,
      //               exFrame.tense, exFrame.isPerfect, exFrame.isProgressive, exFrame.isNegated, exFrame.isPassive
      //             )
      //             List[TagMod](
      //               qaLabelRow(curSentence, questionLabel, color, qid)(
      //                 ^.key := s"short-$qString",
      //                 tagModForStructureLabel(structure, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt)
      //               ),
      //               predictedParaphrasesOpt.whenDefined(predictedParaphrases =>
      //                 predictedParaphrases.getOrElse(structure, Set()).filter(_ != structure).toVdomArray { paraphraseStructure =>
      //                   val clauseIncorrect = goldParaphrasesOpt.exists(_.value.incorrectClauses.contains(paraphraseStructure._1))
      //                   val paraphraseQString = exFrame.copy(
      //                     args = paraphraseStructure._1.args, isPassive = paraphraseStructure._1.isPassive
      //                   ).questionsForSlot(paraphraseStructure._2).head
      //                   <.tr(S.predictedParaphraseRow, S.incorrectClausePredictedParaphraseRow.when(clauseIncorrect))(
      //                     ^.key := s"predicted-paraphrase-$qString-$paraphraseQString",
      //                     <.td(), // to skip round indicator in previous row
      //                     <.td(S.predictedParaphraseCell)(
      //                       goldParaphrasesOpt.whenDefined { goldParaphrases =>
      //                         val clauseIncorrectness = goldParaphrases.zoomStateL(
      //                           VerbParaphraseLabels.incorrectClauses.composeLens(Optics.at(paraphraseStructure._1))
      //                         )
      //                         val paraphraseCorrectness = goldParaphrases.zoomStateL(
      //                           VerbParaphraseLabels.clauseAddingParaphraseLens(structure, paraphraseStructure)
      //                         )
      //                         val paraphraseIncorrectness = goldParaphrases.zoomStateL(
      //                           VerbParaphraseLabels.paraphrases.composeLens(EquivalenceWithApartness.apart(structure, paraphraseStructure))
      //                         )

      //                         <.div(S.goldClauseMarkerDisplay)(
      //                           if(clauseIncorrectness.value) {
      //                             <.label(S.extendedXLabel)(
      //                               <.input(
      //                                 ^.`type` := "checkbox",
      //                                 ^.value := clauseIncorrectness.value,
      //                                 ^.onChange ==> ((e: ReactEventFromInput) =>
      //                                   clauseIncorrectness.setState(false)
      //                                 )
      //                               ),
      //                               <.div(S.extendedX)
      //                             )
      //                           } else {
      //                             TagMod(
      //                               <.label(S.goldClauseCheckLabel)(
      //                                 <.input(
      //                                   ^.`type` := "checkbox",
      //                                   ^.value := paraphraseCorrectness.value,
      //                                   ^.onChange ==> ((e: ReactEventFromInput) =>
      //                                     if(paraphraseCorrectness.value) paraphraseCorrectness.setState(false)
      //                                     else paraphraseCorrectness.setState(true)
      //                                   )
      //                                 ),
      //                                 <.div(S.goldClauseCheck, S.goldClauseCheckCorrect.when(paraphraseCorrectness.value))
      //                               ),
      //                               <.label(S.goldClauseXLabel)(
      //                                 <.input(
      //                                   ^.`type` := "checkbox",
      //                                   ^.value := paraphraseIncorrectness.value,
      //                                   ^.onChange ==> ((e: ReactEventFromInput) =>
      //                                     if(paraphraseIncorrectness.value) paraphraseIncorrectness.setState(false)
      //                                     else paraphraseIncorrectness.setState(true)
      //                                   )
      //                                 ),
      //                                 <.div(S.goldClauseX, S.goldClauseXIncorrect.when(paraphraseIncorrectness.value))
      //                               )
      //                             )
      //                           }
      //                         )
      //                       },
      //                       <.span(S.predictedParaphraseText, S.shiftedClauseTemplateDisplay.when(goldParaphrasesOpt.nonEmpty))(
      //                         tagModForStructureLabel(paraphraseStructure, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt),
      //                         paraphraseQString
      //                       )
      //                     )
      //                   )
      //                 }
      //               )
      //             )
      //           }: _*
      //         )
      //       }
      //     ),
      //     goldClausesOpt.whenDefined
      //   )
      // }
    } else None

    <.div(S.verbEntryDisplay)(
      <.div(
        <.a(
          ^.name := s"verb-${verb.verbIndex}",
          ^.display := "block",
          ^.position := "relative",
          ^.visibility := "hidden"
        )
      ),
      <.div(S.verbHeading)(
        <.span(S.verbHeadingText)(
          ^.color := color.copy(a = 1.0).toColorStyleString,
          ^.onClick --> (
            navQuery.setState(
              DatasetQuery(
                verb.verbInflectedForms.allForms.toSet,
                Set(SentenceId.fromString(curSentence.sentenceId).documentId.toString.lowerCase),
                Set(curSentence.sentenceId.lowerCase)
              )
            )
          ),
          curSentence.sentenceTokens(verb.verbIndex)
        )
      ),
      qaTable.whenDefined
    )(mods: _*)
  }

  def docSelectionPane(
    totalNumDocs: Int,
    curDocMetas: SortedSet[DocumentMetadata],
    curDocMeta: StateSnapshot[DocumentMetadata]
  ) = {
    <.div(S.documentSelectionPaneContainer)(
      <.div(S.documentCountLabel)(
        <.span(S.documentCountLabelText)(
          s"${curDocMetas.size} / $totalNumDocs documents"
        )
      ),
      <.div(S.documentSelectionPane)(
        curDocMetas.toVdomArray { docMeta =>
          <.div(S.documentSelectionEntry)(
            ^.key := docMeta.id.toString,
            if(docMeta == curDocMeta.value) S.currentSelectionEntry else S.nonCurrentSelectionEntry,
            ^.onClick --> curDocMeta.setState(docMeta),
            <.span(S.documentSelectionEntryText)(
              docMeta.title
            )
          )
        }
      )
    )
  }

  def sentenceSelectionPane(
    numSentencesInDocument: Int,
    curSentences: SortedSet[Sentence],
    searchQuery: Search.Query,
    curSentence: StateSnapshot[Sentence]
  ) = {
    val sentencesWord = if(numSentencesInDocument == 1) "sentence" else "sentences"
    val sentenceCountLabel = if(curSentences.size == numSentencesInDocument) {
      s"$numSentencesInDocument $sentencesWord"
    } else {
      s"${curSentences.size} / $numSentencesInDocument $sentencesWord"
    }
    val curSentenceId = SentenceId.fromString(curSentence.value.sentenceId)

    <.div(S.sentenceSelectionPaneContainer)(
      <.div(S.sentenceCountLabel)(
        <.span(S.sentenceCountLabelText)(
          sentenceCountLabel
        )
      ),
      <.div(S.sentenceSelectionPane)(
        curSentences.toVdomArray { sentence =>
          val spanHighlights = qasrl.bank.service.Search.getQueryMatchesInSentence(sentence, searchQuery).toList.map(index =>
            ESpan(index, index + 1) -> queryKeywordHighlightLayer
          )
          <.div(S.sentenceSelectionEntry)(
            ^.key := sentence.sentenceId,
            if(sentence == curSentence.value) S.currentSelectionEntry else S.nonCurrentSelectionEntry,
            ^.onClick --> curSentence.setState(sentence),
            <.span(S.sentenceSelectionEntryText)(
              renderSentenceWithHighlights(sentence.sentenceTokens, RenderWholeSentence(spanHighlights))
            )
          )
        }
      )
    )
  }


  // var qidToFramesCache = Map.empty[QuestionId, Set[(Frame, ArgumentSlot)]]
  def sentenceDisplayPane(
    part: DatasetPartition,
    docMeta: DocumentMetadata,
    sentence: Sentence,
    goldStructureRelation: FiniteRelation[QuestionId, (ArgStructure, ArgumentSlot)],
    // predictionsOpt: Option[Map[Int, Map[String, (SlotBasedLabel[VerbForm], Set[ESpan])]]],
    verbForms: InflectedForms,
    verbIndexOpt: Option[Int],
    goldParaphrasesOpt: Option[StateSnapshot[VerbParaphraseLabels]],
    predictedParaphrasesOpt: Option[Map[QuestionId, Set[(ArgStructure, ArgumentSlot)]]],
    argStructureChoiceOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    argStructureHoverOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    navQuery: StateSnapshot[NavQuery],
    showInvalidQuestions: Boolean
  ) = {
    val sentenceId = SentenceId.fromString(sentence.sentenceId)
    val (sortedVerbs, searchedVerbIndices) = verbIndexOpt.map(vi => List(sentence.verbEntries(vi)) -> Set(vi)).getOrElse {
      val (targetedVerbs, untargetedVerbs) = sentence.verbEntries.values
        .toList.partition(_.verbInflectedForms == verbForms)
      (targetedVerbs.sortBy(_.verbIndex) ++ untargetedVerbs.sortBy(_.verbIndex)) -> targetedVerbs.map(_.verbIndex).toSet
    }
    // val targetedQids = targetedVerbs.flatMap { verb =>
    //   verb.questionLabels.keys.toList.map { question =>
    //     val qid = QuestionId(SentenceId.fromString(sentence.sentenceId), verb.verbIndex, question)
    //     qid
    //   }
    // }
    IntSetLocal.make(initialValue = searchedVerbIndices) { highlightedVerbIndices =>
      val answerSpansWithColors = for {
        (verb, index) <- sortedVerbs.zipWithIndex
        if highlightedVerbIndices.value.contains(verb.verbIndex)
        question <- verb.questionLabels.values.toList
        answerLabel <- question.answerJudgments
        Answer(spans) <- answerLabel.judgment.getAnswer.toList
        span <- spans.toList
      } yield span -> highlightLayerColors(index % highlightLayerColors.size)
      val verbColorMap = sortedVerbs
        .zipWithIndex.map { case (verb, index) =>
          verb.verbIndex -> highlightLayerColors(index % highlightLayerColors.size)
      }.toMap

      <.div(S.sentenceDisplayPane)(
        <.div(S.sentenceInfoContainer)(
          <.span(S.sentenceInfoText) {
            val abbrevTitle = if(docMeta.title.length <= 30) docMeta.title else docMeta.title.take(27) + "..."
            s"$part / ${docMeta.id.domain} / ${docMeta.id.id} ($abbrevTitle) / paragraph ${sentenceId.paragraphNum}, sentence ${sentenceId.sentenceNum}"
          }
        ),
        <.div(S.sentenceTextContainer)(
          <.span(S.sentenceText)(
            renderSentenceWithHighlights(
              sentence.sentenceTokens,
              RenderWholeSentence(answerSpansWithColors),
              verbColorMap.collect { case (verbIndex, color) =>
                verbIndex -> (
                  (v: VdomTag) => <.a(
                    S.verbAnchorLink,
                    ^.href := s"#verb-$verbIndex",
                    v(
                      ^.color := color.copy(a = 1.0).toColorStyleString,
                      ^.fontWeight := "bold",
                      ^.onMouseMove --> (
                        if(highlightedVerbIndices.value == Set(verbIndex)) {
                          Callback.empty
                        } else highlightedVerbIndices.setState(Set(verbIndex))
                      ),
                      ^.onMouseOut --> highlightedVerbIndices.setState(searchedVerbIndices)
                    )
                  )
                )
              }
            )
          )
        ),
        <.div(S.verbEntriesContainer)(
          sortedVerbs.toVdomArray { verb =>
            verbEntryDisplay(
              sentence, verb, // predictionsOpt.map(_(verb.verbIndex)),
              goldStructureRelation,
              goldParaphrasesOpt, predictedParaphrasesOpt, verbColorMap(verb.verbIndex), argStructureChoiceOpt, argStructureHoverOpt,
              navQuery, displayQAs = verb.verbInflectedForms == verbForms,
              showInvalidQuestions)(
              S.hoverHighlightedVerbTable.when(highlightedVerbIndices.value == Set(verb.verbIndex)),
              ^.key := verb.verbIndex,
              ^.onMouseMove --> (
                if(highlightedVerbIndices.value == Set(verb.verbIndex)) {
                  Callback.empty
                } else highlightedVerbIndices.setState(Set(verb.verbIndex))
              ),
              ^.onMouseOut --> highlightedVerbIndices.setState(searchedVerbIndices)
            )
          }
        )
      )
    }
  }

  val textFocusingRef = Ref[html.Element]

  def unsafeListAt[A](index: Int) =
    Lens[List[A], A](s => s(index))(a => s => s.updated(index, a))

  def sequenceLenses[A, B, C](l1: Lens[A, B], l2: Lens[A, C]) =
    Lens[A, (B, C)](a => l1.get(a) -> l2.get(a)) {
      case (b, c) => a => l2.set(c)(l1.set(b)(a))
    }

  def throughOption[A, B](l: Lens[A, Option[B]])(implicit M: Monoid[B]): Lens[A, B] = {
    Lens[A, B](a => l.get(a).combineAll)(
      b => a => if(b == M.empty) l.set(None)(a) else l.set(Some(b))(a)
    )
  }

  val DoubleLocal = new LocalState[Double]

  // TODO color-code the answer spans by _question_ instead of by verb

  val defaultParaphrasingFilter = ParaphrasingFilter(
    ClusterSplittingCriterion.Number(1),
    ClusterSplittingCriterion.Number(5),
    0.02, 0.3
  )

  def frameDisplayPane(
    dataIndex: DataIndex,
    verbInflectedForms: InflectedForms,
    curDocMetasOpt: Option[Set[DocumentMetadata]],
    sentenceOpt: Option[Sentence],
    verbIndices: Set[Int],
    goldStructureRelationOpt: Option[FiniteRelation[QuestionId, (ArgStructure, ArgumentSlot)]],
    frameset: VerbFrameset,
    maxLoss: Double,
    goldParaphrasesOpt: Option[StateSnapshot[VerbParaphraseLabels]],
    predictedParaphrasesOpt: Option[Map[QuestionId, Set[(ArgStructure, ArgumentSlot)]]],
    cachedParaphrasingFilter: StateSnapshot[ParaphrasingFilter],
    paraphrasingFilter: StateSnapshot[ParaphrasingFilter],
    argStructureChoiceOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    argStructureHoverOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
    navQuery: StateSnapshot[NavQuery]
  ) = {
    // val fullStructureRelationOpt = (goldStructureRelationOpt, predStructureRelationOpt).mapN(_ |+| _)
    val numFrames = frameset.frames.size

    // TODO TODO !!! calculate these correctly
    val maxNumQuestions: Int = 5
    val maxQuestionLoss: Double = 0.0

    val sentenceIdOpt = sentenceOpt.map(s => SentenceId.fromString(s.sentenceId))
    val numInstances = frameset.frames.foldMap(_.verbIds.size)
    def makeSurrogateFrame(structure: ArgStructure, forms: InflectedForms, useModal: Boolean) = {
      Frame(
        forms, structure.args,
        tense = (if(useModal) Modal("might".lowerCase) else PresentTense),
        isPassive = structure.isPassive,
        isPerfect = false,
        isProgressive = false,
        isNegated = false)
    }
    // val presentTan = TAN(qasrl.PresentTense, false, false, false)
    // val mightTan = TAN(qasrl.Modal("might".lowerCase), false, false, false)
    val docIdToDocMetaOpt = curDocMetasOpt.map(_.map(m => m.id -> m).toMap)

    val predictedParaphraseClauseTemplatesOpt = predictedParaphrasesOpt.map(
      _.toList.foldMap(_._2).map(_._1)
    )

    def isClauseProbabilityAcceptable(p: Double) = true || p >= 0.01 || paraphrasingFilter.value.minClauseProb <= p

    println("12.1")

    <.div(S.framesetContainer)(
      <.div(S.paraphrasingFilterDisplay)(
        clusterCriterionField("Verb", numFrames, maxLoss, paraphrasingFilter.zoomStateL(ParaphrasingFilter.verbCriterion)),
        clusterCriterionField("Question", maxNumQuestions, maxQuestionLoss, paraphrasingFilter.zoomStateL(ParaphrasingFilter.questionCriterion)),
        <.div(sliderField("Clause", 0.0, 1.0, paraphrasingFilter.zoomStateL(ParaphrasingFilter.minClauseProb))),
        <.div(sliderField("Paraphrase", 0.0, 1.0, paraphrasingFilter.zoomStateL(ParaphrasingFilter.minParaphrasingProb))),
        <.div(
          <.button(
            "cache",
            ^.onClick --> cachedParaphrasingFilter.setState(paraphrasingFilter.value)
          )
        ),
        <.div(f"Verb Loss/instance: ${maxLoss / numInstances}%.3f")
      ),
      // only expect this to happen in eval anyway, when verbIndices has at most 1 elt
      // (sentenceOpt, verbIndices.headOption, goldParaphrasesOpt, paraphrasingFilterOpt).mapN {
      //   (sentence, verbIndex, goldParaphrases, paraphrasingFilter) =>
      //   val chosenFrame = frameset.frames.find(_.verbIds.contains(VerbId(sentence.sentenceId, verbIndex))).get
      //   val goldVerb = sentence.verbEntries(verbIndex)
      //   val results = Evaluation.getVerbResults(goldVerb, goldParaphrases.value, chosenFrame, paraphrasingFilter.value)
      //   import shapeless._
      //   import shapeless.syntax.singleton._
      //   import shapeless.record._
      //   val clauseBoundedAcc = results("clause paraphrasing accuracy").stats
      //   val paraphrasingBoundedAcc = results("question template paraphrasing accuracy (correct QAs)").stats

      //   <.div(S.clauseDecodingResultsDisplay)(
      //     <.span(S.clauseDecodingResultsText)(
      //       f"Clause accuracy: ${clauseBoundedAcc.accuracyLowerBound}%.2f / ${clauseBoundedAcc.accuracyUpperBound}%.2f"
      //     ),
      //     <.span(S.paraphraseDecodingResultsText)(
      //       f"Paraphrase accuracy: ${paraphrasingBoundedAcc.accuracyLowerBound}%.2f / ${paraphrasingBoundedAcc.accuracyUpperBound}%.2f"
      //     )
      //   )
      // }.whenDefined,
      <.div(S.frameSpecDisplay, S.scrollPane) {
        val frameList = frameset.frames.zipWithIndex
        println(s"12.2: ${frameList.size}")
        frameList.toVdomArray { case (frame, frameIndex) =>
          val isFrameChosen = {
            val bools = for {
              sentence <- sentenceOpt.toList
              verbIndex <- verbIndices.toList
            } yield frame.verbIds.contains(VerbId(sentence.sentenceId, verbIndex))
            bools.exists(identity)
          }
          val frameLens = VerbFrameset.frames
            .composeLens(unsafeListAt[VerbFrame](frameIndex))
          println(s"12.25: ${frame.questionClusterTree}")
          val roleClusters = paraphrasingFilter.value.questionCriterion.splitTree(frame.questionClusterTree)
          println(s"12.3: ${roleClusters.mkString("\n\n")}")
          // clause -> slot -> role -> qids
          val argMappings: Map[ArgStructure, Map[ArgumentSlot, Map[Int, Set[QuestionId]]]] = {
            roleClusters.zipWithIndex.foldMap { case (tree, roleIndex) =>
              tree.unorderedFoldMap { case qid @ QuestionId(_, clause, slot) =>
                val argStructure = ArgStructure(clause.args, clause.isPassive).forgetAnimacy
                Map(argStructure -> Map(slot -> Map(roleIndex -> Set(qid))))
              }
            }
          }
          println(s"12.4: ${argMappings}")
          val baseArgSigils = List("X", "Y", "Z", "A", "B", "C")
          val argSigils = baseArgSigils ++ baseArgSigils.map(_ + "2") ++ baseArgSigils.map(_ + "3")
          val getArgSigil = argSigils(_)
          // val argMappings: Map[ArgStructure, Map[ArgumentSlot, String]] = {
          //   val baseArgSigils = List("X", "Y", "Z", "A", "B", "C")
          //   val argSigils = baseArgSigils ++ baseArgSigils.map(_ + "2") ++ baseArgSigils.map(_ + "3")
          //   frame.coindexingTree
          //     .splitByPredicate(t => t.loss > (1.0 - paraphrasingFilter.value.minCoindexingProb))
          //     .filter(_.size > 1)
          //     .zip(argSigils)
          //     .foldMap { case (cluster, sigil) =>
          //       cluster.values.groupBy(_._1).map { case (argStructure, clausalQs) =>
          //         argStructure -> clausalQs.map(_._2 -> sigil).toMap
          //       }
          //     }
          // }

          val frameSentenceDocPairsOpt = (docIdToDocMetaOpt, sentenceOpt).mapN { (docIdToDocMeta, sentence) =>
            (frame.verbIds.map(vid => SentenceId.fromString(vid.sentenceId)).toSet + SentenceId.fromString(sentence.sentenceId)).toList
              .map(sid => sid -> docIdToDocMeta(sid.documentId))
              .sorted(
                Order.catsKernelOrderingForOrder(
                  Order.whenEqual[(SentenceId, DocumentMetadata)](
                    Order.by[(SentenceId, DocumentMetadata), String](_._2.title),
                    Order.by[(SentenceId, DocumentMetadata), SentenceId](_._1)
                  )
                )
              )
          }

          println(s"12.5: ${frameSentenceDocPairsOpt}")

          def makeNavQueryForSentenceIndexOpt(index: Int) = {
            frameSentenceDocPairsOpt.map { allSentencesForFrame =>
              val sid = allSentencesForFrame(index)._1
              val sidStr = SentenceId.toString(sid)
              val docIdStr = sid.documentId.toString
              DatasetQuery(verbInflectedForms.allForms.toSet, Set(docIdStr.lowerCase), Set(sidStr.lowerCase))
            }
          }
          val curSentenceIndexOpt = (frameSentenceDocPairsOpt, sentenceIdOpt).mapN { (frameSentenceDocPairs, sentenceId) =>
            frameSentenceDocPairs
              .zipWithIndex
              .find(t => t._1._1 == sentenceId)
              .map(_._2)
          }.flatten
          def makePrevQuery = (frameSentenceDocPairsOpt, curSentenceIndexOpt).mapN { (frameSentenceDocPairs, curSentenceIndex) =>
            makeNavQueryForSentenceIndexOpt(
              (curSentenceIndex - 1 + frameSentenceDocPairs.size) % frameSentenceDocPairs.size
            )
          }.flatten
          def makeNextQuery = (frameSentenceDocPairsOpt, curSentenceIndexOpt).mapN { (frameSentenceDocPairs, curSentenceIndex) =>
            makeNavQueryForSentenceIndexOpt(
              (curSentenceIndex + 1) % frameSentenceDocPairs.size
            )
          }.flatten
          println(s"A-$frameIndex")
          <.div(S.frameContainer, S.chosenFrameContainer.when(isFrameChosen))(
            ^.key := "clause-set-" + frameIndex.toString,
            <.div(S.frameHeading, S.chosenFrameHeading.when(isFrameChosen))(
              <.span(S.frameHeadingText)(
                f"Frame $frameIndex%s (${frame.probability}%.4f)"
              ),
              makePrevQuery.whenDefined(goToPrev =>
                <.span(S.prevFrameInstanceText)(
                  " (prev)",
                  ^.onClick --> navQuery.setState(goToPrev))
              ),
              makeNextQuery.whenDefined(goToNext =>
                <.span(S.prevFrameInstanceText)(
                  " (next)",
                  ^.onClick --> navQuery.setState(goToNext))
              )
            ),
            <.div(S.clauseSetDisplay)(
              frame.clauseTemplates.zipWithIndex
                .filter(p => isClauseProbabilityAcceptable(p._1.probability))
                .sortBy(-_._1.probability)
                .toVdomArray { case (frameClause, clauseIndex) =>
                  println(s"B-$frameIndex-$clauseIndex: $frameClause")
                  val surrogateFrame = makeSurrogateFrame(frameClause.args, verbInflectedForms, useModal = false)

                  <.div(S.clauseDisplay, S.matchingClause.when(predictedParaphraseClauseTemplatesOpt.exists(_.contains(frameClause.args))))(
                    ^.key := "clause-" + clauseIndex.toString,
                    goldParaphrasesOpt.whenDefined { goldParaphrases =>
                      val clauseCorrectLens = VerbParaphraseLabels.correctClauses.composeLens(Optics.at(frameClause.args))
                      val clauseIncorrectLens = VerbParaphraseLabels.incorrectClauses.composeLens(Optics.at(frameClause.args))
                      val clauseCorrectness = goldParaphrases.zoomStateL(sequenceLenses(clauseCorrectLens, clauseIncorrectLens))
                        <.div(S.goldClauseMarkerDisplay)(
                          <.label(S.goldClauseCheckLabel)(
                            <.input(
                              ^.`type` := "checkbox",
                              ^.value := clauseCorrectness.value._1,
                              ^.onChange ==> ((e: ReactEventFromInput) =>
                                if(clauseCorrectness.value._1) clauseCorrectness.setState(false -> false)
                                else clauseCorrectness.setState(true -> false)
                              )
                            ),
                            <.div(S.goldClauseCheck, S.goldClauseCheckCorrect.when(clauseCorrectness.value._1))
                          ),
                          <.label(S.goldClauseXLabel)(
                            <.input(
                              ^.`type` := "checkbox",
                              ^.value := clauseCorrectness.value._2,
                              ^.onChange ==> ((e: ReactEventFromInput) =>
                                if(clauseCorrectness.value._2) clauseCorrectness.setState(false -> false)
                                else clauseCorrectness.setState(false -> true)
                              )
                            ),
                            <.div(S.goldClauseX, S.goldClauseXIncorrect.when(clauseCorrectness.value._2))
                          )
                        )
                    },
                    <.span(S.shiftedClauseTemplateDisplay.when(goldParaphrasesOpt.nonEmpty))(
                      <.span(f"(${frameClause.probability}%.2f) "),
                      surrogateFrame.clausesWithArgMarkers.head.zipWithIndex.map {
                        case (Left(s), i) => <.span(^.key := s"frame-clause-$i", s)
                        case (Right(argSlot), i) => <.span(
                          ^.key := s"frame-clause-$i",
                          BoolLocal.make(initialValue = false) { isEditingSlot =>
                            val sigilSuffix = surrogateFrame.args.get(argSlot).get match {
                              case Noun(_) => ""
                              case Prep(p, _) =>
                                if(p.toString.contains("doing")) "[ng]"
                                else if(p.toString.contains(" do")) "[inf]"
                                else ""
                              case Locative => "[where]"
                            }
                            val genericGoldMatchingMod = S.genericGoldMatchingArgMarker.when(
                              goldStructureRelationOpt.exists(_.preimage(frameClause.args -> argSlot).nonEmpty)
                            )
                            val selectionMod = tagModForStructureLabel(
                              frameClause.args -> argSlot, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt
                            )
                            def getSigilSpan(roleIndex: Int, ids: Set[QuestionId]): VdomElement = {
                              val goldMatchingMod = S.goldMatchingArgMarker.when(
                                sentenceOpt.exists(sent => ids.exists(_.verbId.sentenceId == sent.sentenceId))
                              )

                              <.span(S.argSigil, genericGoldMatchingMod, goldMatchingMod, /* predMatchingMod, */selectionMod)(
                                getArgSigil(roleIndex) + sigilSuffix
                              )
                            }
                            argMappings.get(frameClause.args).flatMap(_.get(argSlot)).map { roleCounts =>
                              if(roleCounts.size == 1) {
                                val (roleIndex, ids) = roleCounts.head
                                getSigilSpan(roleIndex, ids)
                              } else {
                                val argSigils = roleCounts.toList.map(Function.tupled(getSigilSpan(_, _)))
                                <.span(
                                  "[",
                                  argSigils.map(Vector(_))
                                    .intercalate(Vector(<.span(" / ")))
                                    // .zipWithIndex
                                    // .map { case (x, i) => x(^.key := s"sigil-$i") }
                                    .toVdomArray,
                                  "]"
                                )
                              }
                            }.getOrElse {
                              <.span(S.argPlaceholder, genericGoldMatchingMod, /* predMatchingMod, */ selectionMod){
                                val prefix = surrogateFrame.args.get(argSlot) match {
                                  case Some(Prep(p, _)) if p.endsWith(" doing".lowerCase) => "doing "
                                  case Some(Prep(p, _)) if p == "do".lowerCase || p.endsWith(" do".lowerCase) => "do "
                                  case _ => ""
                                }
                                prefix + surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
                              }
                            }
                            // TODO integrate these into the common tag mod
                            // val predMatchingMod = S.predMatchingArgMarker.when(
                            //   predStructureRelationOpt.exists(_.preimage(frameClause.args -> argSlot).nonEmpty)
                            // )

                            // argMappings.get(frameClause.args).flatMap(_.get(argSlot)).map(s =>
                            //   <.span(S.argSigil, goldMatchingMod, /* predMatchingMod, */selectionMod)(s + sigilSuffix): VdomElement
                            // ).getOrElse(
                            //   <.span(S.argPlaceholder, goldMatchingMod, /* predMatchingMod, */ selectionMod){
                            //     val prefix = surrogateFrame.args.get(argSlot) match {
                            //       case Some(Prep(p, _)) if p.endsWith(" doing".lowerCase) => "doing "
                            //       case Some(Prep(p, _)) if p == "do".lowerCase || p.endsWith(" do".lowerCase) => "do "
                            //       case _ => ""
                            //     }
                            //     prefix + surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
                            //   }
                            // )
                          }
                        )
                      }.map(List(_)).intercalate(List(<.span(" "))).zipWithIndex.toVdomArray(p => p._1(^.key := s"frame-clause-tok-${p._2}"))
                    )
                  )
                }
            )
          )
        }
      },
      // <.div(S.frameAuditingDisplay, S.scrollPane) {
      //   def getInstancesForSentence(
      //     instances: Map[ArgumentSlot, NonEmptySet[QuestionId]]
      //   ) = instances.collect {
      //     case (argSlot, qids) if qids.exists(qid => sentenceIdOpt.exists(_ == qid.sentenceId)) =>
      //       argSlot -> qids.filter(qid => sentenceIdOpt.exists(_ == qid.sentenceId))
      //   }
      //   def getSpanForQid(qid: QuestionId): Option[String] = {
      //     sentenceOpt.flatMap(sentence =>
      //       sentence.verbEntries(qid.verbIndex).questionLabels(qid.questionString)
      //         .answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList)
      //         .groupBy(x => x).toList.sortBy(-_._2.size)
      //         .headOption.map(_._1).map(s => Text.renderSpan(sentence.sentenceTokens, s))
      //     )
      //   }
      //   def getSpanForQids(qids: List[QuestionId]): Option[String] = {
      //     qids.toList.flatMap(getSpanForQid).groupBy(x => x)
      //       .toList.sortBy(-_._2.size)
      //       .headOption.map(_._1)
      //   }
      //     <.div()
      //   // TODO auditing display once we have question connections
      //   // frameset.value.frame.zipWithIndex
      //   //   .filter(_._1.exists(fc => getInstancesForSentence(fc.instances).nonEmpty)).zipWithIndex
      //   //   .toVdomArray { case ((frames, frameIndex), index) =>
      //   //     val instancesForClauseSet = clauseSet.map(_.instances).map(getInstancesForSentence)
      //   //     val qidsForSigils = clauseSet.zip(instancesForClauseSet).foldMap { case (frameClause, instances) =>
      //   //       frameClause.argMapping.map { case (argSlot, sigil) =>
      //   //         sigil -> instances.get(argSlot).combineAll
      //   //       }.toMap
      //   //     }
      //   //     val spansForSigils = qidsForSigils.flatMap { case (sigil, qids) =>
      //   //       getSpanForQids(qids.toList).map(sigil -> _)
      //   //     }
      //   //     <.div(S.singleFrameAuditingDisplay)(
      //   //       ^.key := "frame-audit-" + index.toString,
      //   //       <.div(S.frameHeading)(
      //   //         <.span(S.frameHeadingText)(
      //   //           s"Frame $clauseSetIndex"
      //   //         )
      //   //       ),
      //   //       <.div(S.clauseSetDisplay)(
      //   //         <.ul(
      //   //           clauseSet.zip(instancesForClauseSet).zipWithIndex.toVdomArray { case ((frameClause, instances), index) =>
      //   //             val argSlots = instances.keySet ++ frameClause.argMapping.keySet
      //   //             val argMap = argSlots.toList.flatMap { argSlot =>
      //   //               frameClause.argMapping
      //   //                 .get(argSlot)
      //   //                 .flatMap(spansForSigils.get)
      //   //                 .orElse(instances.get(argSlot).map(_.toList).flatMap(getSpanForQids))
      //   //                 .map(argSlot -> _)
      //   //             }.toMap
      //   //             val surrogateFrame = Frame(frameClause.args, verbInflectedForms, mightTan)
      //   //               <.li(
      //   //                 ^.key := "audited-clause-set-" + index.toString,
      //   //                 <.span(
      //   //                   surrogateFrame.clausesWithArgMarkers.head.map {
      //   //                     case Left(s) => <.span(s)
      //   //                     case Right(argSlot) =>
      //   //                       argMap.get(argSlot).fold(
      //   //                         <.span(surrogateFrame.args.get(argSlot).get.placeholder.mkString(" "))
      //   //                       )(phrase => <.span(S.substitutedArgString)(phrase))
      //   //                   }.map(List(_)).intercalate(List(<.span(" ")))
      //   //                     .zipWithIndex.map(p => p._1(^.key := "audit-clause-token-" + p._2.toString)).toVdomArray
      //   //                 )
      //   //               )
      //   //           }
      //   //         )
      //   //       )
      //   //     )
      //   // }
      // }
      )
    }

  val GoldParaphrasesLocal = new LocalState[VerbParaphraseLabels]
  val ParaphrasingFilterLocal = new LocalState[ParaphrasingFilter]

  def zoomStateP[A, B](
    s: StateSnapshot[A],
    prism: Prism[A, B])(
    implicit ev: Reusability[B]
  ): Option[StateSnapshot[B]] = {
    prism.getOption(s.value).map { b =>
      StateSnapshot.withReuse.prepare[B]((bOpt, cb) => s.setStateOption(bOpt.map(prism.reverseGet), cb))(b)
    }
  }

  def zoomOpt[A](
    s: StateSnapshot[Option[A]])(
    implicit r: Reusability[A]
  ): Option[StateSnapshot[A]] = {
    s.value.map { a =>
      StateSnapshot.withReuse.prepare[A](s.setState)(a)
    }
  }

  val NavQueryLocal = new LocalState[NavQuery]

  val allPartitions = List[DatasetPartition](
    DatasetPartition.Train,
    DatasetPartition.Dev,
    DatasetPartition.Test)

  class Backend(scope: BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      def isPartitionIncluded(partition: DatasetPartition) = partition match {
        case DatasetPartition.Dev => props.mode != RunMode.Test
        case DatasetPartition.Train => props.mode != RunMode.Sanity
        case DatasetPartition.Test => props.mode == RunMode.Test
      }
      DataFetch.make(request = (), sendRequest = _ => props.docService.getDataIndex.product(OrWrapped.wrapped(props.verbService.getVerbs))) {
        case DataFetch.Loading => <.div(S.loadingNotice)("Waiting for verb data...")
        case DataFetch.Loaded((dataIndex, verbCounts)) =>
          val sortedVerbCounts = verbCounts.toList.sorted(
            Order.catsKernelOrderingForOrder(
              Order.whenEqual(
                Order.by[(InflectedForms, Int), Int](-_._2),
                Order.by[(InflectedForms, Int), String](p => inflToString(p._1))
              )
            )
          )
          ParaphrasingFilterLocal.make(initialValue = defaultParaphrasingFilter) { cachedParaphrasingFilter =>
            NavQueryLocal.make(initialValue = props.urlNavQuery) { navQuery =>
              println("1")
              navQuery.value match {
                case query @ DatasetQuery(_, _, _) =>
                  println("2")
                  val initVerb = sortedVerbCounts.map(_._1)
                    .find(query.matchesVerb)
                    .getOrElse(sortedVerbCounts.head._1)
                  VerbLocal.make(initialValue = initVerb) { curVerb =>
                    println("3")
                    val verbSpec = curVerb.value.allForms
                      .map(form => form -> sortedVerbCounts.filter(_._1.allForms.contains(form)).size)
                      .sortBy(_._2).map(_._1)
                      .foldLeft(verbCounts.keySet.map(_.allForms.toSet) -> Set.empty[LowerCaseString]) {
                        case ((remVerbs, spec), nextVerb) =>
                          if(remVerbs.size == 1) remVerbs -> spec
                          else remVerbs.filter(_.contains(nextVerb)) -> (spec + nextVerb)
                      }._2
                    val searchQuery = Search.Query(predicateOpt = Some(curVerb.value), keywords = Set())
                    VerbModelLocal.make(initialValue = None) { curModel =>
                      println("4")
                      val framesetsOpt = curModel.value.map(new LazyFramesets(_))
                      ParaphrasingFilterLocal.make(initialValue = cachedParaphrasingFilter.value) { paraphrasingFilter =>
                        println("5")
                        val curFramesetWithLoss = framesetsOpt.map(_.getFrameset(paraphrasingFilter.value.verbCriterion))
                        SearchFetch.make(request = searchQuery, sendRequest = props.docService.searchDocuments _) { searchFetchCtx =>
                          println("6")
                          val searchCtx = searchFetchCtx match {
                            case SearchFetch.Loading => None
                            case SearchFetch.Loaded(docIds) =>
                              val curDocMetas = dataIndex.allDocumentMetas.filter(meta => docIds.contains(meta.id) && isPartitionIncluded(meta.part))
                              val initDocMeta = curDocMetas
                                .find(query.matchesDoc)
                                .getOrElse(curDocMetas.head)
                              Some((docIds, curDocMetas, initDocMeta))
                          }
                          def docIdsOpt = searchCtx.map(_._1)
                          def curDocMetasOpt = searchCtx.map(_._2)
                          def initDocMetaOpt = searchCtx.map(_._3)
                          DocMetaOptLocal.make(initialValue = initDocMetaOpt) { curDocMetaOpt =>
                            println("7")
                            val docSpecOpt = (curDocMetasOpt, curDocMetaOpt.value).mapN { (curDocMetas, curDocMeta) =>
                              if(curDocMetas.size == 1) Set[LowerCaseString]()
                              else {
                                val docsLowerTitles = curDocMetas.map(_.title.lowerCase).toSet
                                curDocMeta.title.split(" ").map(_.lowerCase)
                                  .map(token => token -> docsLowerTitles.filter(_.contains(token)).size)
                                  .sortBy(_._2).map(_._1)
                                  .foldLeft(docsLowerTitles -> Set.empty[LowerCaseString]) {
                                    case ((remTitles, spec), nextToken) =>
                                      if(remTitles.size == 1) remTitles -> spec
                                      else remTitles.filter(_.contains(nextToken)) -> (spec + nextToken)
                                  }._2
                              }
                            }
                            DocOptFetch.make(
                              request = curDocMetaOpt.value.map(_.id),
                              sendRequest = idOpt => idOpt.map(props.docService.getDocument).sequence) { docFetchCtx =>
                              println("8")
                              val docCtx = docFetchCtx match {
                                case DocOptFetch.Loading => None
                                case DocOptFetch.Loaded(None) => None
                                case DocOptFetch.Loaded(Some(doc)) =>
                                  val curSentences = getCurSentences(doc.sentences, searchQuery)
                                  val initSentence = curSentences
                                    .find(query.matchesSentence)
                                    .getOrElse(curSentences.head)
                                  Some((doc, curSentences, initSentence))
                              }
                              def curDocOpt = docCtx.map(_._1)
                              def curSentencesOpt = docCtx.map(_._2)
                              def initSentenceOpt = docCtx.map(_._3)
                              SentOptLocal.make(initialValue = initSentenceOpt) { curSentenceOpt =>
                                println("9")
                                BoolLocal.make(initialValue = false) { showInvalidQuestions =>
                                  println("10")
                                  val sentSpecOpt = (curSentencesOpt, curSentenceOpt.value).mapN { (curSentences, curSentence) =>
                                    if(curSentences.size == 1) Set[LowerCaseString]()
                                    else {
                                      val sentencesLowerTokens = curSentences.map(_.sentenceTokens.map(_.lowerCase).toSet)
                                      curSentence.sentenceTokens.map(_.lowerCase)
                                        .map(token => token -> sentencesLowerTokens.filter(_.contains(token)).size)
                                        .sortBy(_._2).map(_._1)
                                        .foldLeft(sentencesLowerTokens -> Set.empty[LowerCaseString]) {
                                          case ((remSents, spec), nextToken) =>
                                            if(remSents.size == 1) remSents -> spec
                                            else remSents.filter(_.contains(nextToken)) -> (spec + nextToken)
                                        }._2
                                    }
                                  }
                                  val currentVerbIndicesOpt = curSentenceOpt.value.map(
                                    _.verbEntries.values
                                      .filter(v => curModel.value.exists(_.verbInflectedForms == v.verbInflectedForms))
                                      .map(_.verbIndex).toSet
                                  )
                                  val goldStructureRelationOpt = curSentenceOpt.value.map(sentence =>
                                    currentVerbIndicesOpt.map(_.toList).getOrElse(Nil).map(sentence.verbEntries).foldMap { verb =>
                                      val makeQid = makeQidMap(sentence.sentenceId, verb)
                                      verb.questionLabels.values.toList
                                        .foldMap(qLabel =>
                                          ClauseResolution.getFramesWithAnswerSlots(qLabel.questionSlots).map { case (frame, slot) =>
                                            ClauseResolution.getClauseTemplate(frame) -> slot
                                          }.unorderedFoldMap(x =>
                                            FiniteRelation.single(makeQid(qLabel.questionString), x)
                                          )
                                        )
                                    }
                                  )
                                  println("11")

                                  <.div(S.mainContainer)(
                                    <.div(S.headerContainer)(
                                      <.select(S.verbDropdown)(
                                        ^.value := inflToString(curVerb.value),
                                        ^.onChange ==> ((e: ReactEventFromInput) =>
                                          curVerb.setState(inflFromString(e.target.value))
                                        ),
                                        sortedVerbCounts.toVdomArray { case (forms, count) =>
                                          <.option(
                                            ^.key := inflToString(forms),
                                            ^.value := inflToString(forms),
                                            f"$count%5d ${forms.allForms.mkString(", ")}%s"
                                          )
                                        }
                                      ),
                                      <.div(S.goDisplay)(
                                        <.span(S.goLabelText)("Go: "),
                                        StringLocal.make(initialValue = "") { goQueryString =>
                                          <.input(S.goTextField)(
                                            ^.`type` := "text",
                                            ^.value := goQueryString.value,
                                            ^.onChange ==> ((e: ReactEventFromInput) => goQueryString.setState(e.target.value)),
                                            ^.onKeyDown ==> ((e: ReactKeyboardEventFromInput) =>
                                              CallbackOption.keyCodeSwitch(e) {
                                                case KeyCode.Enter =>
                                                  navQuery.setState(NavQuery.fromString(goQueryString.value)) >>
                                                    goQueryString.setState("")
                                              }
                                            )
                                          )
                                        }
                                      ),
                                      (docSpecOpt, sentSpecOpt).mapN { (docSpec, sentSpec) =>
                                        def qStr(s: Set[LowerCaseString]) = s.toList.sorted.mkString(",")
                                        val linkPath = s"/${qStr(verbSpec)}/${qStr(docSpec)}/${qStr(sentSpec)}"
                                          .reverse.dropWhile(_ == '/').reverse
                                        Mounting.make(
                                          Callback(
                                            dom.window.history.pushState(
                                              "", "", linkPath
                                            )))(
                                          <.span(S.sentenceLink)("Link: ", <.a(^.href := linkPath, linkPath.tail))
                                        )
                                      }.whenDefined,
                                      checkboxToggle("Show invalid questions", showInvalidQuestions)
                                    ),
                                    ArgStructureOptLocal.make(initialValue = None) { argStructureChoiceOpt =>
                                      ArgStructureOptLocal.make(initialValue = None) { argStructureHoverOpt =>
                                        <.div(S.dataContainer)(
                                          curFramesetWithLoss.filter(_._2.inflectedForms == curVerb.value) match {
                                            case None =>
                                              Mounting.make(
                                                props.verbService.getModel(curVerb.value)
                                                  .flatMap(m => curModel.setState(Some(m)).asAsyncCallback)
                                                  .toCallback
                                              )(<.div(S.loadingNotice)("Loading frameset..."))
                                            case Some((maxLoss, frameset)) =>
                                              println("12")
                                              val res = frameDisplayPane(
                                                dataIndex, curVerb.value, curDocMetasOpt, curSentenceOpt.value, currentVerbIndicesOpt.getOrElse(Set()),
                                                goldStructureRelationOpt, frameset, maxLoss, None, None, cachedParaphrasingFilter, paraphrasingFilter, argStructureChoiceOpt, argStructureHoverOpt, navQuery
                                              )
                                              println("13")
                                              res

                                          },
                                          (searchCtx, zoomOpt(curDocMetaOpt)(Reusability.by_==[DocumentMetadata])) match {
                                            case (None, _) | (_, None) =>
                                              print("14a")
                                              <.div(S.loadingNotice)("Loading document list...")
                                            case (Some((_, docMetas, _)), Some(curDocMeta)) =>
                                              print("14b")
                                              <.div(S.dataContainer)(
                                                docSelectionPane(
                                                  allPartitions.filter(isPartitionIncluded).foldMap(p => dataIndex.documents(p).size),
                                                  docMetas,
                                                  curDocMeta
                                                ),
                                                (docCtx, zoomOpt(curSentenceOpt)(Reusability.by_==[Sentence])) match {
                                                  case (None, _) | (_, None) =>
                                                    print("15a")
                                                    <.div(S.loadingNotice)("Loading document...")
                                                  case (Some((doc, curSentences, _)), Some(curSentence)) =>
                                                    print("15b")
                                                    <.div(S.documentContainer)(
                                                      sentenceSelectionPane(
                                                        doc.sentences.size,
                                                        curSentences,
                                                        searchQuery,
                                                        curSentence
                                                      ),
                                                      sentenceDisplayPane(
                                                        dataIndex.getPart(curDocMeta.value.id),
                                                        curDocMeta.value,
                                                        curSentence.value,
                                                        goldStructureRelationOpt.get, // will be present bc sentence is present
                                                        curVerb.value,
                                                        None,
                                                        None,
                                                        None,
                                                        argStructureChoiceOpt,
                                                        argStructureHoverOpt,
                                                        navQuery,
                                                        showInvalidQuestions.value
                                                      )
                                                    )
                                                }
                                              )
                                          }
                                        )
                                      }
                                    }
                                  )
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                case EvalQuery(evalItemIndex) =>
                  EvalItemFetch.make(request = evalItemIndex, sendRequest = i => OrWrapped.wrapped(props.verbService.getParaphrasingInfo(i))) {
                    case EvalItemFetch.Loading => <.div(S.loadingNotice)("Loading evaluation data...")
                    case EvalItemFetch.Loaded(
                      ParaphrasingInfo(sentenceId, verbIndex, goldVerb, verbModel, initGoldParaphrases)
                    ) =>
                      val framesets = new LazyFramesets(verbModel)
                      val docMeta = dataIndex.allDocumentMetas.find(_.id == SentenceId.fromString(sentenceId).documentId).get
                      DocFetch.make(request = docMeta.id, sendRequest = props.docService.getDocument) {
                        case DocFetch.Loading => <.div(S.loadingNotice)("Loading document...")
                        case DocFetch.Loaded(document) =>
                          val sentence = document.sentences.find(_.sentenceId == sentenceId).get
                          val verb = verbModel.verbInflectedForms
                          val verbCount = verbCounts(verb)
                          val linkPath = s"/$evalItemIndex"
                          val makeQid = makeQidMap(sentenceId, goldVerb)
                          val goldValidQuestionLabels = filterGoldNonDense(filterOrigAnnotationRound(sentence.verbEntries(verbIndex)))._2
                          val goldStructureRelation = goldValidQuestionLabels.toList.foldMap {
                            case (qString, qLabel) =>
                              ClauseResolution.getFramesWithAnswerSlots(verb, qLabel.questionSlots).map { case (frame, slot) =>
                                ClauseResolution.getClauseTemplate(frame) -> slot
                              }.unorderedFoldMap(x => FiniteRelation.single(makeQid(qLabel.questionString), x))
                          }
                          GoldParaphrasesLocal.make(initialValue = initGoldParaphrases) { curGoldParaphrases =>
                            val syncedGoldParaphrases = StateSnapshot.withReuse.prepare[VerbParaphraseLabels](
                              (vpOpt, cb) => vpOpt.fold(cb)(vp =>
                                curGoldParaphrases.setState(vp, cb) >>
                                  props.verbService.saveParaphraseAnnotations(sentenceId, verbIndex, vp)
                                  .flatMap(newVP => curGoldParaphrases.setState(newVP, cb).asAsyncCallback)
                                  .toCallback
                              )
                            )(curGoldParaphrases.value)(Reusability.by_==[VerbParaphraseLabels])

                            ParaphrasingFilterLocal.make(initialValue = cachedParaphrasingFilter.value) { paraphrasingFilter =>
                              val (maxLoss, verbFrameset) = framesets.getFrameset(paraphrasingFilter.value.verbCriterion)
                              val roleClusterSets = verbFrameset.frames.map { frame =>
                                paraphrasingFilter.value.questionCriterion.splitTree(frame.questionClusterTree)
                              }
                              val (chosenFrame, chosenFrameIndex) = verbFrameset.frames
                                .zipWithIndex.find(_._1.verbIds.contains(VerbId(sentenceId, verbIndex))).get
                              val predictedParaphrases = goldValidQuestionLabels.keySet.map(makeQid).map(qid =>
                                qid -> paraphrasingFilter.value.getParaphrases(
                                  chosenFrame, qid, roleClusterSets(chosenFrameIndex).find(_.values.contains(qid)).get
                                )
                              ).toMap
                              BoolLocal.make(initialValue = false) { showInvalidQuestions =>
                                <.div(S.mainContainer)(
                                  <.div(S.headerContainer)(
                                    <.span(S.verbInflectionsDisplay)(
                                      f"$verbCount%5d ${verb.allForms.mkString(", ")}%s"
                                    ),
                                    <.button(
                                      "prev",
                                      ^.onClick --> navQuery.setState(EvalQuery(evalItemIndex - 1)),
                                      ^.disabled := evalItemIndex == 0
                                    ),
                                    <.button(
                                      "next",
                                      ^.onClick --> navQuery.setState(EvalQuery(evalItemIndex + 1)),
                                      ),
                                    <.div(S.goDisplay)(
                                      <.span(S.goLabelText)("Go: "),
                                      StringLocal.make(initialValue = evalItemIndex.toString) { goQueryString =>
                                        <.input(S.goTextField)(
                                          ^.`type` := "text",
                                          ^.value := goQueryString.value,
                                          ^.onChange ==> ((e: ReactEventFromInput) => goQueryString.setState(e.target.value)),
                                          ^.onKeyDown ==> ((e: ReactKeyboardEventFromInput) =>
                                            CallbackOption.keyCodeSwitch(e) {
                                              case KeyCode.Enter =>
                                                navQuery.setState(NavQuery.fromString(goQueryString.value)) >>
                                                  goQueryString.setState("")
                                            }
                                          )
                                        )
                                      }
                                    ),
                                    Mounting.make(
                                      Callback(
                                        dom.window.history.pushState(
                                          "", "", linkPath
                                        )))(
                                      <.span(S.sentenceLink)("Link: ", <.a(^.href := linkPath, linkPath.tail))
                                    ),
                                    checkboxToggle("Show invalid questions", showInvalidQuestions)
                                  ),
                                  ArgStructureOptLocal.make(initialValue = None) { argStructureChoiceOpt =>
                                    ArgStructureOptLocal.make(initialValue = None) { argStructureHoverOpt =>
                                      <.div(S.dataContainer)(
                                        frameDisplayPane(
                                          dataIndex, verbFrameset.inflectedForms,
                                          None,
                                          Some(sentence),
                                          Set(verbIndex),
                                          Some(goldStructureRelation),
                                          verbFrameset,
                                          maxLoss,
                                          Some(syncedGoldParaphrases),
                                          Some(predictedParaphrases),
                                          cachedParaphrasingFilter,
                                          paraphrasingFilter,
                                          argStructureChoiceOpt,
                                          argStructureHoverOpt,
                                          navQuery
                                        ),
                                        <.div(S.dataContainer){
                                          sentenceDisplayPane(
                                            dataIndex.getPart(docMeta.id),
                                            docMeta,
                                            sentence,
                                            goldStructureRelation,
                                            verbFrameset.inflectedForms,
                                            Some(verbIndex),
                                            Some(syncedGoldParaphrases),
                                            Some(predictedParaphrases),
                                            argStructureChoiceOpt,
                                            argStructureHoverOpt,
                                            navQuery,
                                            showInvalidQuestions.value
                                          )
                                        }
                                      )
                                    }
                                  }
                                )
                              }
                            }
                          }
                      }
                  }
              }
            }
          }
      }
    }
  }

  val Component = ScalaComponent.builder[Props]("VerbAnnClient")
    .initialState(State.initial)
    .renderBackend[Backend]
    .build

}
