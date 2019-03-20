package qfirst.frames.verbal
import qfirst.frames._

import cats.Id
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
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.Reusability

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

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
import qasrl.bank.service.Search

import qasrl.data.AnswerLabel
import qasrl.data.AnswerJudgment
import qasrl.data.AnswerSpan
import qasrl.data.Answer
import qasrl.data.InvalidQuestion
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

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
  def toColorStyleString = f"rgba(${math.round(r)}%d, ${math.round(g)}%d, ${math.round(b)}%d, $a%.4f)"
}

case class NavQuery(
  verbFormsMatch: Set[LowerCaseString],
  docMatch: Set[LowerCaseString],
  sentenceMatch: Set[LowerCaseString]) {
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
object NavQuery {
  def empty = NavQuery(Set(), Set(), Set())
  def fromString(path: String) = {
    val verbMatch = Option(path).filter(_.nonEmpty).foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    val verbRemainderOpt = Option(path.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
    val docMatch = verbRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    val docRemainderOpt = verbRemainderOpt.map(_.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
    val sentMatch = docRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    NavQuery(verbMatch, docMatch, sentMatch)
  }
}

object VerbAnnUI {

  val S = VerbAnnStyles

  val DataFetch = new CacheCallContent[Unit, (DataIndex, Map[InflectedForms, Int])]
  val DocFetch = new CacheCallContent[Option[DocumentId], Option[Document]]
  val SearchFetch = new CacheCallContent[Search.Query, Set[DocumentId]]
  val FrameFetch = new CacheCallContent[InflectedForms, VerbFrame]
  val VerbLocal = new LocalState[InflectedForms]
  val FrameLocal = new LocalState[Option[VerbFrame]]
  val DocMetaOptLocal = new LocalState[Option[DocumentMetadata]]
  val SentOptLocal = new LocalState[Option[Sentence]]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]
  val IntSetLocal = new LocalState[Set[Int]]
  val FrameChoiceLocal = new LocalState[Set[(QuestionId, ArgStructure, ArgumentSlot)]]
  val QuestionSetLocal = new LocalState[Set[QuestionId]]
  val BoolLocal = new LocalState[Boolean]
  val StringLocal = new LocalState[String]

  val (inflToString, inflFromString) = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    import io.circe.syntax._
    import io.circe.parser.decode
    val printer = io.circe.Printer.noSpaces
    val toStr = (f: InflectedForms) => printer.pretty(f.asJson)
    val fromStr = (s: String) => decode[InflectedForms](s).right.get
    (toStr, fromStr)
  }

  case class Props(
    docService: DocumentService[CacheCall],
    verbService: VerbAnnotationService[Future],
    urlNavQuery: NavQuery
  )

  case class State()
  object State {
    val initial = State()
  }

  def checkboxToggle(
    label: String,
    isValueActive: StateSnapshot[Boolean]
  ) = <.div(
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
    val qSource = label.questionSources.map(s => QuestionSource.fromString(s): QuestionSource).min
    qSource match {
      case QuestionSource.Turker(_) => AnnotationRound.Original
      case QuestionSource.Model(_)  =>
        val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
          AnswerSource.fromString(s).round == AnnotationRound.Expansion
        )
        if(hasAnswersInExpansion) AnnotationRound.Expansion else AnnotationRound.Eval
    }
  }

  import cats.Order.catsKernelOrderingForOrder

  implicit val answerSpanOrder: Order[AnswerSpan] = Order.whenEqual(
    Order.by[AnswerSpan, Int](_.begin),
    Order.by[AnswerSpan, Int](_.end)
  )
  implicit val qasrlDataQuestionLabelOrder: Order[QuestionLabel] = Order.whenEqual(
    Order.by[QuestionLabel, AnnotationRound](getRoundForQuestion _),
    Order.by[QuestionLabel, String](_.questionString)
  )

  def spanOverlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
    x.begin < y.end && y.begin < x.end
  }
  def spanContains(s: AnswerSpan, q: Int): Boolean = {
    q >= s.begin && q < s.end
  }

  sealed trait SpanColoringSpec {
    def spansWithColors: List[(AnswerSpan, Rgba)]
  }
  case class RenderWholeSentence(val spansWithColors: List[(AnswerSpan, Rgba)]) extends SpanColoringSpec
  case class RenderRelevantPortion(spansWithColorsNel: NonEmptyList[(AnswerSpan, Rgba)]) extends SpanColoringSpec {
    def spansWithColors = spansWithColorsNel.toList
  }

  def renderSentenceWithHighlights(
    sentenceTokens: Vector[String],
    coloringSpec: SpanColoringSpec,
    wordRenderers : Map[Int, VdomTag => VdomTag] = Map()
  ) = {
    val containingSpan = coloringSpec match {
      case RenderWholeSentence(_) =>
        AnswerSpan(0, sentenceTokens.size)
      case RenderRelevantPortion(swcNel) =>
        val spans = swcNel.map(_._1)
        AnswerSpan(spans.map(_.begin).minimum, spans.map(_.end).maximum)
    }
    val wordIndexToLayeredColors = (containingSpan.begin until containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if spanContains(span, i) => color
      }
    }.toMap
    val indexAfterToSpaceLayeredColors = ((containingSpan.begin + 1) to containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if spanContains(span, i - 1) && spanContains(span, i) => color
      }
    }.toMap
    Text.render[Int, List, List[VdomElement]](
      words = sentenceTokens.indices.toList,
      getToken = (index: Int) => sentenceTokens(index),
      spaceFromNextWord = (nextIndex: Int) => {
        if(!spanContains(containingSpan, nextIndex) || nextIndex == containingSpan.begin) List() else {
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
        if(!spanContains(containingSpan, index)) List() else {
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
      completeGroups: List[NonEmptyList[AnswerSpan]],
      currentGroup: NonEmptyList[AnswerSpan]
    )
    val groupingState = orderedSpans.tail.foldLeft(GroupingState(Nil, NonEmptyList.of(orderedSpans.head))) {
      case (GroupingState(groups, curGroup), span) =>
        if(curGroup.exists(s => spanOverlaps(s, span))) {
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
    val includedJudgments = label.answerJudgments
    val numValidJudgments = includedJudgments.count(_.judgment.isAnswer)
    numValidJudgments.toDouble / includedJudgments.size > (4.99 / 6.0)
  }

  val colspan = VdomAttr("colspan")

  def qaLabelRow(
    sentence: Sentence,
    label: QuestionLabel,
    color: Rgba,
    qid: QuestionId,
    resolutions: Set[(Frame, ArgumentSlot)],
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]],
    hoveredQids: StateSnapshot[Set[QuestionId]],
    canDeleteCoveringInstance: Option[Callback]
  ) = {
    val answerJudgments = label.answerJudgments
    val qSource = label.questionSources.map(s => QuestionSource.fromString(s): QuestionSource).min
    val roundIndicatorStyle = qSource match {
      case QuestionSource.Turker(_) => S.originalRoundIndicator
      case QuestionSource.Model(_)  =>
        val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
          AnswerSource.fromString(s).round == AnnotationRound.Expansion
        )
        if(hasAnswersInExpansion) S.expansionRoundIndicator else S.evalRoundIndicator
    }
    val questionChoiceSet = resolutions.map { case (frame, argSlot) =>
      (qid, frame.structure.forgetAnimacy, argSlot)
    }.toSet
    val isNotChosen = frameChoices.value.forall { case (chosenQid, chosenArgs, argSlot) =>
      chosenQid != qid
    }

    <.tr(S.qaPairRow)(
      <.td(roundIndicatorStyle),
      <.td(S.questionCell,
           if(!isNotChosen) S.candidateClauseChoice
           else if(hoveredQids.value.contains(qid) && canDeleteCoveringInstance.nonEmpty) S.hoveredQuestion
           else S.coveredQuestion.when(canDeleteCoveringInstance.nonEmpty))(
        ^.onClick --> (
          canDeleteCoveringInstance.getOrElse {
            if(isNotChosen) frameChoices.setState(questionChoiceSet)
            else frameChoices.setState(Set())
          }
        ),
        ^.onMouseMove --> (
          if(hoveredQids.value != Set(qid)) hoveredQids.setState(Set(qid))
          else Callback.empty
        ),
        ^.onMouseOut --> hoveredQids.setState(Set()),
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

  def verbEntryDisplay(
    curSentence: Sentence,
    syncedFrameOpt: Option[StateSnapshot[VerbFrame]],
    verb: VerbEntry,
    color: Rgba,
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]],
    hoveredQids: StateSnapshot[Set[QuestionId]],
    navQuery: StateSnapshot[NavQuery],
    displayQAs: Boolean
  ) = {
    val coveredQids = syncedFrameOpt
      .foldMap(_.value.clauseSets.foldMap(_.foldMap(_.instances.values.toList.foldMap(_.toSortedSet.toSet))))
    val qaTable = if(displayQAs) Some {
      <.table(S.verbQAsTable)(
          <.tbody(S.verbQAsTableBody) {
            val questionLabels = verb.questionLabels.toList.map(_._2).sorted
            if(questionLabels.isEmpty) {
              <.tr(<.td(<.span((S.loadingNotice)("All questions have been filtered out."))))
            } else questionLabels.toVdomArray { label =>
              val qid = QuestionId(SentenceId.fromString(curSentence.sentenceId), verb.verbIndex, label.questionString)
              val resolutions = getFramesForQid(verb.verbInflectedForms, qid)
              val canDeleteCoveringInstance = {
                val deletionFunctions = for {
                  frame <- syncedFrameOpt.toList
                  (clauseSet, clauseSetIndex) <- frame.value.clauseSets.zipWithIndex
                  (frameClause, frameClauseIndex) <- clauseSet.zipWithIndex
                  (argSlot, qids) <- frameClause.instances.toList
                  if qids.contains(qid)
                } yield {
                  VerbFrame.clauseSets
                    .composeLens(unsafeListAt(clauseSetIndex))
                    .composeLens(unsafeListAt(frameClauseIndex))
                    .composeLens(FrameClause.instances)
                    .composeLens(Optics.at(argSlot))
                    .composeIso(nonEmptySetOptionIso[QuestionId])
                    .modify(_ - qid)
                }
                if(deletionFunctions.isEmpty) None
                else syncedFrameOpt.map(_.modState(deletionFunctions.reduce(_ compose _)))
              }
              val hasCoveringInstance =
                syncedFrameOpt
                coveredQids.contains(qid)
              qaLabelRow(curSentence, label, color, qid, resolutions, frameChoices, hoveredQids, canDeleteCoveringInstance)(^.key := s"short-${label.questionString}"),
            }
          }
      )
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
              NavQuery(
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
    )
  }

  def docSelectionPane(
    totalNumDocs: Int,
    curDocMetas: SortedSet[DocumentMetadata],
    labeledDocuments: Set[DocumentId],
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
          <.div(S.documentSelectionEntry, S.labeledDocumentSelectionEntry.when(labeledDocuments.contains(docMeta.id)))(
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
    labeledSentences: Set[SentenceId],
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
            AnswerSpan(index, index + 1) -> queryKeywordHighlightLayer
          )
          <.div(S.sentenceSelectionEntry, S.labeledSentenceSelectionEntry.when(labeledSentences.contains(curSentenceId)))(
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


  var qidToFramesCache = Map.empty[QuestionId, Set[(Frame, ArgumentSlot)]]
  def getFramesForQid(verbForms: InflectedForms, qid: QuestionId) = {
    qidToFramesCache.get(qid).getOrElse {
      val questionTokensIsh = qid.questionString.init.split(" ").toVector
      val stateMachine = new TemplateStateMachine(questionTokensIsh, verbForms)
      val template = new QuestionProcessor(stateMachine)
      val results = template.processStringFully(qid.questionString).right.toOption.map { goodStates =>
        val completeStates = goodStates.map(s =>
          QuestionProcessor.ValidState.eitherIso.get(s).right.get
        )
        val framesWithAnswer = completeStates.map(s =>
          s.frame -> s.answerSlot
        ).toList.toSet
        framesWithAnswer
      }.get
      qidToFramesCache = qidToFramesCache + (qid -> results)
      results
    }
  }

  def sentenceDisplayPane(
    part: DatasetPartition,
    docMeta: DocumentMetadata,
    sentence: Sentence,
    verbForms: InflectedForms,
    syncedFrameOpt: Option[StateSnapshot[VerbFrame]],
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]],
    hoveredQids: StateSnapshot[Set[QuestionId]],
    navQuery: StateSnapshot[NavQuery]
  ) = {
    val sentenceId = SentenceId.fromString(sentence.sentenceId)
    val (targetedVerbs, untargetedVerbs) = sentence.verbEntries.values
      .toList.partition(_.verbInflectedForms == verbForms)
    val searchedVerbIndices = targetedVerbs.map(_.verbIndex).toSet
    val sortedVerbs = targetedVerbs.sortBy(_.verbIndex) ++ untargetedVerbs.sortBy(_.verbIndex)
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
            verbEntryDisplay(sentence, syncedFrameOpt, verb, verbColorMap(verb.verbIndex), frameChoices, hoveredQids, navQuery, displayQAs = verb.verbInflectedForms == verbForms)(
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

  def frameDisplayPane(
    dataIndex: DataIndex,
    verbInflectedForms: InflectedForms,
    curDocMetasOpt: Option[Set[DocumentMetadata]],
    sentenceOpt: Option[Sentence],
    frame: StateSnapshot[VerbFrame],
    clauseOptions: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]],
    hoveredQids: StateSnapshot[Set[QuestionId]],
    navQuery: StateSnapshot[NavQuery]
  ) = {
    val sentenceIdOpt = sentenceOpt.map(s => SentenceId.fromString(s.sentenceId))
    val presentTan = TAN(qasrl.PresentTense, false, false, false)
    val mightTan = TAN(qasrl.Modal("might".lowerCase), false, false, false)
    val docIdToDocMetaOpt = curDocMetasOpt.map(_.map(m => m.id -> m).toMap)
    <.div(S.frameContainer)(
      <.div(S.frameSpecDisplay, S.scrollPane)(
        frame.value.clauseSets.zipWithIndex.toVdomArray { case (clauseSet, clauseSetIndex) =>
          val clauseSetLens = VerbFrame.clauseSets
            .composeLens(unsafeListAt[List[FrameClause]](clauseSetIndex))
          val newClauseOptions = clauseOptions.value.filter(p => !clauseSet.exists(_.args == p._2))
          def questionIsValid(question: QuestionLabel) = {
            val numValids = question.answerJudgments.filter(_.judgment.isAnswer).size
            val numTotal = question.answerJudgments.size
            (numValids.toDouble / numTotal) >= 5.0/6
          }
          val validLinkableQuestionsOpt = sentenceOpt.map { sentence =>
            val res = for {
              verb <- sentence.verbEntries.values.toList
              if verb.verbInflectedForms == verbInflectedForms
              question <- verb.questionLabels.values.toList
              if questionIsValid(question)
              qid = QuestionId(SentenceId.fromString(sentence.sentenceId), verb.verbIndex, question.questionString)
              (frame, argSlot) <- getFramesForQid(verb.verbInflectedForms, qid).toList
            } yield {
              Map(
                frame.structure.forgetAnimacy -> Map(
                  argSlot -> NonEmptySet.of(qid)
                )
              )
            }
            res.combineAll
          }
          val allSentencesForClauseSetOpt = (docIdToDocMetaOpt, sentenceOpt).mapN { (docIdToDocMeta, sentence) =>
            (clauseSet.foldMap { frameClause =>
               frameClause.instances.values.toList.foldMap(_.map(_.sentenceId).toList.toSet)
             } + SentenceId.fromString(sentence.sentenceId))
              .toList
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
          def makeNavQueryForSentenceIndexOpt(index: Int) = {
            allSentencesForClauseSetOpt.map { allSentencesForClauseSet =>
              val sid = allSentencesForClauseSet(index)._1
              val sidStr = SentenceId.toString(sid)
              val docIdStr = sid.documentId.toString
              NavQuery(verbInflectedForms.allForms.toSet, Set(docIdStr.lowerCase), Set(sidStr.lowerCase))
            }
          }
          val curSentenceIndexOpt = (allSentencesForClauseSetOpt, sentenceIdOpt).mapN { (allSentencesForClauseSet, sentenceId) =>
            allSentencesForClauseSet
              .zipWithIndex
              .find(t => t._1._1 == sentenceId)
              .map(_._2)
          }.flatten
          def makePrevQuery = (allSentencesForClauseSetOpt, curSentenceIndexOpt).mapN { (allSentencesForClauseSet, curSentenceIndex) =>
            makeNavQueryForSentenceIndexOpt(
              (curSentenceIndex - 1 + allSentencesForClauseSet.size) % allSentencesForClauseSet.size
            )
          }.flatten
          def makeNextQuery = (allSentencesForClauseSetOpt, curSentenceIndexOpt).mapN { (allSentencesForClauseSet, curSentenceIndex) =>
            makeNavQueryForSentenceIndexOpt(
              (curSentenceIndex + 1) % allSentencesForClauseSet.size
            )
          }.flatten

          <.div(
            ^.key := "clause-set-" + clauseSetIndex.toString,
            <.div(S.frameHeading)(
              <.span(S.frameHeadingText)(
                s"Frame $clauseSetIndex "
              ),
              <.span(S.frameDeleteText)(
                "(delete)",
                ^.onClick --> frame.zoomStateL(VerbFrame.clauseSets).modState(_.patch(clauseSetIndex, Nil, 1))
              ),
              validLinkableQuestionsOpt.whenDefined(validLinkableQuestions =>
                <.span(S.frameLinkAllValidText)(
                  " (link all valid)",
                  ^.onClick --> frame.zoomStateL(clauseSetLens).modState(
                    _.map(frameClause =>
                      validLinkableQuestions.get(frameClause.args).fold(frameClause) { newArgs =>
                        FrameClause.instances.modify(_ |+| newArgs)(frameClause)
                      }
                    )
                  )
                )
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
              clauseSet.zipWithIndex.toVdomArray { case (frameClause, clauseIndex) =>
                val qidsInSentence = sentenceIdOpt.foldMap(sentenceId =>
                  frameClause.instances.toList.foldMap(_._2.filter(_.sentenceId == sentenceId).toSet)
                )
                val validInstance = clauseOptions.value.find(_._2 == frameClause.args)
                val frameClauseLens = clauseSetLens
                  .composeLens(unsafeListAt[FrameClause](clauseIndex))
                val instancesLens = frameClauseLens
                  .composeLens(FrameClause.instances)
                val surrogateFrame = Frame(frameClause.args, verbInflectedForms, presentTan)

                <.div(
                  if(validInstance.nonEmpty) S.candidateClauseChoice
                  else if(hoveredQids.value.intersect(qidsInSentence).nonEmpty) S.hoveredClause
                  else S.coveredClause.when(qidsInSentence.nonEmpty)
                )(
                  ^.key := "clause-" + clauseIndex.toString,
                  <.span(S.clauseDeleteText)(
                    "(-)",
                    ^.onClick --> frame.zoomStateL(clauseSetLens).modState(_.patch(clauseIndex, Nil, 1))
                  ),
                  <.span(
                    ^.onMouseMove --> (
                      if(hoveredQids.value != qidsInSentence) hoveredQids.setState(qidsInSentence)
                      else Callback.empty
                    ),
                    ^.onMouseOut --> hoveredQids.setState(Set()),
                    <.span(" "),
                    surrogateFrame.clausesWithArgMarkers.head.zipWithIndex.map {
                      case (Left(s), i) => <.span(^.key := s"frame-clause-$i", s)
                      case (Right(argSlot), i) => <.span(
                        ^.key := s"frame-clause-$i",
                        BoolLocal.make(initialValue = false) { isEditingSlot =>
                          val sigilSuffix = surrogateFrame.args.get(argSlot).get match {
                            case Noun(_) => ""
                            case Preposition(_, _) => ""
                            case Locative => "[where]"
                            case Complement(_) => "[verb]"
                            case Gerund => "[ing]"
                          }
                          if(!isEditingSlot.value) {
                            frameClause.argMapping.get(argSlot).map(s => <.span(S.argSigil)(s + sigilSuffix)).getOrElse(
                              <.span(S.argPlaceholder)(
                                surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
                              )
                            )((^.onClick --> isEditingSlot.setState(true)).when(validInstance.isEmpty))
                          } else {
                            Mounting.make(textFocusingRef.foreach(_.focus())) {
                              StringLocal.make(
                                initialValue = frameClause.argMapping.get(argSlot).getOrElse("")
                              ) { argSigil =>
                                val argLens = frameClauseLens
                                  .composeLens(FrameClause.argMapping)
                                  .composeLens(Optics.at(argSlot: ArgumentSlot))
                                  <.input(
                                    ^.`type` := "text",
                                    ^.value := argSigil.value,
                                    ^.onChange ==> ((e: ReactEventFromInput) => argSigil.setState(e.target.value)),
                                    ^.onKeyDown ==> ((e: ReactKeyboardEventFromInput) =>
                                      CallbackOption.keyCodeSwitch(e) {
                                        case KeyCode.Enter => frame.zoomStateL(argLens).setState(
                                          Some(argSigil.value).filter(_.nonEmpty)
                                        )
                                      } >> isEditingSlot.setState(false)
                                    )
                                  ).withRef(textFocusingRef)
                              }
                            }
                          }
                        }
                      )
                    }.map(List(_)).intercalate(List(<.span(" "))).zipWithIndex.map(p => p._1(^.key := "frame-clause-tok-" + p._2.toString)).toVdomArray,
                    validInstance.whenDefined { instance =>
                      val instanceLens = instancesLens
                        .composeLens(Optics.at(instance._3))
                        .composeIso(nonEmptySetOptionIso[QuestionId])
                      (^.onClick --> (
                         frame.zoomStateL(instanceLens).modState(_ + instance._1) >>
                           clauseOptions.setState(Set())
                       )
                      )
                    }
                  )
                )
              },
              newClauseOptions.zipWithIndex.toVdomArray { case ((qid, thisFrame, argSlot), index) =>
                val surrogateFrame = Frame(thisFrame, verbInflectedForms, presentTan)
                <.div(S.addClauseOption, S.candidateClauseChoice)(
                  ^.key := "new-clause-" + index.toString,
                  s"(+) ${surrogateFrame.clauses().head}",
                  ^.onClick --> (
                    frame.zoomStateL(clauseSetLens).modState(
                      _ ++ List(FrameClause(thisFrame, Map(), Map(argSlot -> NonEmptySet.of(qid))))
                    ) >> clauseOptions.setState(Set())
                  )
                )
              }
            )
          )
        },
        <.div(S.addFrameOption, S.candidateClauseChoice.when(clauseOptions.value.nonEmpty))(
          "(+) new frame",
          ^.onClick --> frame.zoomStateL(VerbFrame.clauseSets).modState(
            _ ++ List(Nil)
          )
        )
      ),
      <.div(S.frameAuditingDisplay, S.scrollPane) {
        def getInstancesForSentence(
          instances: Map[ArgumentSlot, NonEmptySet[QuestionId]]
        ) = instances.collect {
          case (argSlot, qids) if qids.exists(qid => sentenceIdOpt.exists(_ == qid.sentenceId)) =>
            argSlot -> qids.filter(qid => sentenceIdOpt.exists(_ == qid.sentenceId))
        }
        def getSpanForQid(qid: QuestionId): Option[String] = {
          sentenceOpt.flatMap(sentence =>
            sentence.verbEntries(qid.verbIndex).questionLabels(qid.questionString)
              .answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList)
              .groupBy(x => x).toList.sortBy(-_._2.size)
              .headOption.map(_._1).map(s => Text.renderSpan(sentence.sentenceTokens, (s.begin until s.end).toSet))
          )
        }
        def getSpanForQids(qids: List[QuestionId]): Option[String] = {
          qids.toList.flatMap(getSpanForQid).groupBy(x => x)
            .toList.sortBy(-_._2.size)
            .headOption.map(_._1)
        }
        frame.value.clauseSets.zipWithIndex
          .filter(_._1.exists(fc => getInstancesForSentence(fc.instances).nonEmpty)).zipWithIndex
          .toVdomArray { case ((clauseSet, clauseSetIndex), index) =>
            val instancesForClauseSet = clauseSet.map(_.instances).map(getInstancesForSentence)
            val qidsForSigils = clauseSet.zip(instancesForClauseSet).foldMap { case (frameClause, instances) =>
              frameClause.argMapping.map { case (argSlot, sigil) =>
                sigil -> instances.get(argSlot).combineAll
              }.toMap
            }
            val spansForSigils = qidsForSigils.flatMap { case (sigil, qids) =>
              getSpanForQids(qids.toList).map(sigil -> _)
            }
            <.div(S.singleFrameAuditingDisplay)(
              ^.key := "frame-audit-" + index.toString,
              <.div(S.frameHeading)(
                <.span(S.frameHeadingText)(
                  s"Frame $clauseSetIndex"
                )
              ),
              <.div(S.clauseSetDisplay)(
                <.ul(
                  clauseSet.zip(instancesForClauseSet).zipWithIndex.toVdomArray { case ((frameClause, instances), index) =>
                    val argSlots = instances.keySet ++ frameClause.argMapping.keySet
                    val argMap = argSlots.toList.flatMap { argSlot =>
                      frameClause.argMapping
                        .get(argSlot)
                        .flatMap(spansForSigils.get)
                        .orElse(instances.get(argSlot).map(_.toList).flatMap(getSpanForQids))
                        .map(argSlot -> _)
                    }.toMap
                    val surrogateFrame = Frame(frameClause.args, verbInflectedForms, mightTan)
                      <.li(
                        ^.key := "audited-clause-set-" + index.toString,
                        <.span(
                          surrogateFrame.clausesWithArgMarkers.head.map {
                            case Left(s) => <.span(s)
                            case Right(argSlot) =>
                              argMap.get(argSlot).fold(
                                <.span(surrogateFrame.args.get(argSlot).get.placeholder.mkString(" "))
                              )(phrase => <.span(S.substitutedArgString)(phrase))
                          }.map(List(_)).intercalate(List(<.span(" ")))
                            .zipWithIndex.map(p => p._1(^.key := "audit-clause-token-" + p._2.toString)).toVdomArray
                        )
                      )
                  }
                )
              )
            )
        }
      }
    )
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

  class Backend(scope: BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      DataFetch.make(request = (), sendRequest = _ => props.docService.getDataIndex.product(Remote(props.verbService.getVerbs))) {
        case DataFetch.Loading => <.div(S.loadingNotice)("Waiting for verb data...")
        case DataFetch.Loaded((dataIndex, verbCounts)) =>
          NavQueryLocal.make(initialValue = props.urlNavQuery) { navQuery =>
            val sortedVerbCounts = verbCounts.toList.sorted(
              Order.catsKernelOrderingForOrder(
                Order.whenEqual(
                  Order.by[(InflectedForms, Int), Int](-_._2),
                  Order.by[(InflectedForms, Int), String](p => inflToString(p._1))
                )
              )
            )
            val initVerb = sortedVerbCounts.map(_._1)
              .find(navQuery.value.matchesVerb)
              .getOrElse(sortedVerbCounts.head._1)
            VerbLocal.make(initialValue = initVerb) { curVerb =>
              val verbSpec = curVerb.value.allForms
                .map(form => form -> sortedVerbCounts.filter(_._1.allForms.contains(form)).size)
                .sortBy(_._2).map(_._1)
                .foldLeft(verbCounts.keySet.map(_.allForms.toSet) -> Set.empty[LowerCaseString]) {
                case ((remVerbs, spec), nextVerb) =>
                  if(remVerbs.size == 1) remVerbs -> spec
                  else remVerbs.filter(_.contains(nextVerb)) -> (spec + nextVerb)
              }._2
              val searchQuery = Search.Query(predicateOpt = Some(curVerb.value), keywords = Set())
              FrameLocal.make(initialValue = None) { curFrame =>
                val syncedFrameOpt = curFrame.value.map { frame =>
                  StateSnapshot.withReuse.prepare[VerbFrame](
                    (vfOpt, cb) => vfOpt.fold(cb)(vf =>
                      curFrame.setState(Some(vf), cb) >>
                        Callback(
                          props.verbService.saveFrame(vf)
                            .foreach(newVF => curFrame.setState(Some(newVF), cb).runNow)
                        )
                    )
                  )(frame)(Reusability.by_==[VerbFrame])
                }
                val labeledSentences = curFrame.value.foldMap(
                  _.clauseSets.flatten.foldMap(
                    _.instances.values.toList.foldMap(_.map(_.sentenceId).toList.toSet)
                  )
                )
                val labeledDocuments = labeledSentences.map(_.documentId)
                SearchFetch.make(request = searchQuery, sendRequest = props.docService.searchDocuments _) { searchFetchCtx =>
                  val searchCtx = searchFetchCtx match {
                    case SearchFetch.Loading => None
                    case SearchFetch.Loaded(docIds) =>
                      val curDocMetas = dataIndex.allDocumentMetas.filter(meta => docIds.contains(meta.id) && meta.part == DatasetPartition.Train)
                      val initDocMeta = curDocMetas
                        .find(navQuery.value.matchesDoc)
                        .getOrElse(curDocMetas.head)
                      Some((docIds, curDocMetas, initDocMeta))
                  }
                  def docIdsOpt = searchCtx.map(_._1)
                  def curDocMetasOpt = searchCtx.map(_._2)
                  def initDocMetaOpt = searchCtx.map(_._3)
                  DocMetaOptLocal.make(initialValue = initDocMetaOpt) { curDocMetaOpt =>
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
                    DocFetch.make(
                      request = curDocMetaOpt.value.map(_.id),
                      sendRequest = idOpt => idOpt.map(props.docService.getDocument).sequence) { docFetchCtx =>
                      val docCtx = docFetchCtx match {
                        case DocFetch.Loading => None
                        case DocFetch.Loaded(None) => None
                        case DocFetch.Loaded(Some(doc)) =>
                          val curSentences = getCurSentences(doc.sentences, searchQuery)
                          val initSentence = curSentences
                            .find(navQuery.value.matchesSentence)
                            .getOrElse(curSentences.head)
                          Some((doc, curSentences, initSentence))
                      }
                      def curDocOpt = docCtx.map(_._1)
                      def curSentencesOpt = docCtx.map(_._2)
                      def initSentenceOpt = docCtx.map(_._3)
                      SentOptLocal.make(initialValue = initSentenceOpt) { curSentenceOpt =>
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
                                  dom.window.history.replaceState(
                                    "", "", linkPath
                                  )))(
                                <.span(S.sentenceLink)("Link: ", <.a(^.href := linkPath, linkPath.tail))
                              )
                            }.whenDefined
                          ),
                          QuestionSetLocal.make(initialValue = Set()) { hoveredQids =>
                            FrameChoiceLocal.make(initialValue = Set()) { curFrameOptions =>
                              <.div(S.dataContainer)(
                                syncedFrameOpt.filter(_.value.inflectedForms == curVerb.value) match {
                                  case None =>
                                    Mounting.make(
                                      Callback.future(
                                        props.verbService.getFrame(curVerb.value)
                                          .map(f => curFrame.setState(Some(f)))))(
                                      <.div(S.loadingNotice)("Loading frame..."))
                                  case Some(frame) => frameDisplayPane(
                                    dataIndex, curVerb.value, curDocMetasOpt, curSentenceOpt.value,
                                    frame, curFrameOptions, hoveredQids, navQuery
                                  )
                                },
                                (searchCtx, zoomOpt(curDocMetaOpt)(Reusability.by_==[DocumentMetadata])) match {
                                  case (None, _) | (_, None) => <.div(S.loadingNotice)("Loading document list...")
                                  case (Some((_, docMetas, _)), Some(curDocMeta)) =>
                                    <.div(S.dataContainer)(
                                      docSelectionPane(
                                        dataIndex.documents(DatasetPartition.Train).size,
                                        docMetas,
                                        labeledDocuments,
                                        curDocMeta
                                      ),
                                      (docCtx, zoomOpt(curSentenceOpt)(Reusability.by_==[Sentence])) match {
                                        case (None, _) | (_, None) => <.div(S.loadingNotice)("Loading document...")
                                        case (Some((doc, curSentences, _)), Some(curSentence)) =>
                                          <.div(S.documentContainer)(
                                            sentenceSelectionPane(
                                              doc.sentences.size,
                                              curSentences,
                                              labeledSentences,
                                              searchQuery,
                                              curSentence
                                            ),
                                            sentenceDisplayPane(
                                              dataIndex.getPart(curDocMeta.value.id),
                                              curDocMeta.value,
                                              curSentence.value,
                                              curVerb.value,
                                              syncedFrameOpt,
                                              curFrameOptions,
                                              hoveredQids,
                                              navQuery
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
    }
  }

  val Component = ScalaComponent.builder[Props]("VerbAnnClient")
    .initialState(State.initial)
    .renderBackend[Backend]
    .build

}
