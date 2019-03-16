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

object VerbAnnUI {

  val S = VerbAnnStyles

  val DataFetch = new CacheCallContent[Unit, (DataIndex, Map[InflectedForms, Int])]
  val DocFetch = new CacheCallContent[DocumentId, Document]
  val SearchFetch = new CacheCallContent[Search.Query, Set[DocumentId]]
  val FrameFetch = new CacheCallContent[InflectedForms, VerbFrame]
  val VerbLocal = new LocalState[InflectedForms]
  val FrameLocal = new LocalState[Option[VerbFrame]]
  val DocMetaLocal = new LocalState[DocumentMetadata]
  val SentLocal = new LocalState[Sentence]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]
  val IntSetLocal = new LocalState[Set[Int]]
  val FrameChoiceLocal = new LocalState[Set[(QuestionId, ArgStructure, ArgumentSlot)]]

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
    verbService: VerbAnnotationService[Future]
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
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]]
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

    <.tr(S.qaPairRow)(
      <.td(roundIndicatorStyle),
      <.td(S.questionCell)(
        <.span(S.questionText) {
          val questionChoiceSet = resolutions.map { case (frame, argSlot) =>
            (qid, frame.structure, argSlot)
          }.toSet
          val isNotChosen = frameChoices.value.forall { case (chosenQid, chosenArgs, argSlot) =>
            chosenQid != qid
          }

          <.span(
            ^.onClick --> (
              if(isNotChosen) frameChoices.setState(questionChoiceSet)
              else frameChoices.setState(Set())
            ),
            S.candidateClauseChoice.when(!isNotChosen),
            label.questionString
          )
        }
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
    verb: VerbEntry,
    color: Rgba,
    qidToResolutions: Map[QuestionId, Set[(Frame, ArgumentSlot)]],
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]],
    displayQAs: Boolean
    // anchorCorrectionPixels: Int
  ) = {
    val qaTable = if(displayQAs) Some {
      <.table(S.verbQAsTable)(
          <.tbody(S.verbQAsTableBody) {
            val questionLabels = verb.questionLabels.toList.map(_._2).sorted
            if(questionLabels.isEmpty) {
              <.tr(<.td(<.span((S.loadingNotice)("All questions have been filtered out."))))
            } else questionLabels.toVdomArray { label =>
              val qid = QuestionId(SentenceId.fromString(curSentence.sentenceId), verb.verbIndex, label.questionString)
              val resolutions = qidToResolutions(qid)
              qaLabelRow(curSentence, label, color, qid, resolutions, frameChoices)(^.key := s"short-${label.questionString}"),
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
          // ^.top := s"-${anchorCorrectionPixels}px",
          ^.visibility := "hidden"
        )
      ),
      <.div(S.verbHeading)(
        <.span(S.verbHeadingText)(
          ^.color := color.copy(a = 1.0).toColorStyleString,
          curSentence.sentenceTokens(verb.verbIndex)
        )
      ),
      qaTable.whenDefined
    )
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


  var qidToFramesCache = Map.empty[QuestionId, Set[(Frame, ArgumentSlot)]]

  def sentenceDisplayPane(
    part: DatasetPartition,
    docMeta: DocumentMetadata,
    sentence: Sentence,
    verbForms: InflectedForms,
    frameChoices: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]]
  ) = {
    val sentenceId = SentenceId.fromString(sentence.sentenceId)
    val (targetedVerbs, untargetedVerbs) = sentence.verbEntries.values
      .toList.partition(_.verbInflectedForms == verbForms)
    val searchedVerbIndices = targetedVerbs.map(_.verbIndex).toSet
    val sortedVerbs = targetedVerbs.sortBy(_.verbIndex) ++ untargetedVerbs.sortBy(_.verbIndex)
    val qidToResolutions = targetedVerbs.flatMap { verb =>
      verb.questionLabels.keys.toList.map { question =>
        val qid = QuestionId(SentenceId.fromString(sentence.sentenceId), verb.verbIndex, question)
        val frames = qidToFramesCache.get(qid).getOrElse {
          val questionTokensIsh = question.init.split(" ").toVector
          val stateMachine = new TemplateStateMachine(questionTokensIsh, verbForms)
          val template = new QuestionProcessor(stateMachine)
          val results = template.processStringFully(question).right.toOption.map { goodStates =>
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
        qid -> frames
      }
    }.toMap
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
            verbEntryDisplay(sentence, verb, verbColorMap(verb.verbIndex), qidToResolutions, frameChoices, displayQAs = verb.verbInflectedForms == verbForms)(
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

  def unsafeListAt[A](index: Int) =
    Lens[List[A], A](s => s(index))(a => s => s.updated(index, a))

  def frameDisplayPane(
    dataIndex: DataIndex,
    verbInflectedForms: InflectedForms,
    curDocMeta: StateSnapshot[DocumentMetadata],
    sentence: Sentence,
    frame: StateSnapshot[VerbFrame],
    clauseOptions: StateSnapshot[Set[(QuestionId, ArgStructure, ArgumentSlot)]]
  ) = {
    // TODO add ambient highlighting of questions and clauses that have been annotated for the current sentence
    // TODO: add jump options to go to instances of a frame and skip to next instance of it, etc.
    val surrogateTan = TAN(qasrl.Modal("might".lowerCase), false, false, false)
    <.div(S.frameContainer)(
      <.div(S.frameSpecDisplay, S.scrollPane)(
        frame.value.clauseSets.zipWithIndex.toVdomArray { case (clauseSet, clauseSetIndex) =>
          val clauseSetLens = VerbFrame.clauseSets
            .composeLens(unsafeListAt[List[FrameClause]](clauseSetIndex))
          val newClauseOptions = clauseOptions.value.filter(p => !clauseSet.exists(_.args == p._2))

          <.div(
            <.div(S.frameHeading)(
              <.span(S.frameHeadingText)(
                s"Frame $clauseSetIndex "
              ),
              <.span(S.frameDeleteText)(
                "(delete)",
                ^.onClick --> frame.zoomStateL(VerbFrame.clauseSets).modState(_.patch(clauseSetIndex, Nil, 1))
              ) // TODO: "link all valid" option
            ),
            <.div(S.clauseSetDisplay)(
              clauseSet.zipWithIndex.toVdomArray { case (frameClause, clauseIndex) =>
                val validInstance = clauseOptions.value.find(_._2 == frameClause.args)
                val frameClauseLens = clauseSetLens
                  .composeLens(unsafeListAt[FrameClause](clauseIndex))
                val instancesLens = frameClauseLens
                  .composeLens(FrameClause.instances)
                val surrogateFrame = Frame(frameClause.args, verbInflectedForms, surrogateTan)

                <.div(S.candidateClauseChoice.when(validInstance.nonEmpty))(
                  <.span(S.clauseDeleteText)(
                    "(-)",
                    ^.onClick --> frame.zoomStateL(clauseSetLens).modState(_.patch(clauseIndex, Nil, 1))
                  ),
                  // TODO arg sigil modification
                  <.span(
                    " " + surrogateFrame.clausesWithArgs(frameClause.argMapping).head,
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
              clauseOptions.value.toVdomArray { case (qid, thisFrame, argSlot) =>
                val surrogateFrame = Frame(thisFrame, verbInflectedForms, surrogateTan)
                <.div(S.addClauseOption, S.candidateClauseChoice)(
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
        <.div(S.addFrameOption)(
          "(+) new frame",
          ^.onClick --> frame.zoomStateL(VerbFrame.clauseSets).modState(
            _ ++ List(Nil)
          )
        )
      ),
      <.div(S.frameAuditingDisplay, S.scrollPane) {
        // TODO jump to prev/next instance of frame
        val sid = SentenceId.fromString(sentence.sentenceId)
        def getInstancesForSentence(
          instances: Map[ArgumentSlot, NonEmptySet[QuestionId]]
        ) = instances.collect {
          case (argSlot, qids) if qids.exists(_.sentenceId == sid) =>
            argSlot -> qids.filter(_.sentenceId == sid)
        }
        def getSpanForQid(qid: QuestionId): Option[String] = {
          sentence.verbEntries(qid.verbIndex).questionLabels(qid.questionString)
            .answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList)
            .groupBy(x => x).toList.sortBy(-_._2.size)
            .headOption.map(_._1).map(s => Text.renderSpan(sentence.sentenceTokens, (s.begin until s.end).toSet))
        }
        def getSpanForQids(qids: List[QuestionId]): Option[String] = {
          qids.toList.flatMap(getSpanForQid).groupBy(x => x)
            .toList.sortBy(-_._2.size)
            .headOption.map(_._1)
        }
        frame.value.clauseSets.zipWithIndex
          .filter(_._1.exists(fc => getInstancesForSentence(fc.instances).nonEmpty))
          .toVdomArray { case (clauseSet, clauseSetIndex) =>
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
              <.div(S.frameHeading)(
                <.span(S.frameHeadingText)(
                  s"Frame $clauseSetIndex"
                )
              ),
              <.div(S.clauseSetDisplay)(
                clauseSet.zip(instancesForClauseSet).toVdomArray { case (frameClause, instances) =>
                  val argSlots = instances.keySet ++ frameClause.argMapping.keySet
                  val argMap = argSlots.toList.flatMap { argSlot =>
                    frameClause.argMapping
                      .get(argSlot)
                      .flatMap(spansForSigils.get)
                      .orElse(instances.get(argSlot).map(_.toList).flatMap(getSpanForQids))
                      .map(argSlot -> _)
                  }.toMap
                  val surrogateFrame = Frame(frameClause.args, verbInflectedForms, surrogateTan)
                    <.div(
                      <.span(
                        surrogateFrame.clausesWithArgs(argMap).head
                      )
                    )
                }
              )
            )
        }
      }
    )
  }

  class Backend(scope: BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      DataFetch.make(request = (), sendRequest = _ => props.docService.getDataIndex.product(Remote(props.verbService.getVerbs))) {
        case DataFetch.Loading => <.div(S.loadingNotice)("Waiting for verb data...")
        case DataFetch.Loaded((dataIndex, verbCounts)) =>
          val sortedVerbCounts = verbCounts.toList.sortBy(-_._2)
          VerbLocal.make(initialValue = sortedVerbCounts.head._1) { curVerb =>
            val searchQuery = Search.Query(predicateOpt = Some(curVerb.value), keywords = Set())
            <.div(S.mainContainer)(
              <.div(S.headerContainer)(
                <.select(S.verbDropdown)(
                  ^.value := inflToString(curVerb.value),
                  ^.onChange ==> ((e: ReactEventFromInput) => curVerb.setState(inflFromString(e.target.value))),
                  sortedVerbCounts.toVdomArray { case (forms, count) =>
                    <.option(
                      ^.value := inflToString(forms),
                      f"$count%5d ${forms.allForms.mkString(", ")}%s"
                    )
                  }
                )
              ),
              SearchFetch.make(request = searchQuery, sendRequest = props.docService.searchDocuments _) {
                case SearchFetch.Loading => <.span(S.loadingNotice)("Waiting for document list...")
                case SearchFetch.Loaded(docIds) =>
                  val curDocMetas = dataIndex.allDocumentMetas.filter(meta => docIds.contains(meta.id) && meta.part == DatasetPartition.Train)
                  FrameLocal.make(initialValue = None) { curFrame =>
                    DocMetaLocal.make(initialValue = curDocMetas.head, shouldRefresh = _ => false) { curDocMeta =>
                      FrameChoiceLocal.make(initialValue = Set()) { curFrameOptions =>
                          DocFetch.make(request = curDocMeta.value.id, sendRequest = id => props.docService.getDocument(id)) {
                            case DocFetch.Loading =>
                              <.div(S.documentContainer)(
                                <.span(S.loadingNotice)("Loading document...")
                              )
                            case DocFetch.Loaded(doc) =>
                              val curSentences = getCurSentences(doc.sentences, searchQuery)
                              SentLocal.make(initialValue = curSentences.head) { curSentence =>
                                <.div(S.dataContainer)(
                                  FrameFetch.make(
                                    request = curVerb.value, sendRequest = v => Remote(props.verbService.getFrame(v)),
                                    willLoad = f => curFrame.setState(Some(f))) {
                                    case FrameFetch.Loading => <.div(S.loadingNotice)("Loading verb frame...")
                                    case FrameFetch.Loaded(_) => // ignore and use local state instead
                                      curFrame.value match {
                                        case None => <.div(S.loadingNotice)("Loading frame...")
                                        case Some(frame) => frameDisplayPane(
                                          dataIndex, curVerb.value, curDocMeta, curSentence.value,
                                          StateSnapshot.withReuse.prepare[VerbFrame](
                                            (vfOpt, cb) => vfOpt.fold(cb)(vf =>
                                              Callback(
                                                props.verbService.saveFrame(vf)
                                                  .foreach(newVF => curFrame.setState(Some(newVF), cb).runNow)
                                              )
                                            )
                                          )(frame)(Reusability.by_==[VerbFrame]),
                                          curFrameOptions
                                        )
                                      }
                                  },
                                  docSelectionPane(
                                    dataIndex.documents(DatasetPartition.Train).size,
                                    curDocMetas,
                                    curDocMeta
                                  ),
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
                                      curVerb.value,
                                      curFrameOptions
                                    )
                                  )
                                )
                              }
                          }
                      }
                    }
                  }
              }
            )
          }
      }
      // ResIdLocal.make(initialValue = ResId(false, 0)) { resId =>
      //   <.div(S.mainContainer)(
      //     ResIdProxyLocal.make(initialValue = resId.value.toProxy) { resIdProxy =>
      //       <.div(S.fixedRowContainer, S.headyContainer)(
      //         checkboxToggle("Full ambiguity", resIdProxy.zoomStateL(ResIdProxy.isFull)),
      //         <.input(
      //           ^.`type` := "text",
      //           ^.placeholder := "Index of ambiguity",
      //           ^.value := resIdProxy.value.index,
      //           ^.onChange ==> ((e: ReactEventFromInput) => resIdProxy.zoomStateL(ResIdProxy.index).setState(e.target.value)),
      //           ^.onKeyDown ==> (
      //             (e: ReactKeyboardEvent) => {
      //               CallbackOption.keyCodeSwitch(e) {
      //                 case KeyCode.Enter =>
      //                   resIdProxy.value.toResId.fold(Callback.empty)(resId.setState)
      //               }
      //             }
      //           )
      //         ),
      //         <.button(
      //           ^.`type` := "button",
      //           ^.onClick --> resId.zoomStateL(ResId.index).modState(_ - 1),
      //           "<--"
      //         ),
      //         <.button(
      //           ^.`type` := "button",
      //           ^.onClick --> resId.zoomStateL(ResId.index).modState(_ + 1),
      //           "-->"
      //         )
      //       )
      //     },
      //     ResFetch.make(request = resId.value, sendRequest = id => Remote(props.annService.getResolution(id.isFull, id.index))) {
      //       case ResFetch.Loading => <.div(S.loadingNotice)("Waiting for clause ambiguity data...")
      //       case ResFetch.Loaded(loadedClauseResolution) =>
      //         ResLocal.make(initialValue = loadedClauseResolution) { clauseResolutionS =>
      //           val clauseResolution = clauseResolutionS.value
      //           val ambig = clauseResolution.ambiguity
      //           val sid = ambig.sentenceId
      //           DocFetch.make(request = sid.documentId, sendRequest = id => props.docService.getDocument(id)) {
      //             case DocFetch.Loading => <.div(S.loadingNotice)("Waiting for document...")
      //             case DocFetch.Loaded(document) =>
      //               val sentence = document.sentences.find(_.sentenceId == SentenceId.toString(sid)).get
      //               val verbEntry = sentence.verbEntries(ambig.verbIndex)
      //               val questionLabel = verbEntry.questionLabels(ambig.questionString)

      //               val blueGreen = Rgba(0, 128, 255, 0.1)
      //               val verbColorMap = Map(verbEntry.verbIndex -> blueGreen)
      //               val answerSpansWithColors = questionLabel.answerJudgments.toList
      //                 .flatMap(_.judgment.getAnswer).flatMap(_.spans.toList)
      //                 .map(span => span -> blueGreen)

      //               <.div(S.flexyBottomContainer, S.flexColumnContainer)(
      //                 <.div(S.fixedRowContainer, S.sentenceTextContainer, S.headyContainer)(
      //                   <.span(S.sentenceText)(
      //                     renderSentenceWithHighlights(
      //                       sentence.sentenceTokens,
      //                       RenderWholeSentence(answerSpansWithColors),
      //                       verbColorMap.collect { case (verbIndex, color) =>
      //                         verbIndex -> (
      //                           (v: VdomTag) => <.a(
      //                             S.verbAnchorLink,
      //                             ^.href := s"#verb-$verbIndex",
      //                             v(
      //                               ^.color := color.copy(a = 1.0).toColorStyleString,
      //                               ^.fontWeight := "bold",
      //                               )
      //                           )
      //                         )
      //                       }
      //                     )
      //                   )
      //                 ),
      //                 <.div(S.verbEntryDisplay)(
      //                   <.div(S.verbHeading)(
      //                     <.span(S.verbHeadingText)(
      //                       ^.color := blueGreen.copy(a = 1.0).toColorStyleString,
      //                       sentence.sentenceTokens(verbEntry.verbIndex)
      //                     )
      //                   ),
      //                   <.div(S.questionHeading)(
      //                     <.span(S.questionHeadingText)(
      //                       ambig.questionString
      //                     )
      //                   ),
      //                   <.table(S.verbQAsTable)(
      //                     <.tbody(S.verbQAsTableBody)(
      //                       ambig.structures.toList.toVdomArray { clauseChoice =>
      //                         val innerCell = <.td(
      //                           <.span(S.clauseChoiceText)(
      //                             clauseChoice.frame.clauses(true).mkString(" / "),
      //                             ),
      //                           ^.onClick --> (
      //                             Callback(println(clauseChoice)) >>
      //                               Callback.future {
      //                                 val curChoice = clauseResolution.choiceOpt.getOrElse(Set.empty[ClauseChoice])
      //                                 val newChoice = if(curChoice.contains(clauseChoice)) curChoice - clauseChoice else curChoice + clauseChoice
      //                                 props.annService.saveResolution(resId.value.isFull, resId.value.index, newChoice)
      //                                   .map(clauseResolutionS.setState)
      //                               }
      //                           )
      //                         )
      //                         if(clauseResolution.choiceOpt.exists(_.contains(clauseChoice))) {
      //                           <.tr(S.clauseChoiceRow, S.darkerClauseChoiceRow)(
      //                             innerCell
      //                           )
      //                         } else {
      //                           <.tr(S.clauseChoiceRow)(
      //                             innerCell
      //                           )
      //                         }
      //                       },
      //                       {
      //                         val innerCell = <.td(
      //                           <.span(S.clauseChoiceText)(
      //                             "<None>"
      //                           ),
      //                           ^.onClick --> (
      //                             Callback.future {
      //                               props.annService.saveResolution(resId.value.isFull, resId.value.index, Set.empty[ClauseChoice])
      //                                 .map(clauseResolutionS.setState)
      //                             }
      //                           )
      //                         )
      //                         if(clauseResolution.choiceOpt.exists(_.isEmpty)) {
      //                           <.tr(S.clauseChoiceRow, S.darkerClauseChoiceRow)(innerCell)
      //                         } else {
      //                           <.tr(S.clauseChoiceRow)(innerCell)
      //                         }
      //                       }
      //                     )
      //                   ),
      //                   <.div(^.paddingTop := "20px")(
      //                     <.h3("Other Questions"),
      //                     <.ul(
      //                       verbEntry.questionLabels.keySet.toList.toVdomArray(qStr =>
      //                         <.li(qStr)
      //                       )
      //                     )
      //                   )
      //                 )
      //               )
      //           }
      //         }
      //     }
      //   )
      // }
    }
  }

  val Component = ScalaComponent.builder[Props]("VerbAnnClient")
    .initialState(State.initial)
    .renderBackend[Backend]
    .build

}
