package qfirst.browse
import qfirst.Mounting
import qfirst.ClauseResolution
import qfirst.ClauseResolution.ArgStructure

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

import qasrl._
import qasrl.data._

import qasrl.bank._

import qasrl.bank.service.DocumentService
import qasrl.bank.service.Search


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
  // val FramesetFetch = new CacheCallContent[InflectedForms, VerbFrameset]
  val VerbLocal = new LocalState[InflectedForms]
  val FramesetLocal = new LocalState[Option[VerbFrameset]]
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
    verbService: VerbFrameService[Future],
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
    hoveredQids: StateSnapshot[Set[QuestionId]]
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
      (qid, ClauseResolution.getClauseTemplate(frame), argSlot)
    }.toSet

    <.tr(S.qaPairRow)(
      <.td(roundIndicatorStyle),
      <.td(S.questionCell,
           S.hoveredQuestion.when(hoveredQids.value.contains(qid)))(
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
    curFrameset: Option[VerbFrameset],
    verb: VerbEntry,
    color: Rgba,
    hoveredQids: StateSnapshot[Set[QuestionId]],
    navQuery: StateSnapshot[NavQuery],
    displayQAs: Boolean
  ) = {
    // TODO
    // val coveredQids = curFrameset
    //   .foldMap(_.frames.foldMap(_.foldMap(_.instances.values.toList.foldMap(_.toSortedSet.toSet))))
    val qaTable = if(displayQAs) Some {
      <.table(S.verbQAsTable)(
          <.tbody(S.verbQAsTableBody) {
            val questionLabels = verb.questionLabels.toList.map(_._2).sorted
            if(questionLabels.isEmpty) {
              <.tr(<.td(<.span((S.loadingNotice)("All questions have been filtered out."))))
            } else questionLabels.toVdomArray { label =>
              val qid = QuestionId(SentenceId.fromString(curSentence.sentenceId), verb.verbIndex, label.questionString)
              val resolutions = getFramesForQid(verb.verbInflectedForms, qid)
              qaLabelRow(curSentence, label, color, qid, resolutions, hoveredQids)(^.key := s"short-${label.questionString}"),
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
    curFrameset: Option[VerbFrameset],
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
            verbEntryDisplay(sentence, curFrameset, verb, verbColorMap(verb.verbIndex), hoveredQids, navQuery, displayQAs = verb.verbInflectedForms == verbForms)(
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
    frameset: VerbFrameset,
    hoveredQids: StateSnapshot[Set[QuestionId]],
    navQuery: StateSnapshot[NavQuery]
  ) = {
    // TODO this whole method
    val sentenceIdOpt = sentenceOpt.map(s => SentenceId.fromString(s.sentenceId))
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
    <.div(S.frameContainer)(
      <.div(S.frameSpecDisplay, S.scrollPane)(
        frameset.frames.zipWithIndex.toVdomArray { case (frame, frameIndex) =>
          val frameLens = VerbFrameset.frames
            .composeLens(unsafeListAt[VerbFrame](frameIndex))
          val matchesCurrentSentence = sentenceOpt.exists(s => frame.instances.contains(SentenceId.fromString(s.sentenceId)))
          val frameSentenceDocPairsOpt = (docIdToDocMetaOpt, sentenceOpt).mapN { (docIdToDocMeta, sentence) =>
            (frame.instances.toSet + SentenceId.fromString(sentence.sentenceId)).toList
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
            frameSentenceDocPairsOpt.map { allSentencesForFrame =>
              val sid = allSentencesForFrame(index)._1
              val sidStr = SentenceId.toString(sid)
              val docIdStr = sid.documentId.toString
              NavQuery(verbInflectedForms.allForms.toSet, Set(docIdStr.lowerCase), Set(sidStr.lowerCase))
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
          <.div(S.matchingFrame.when(matchesCurrentSentence))(
            ^.key := "clause-set-" + frameIndex.toString,
            <.div(S.frameHeading)(
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
              frame.clauseTemplates.zipWithIndex.toVdomArray { case (frameClause, clauseIndex) =>
                // TODO compute based on probabilities
                // val qidsInSentence = sentenceIdOpt.foldMap(sentenceId =>
                //   frameClause.instances.toList.foldMap(_._2.filter(_.sentenceId == sentenceId).toSet)
                // )
                val frameClauseLens = frameLens
                  .composeLens(VerbFrame.clauseTemplates)
                  .composeLens(unsafeListAt[FrameClause](clauseIndex))
                // val instancesLens = frameClauseLens
                //   .composeLens(FrameClause.instances)
                val surrogateFrame = makeSurrogateFrame(frameClause.args, verbInflectedForms, useModal = false)

                <.div(
                  // if(hoveredQids.value.intersect(qidsInSentence).nonEmpty) S.hoveredClause
                  // else S.coveredClause.when(qidsInSentence.nonEmpty)
                )(
                  ^.key := "clause-" + clauseIndex.toString,
                  <.span(
                    // ^.onMouseMove --> (
                    //   if(hoveredQids.value != qidsInSentence) hoveredQids.setState(qidsInSentence)
                    //   else Callback.empty
                    // ),
                    // ^.onMouseOut --> hoveredQids.setState(Set()),
                    <.span(f"(${frameClause.probability}%.2f) "),
                        // ${surrogateFrame.clauses.head}"
                      // TODO fix to include arg sigils once we have them
                    surrogateFrame.clausesWithArgMarkers.head.zipWithIndex.map {
                      case (Left(s), i) => <.span(^.key := s"frame-clause-$i", s)
                      case (Right(argSlot), i) => <.span(
                        ^.key := s"frame-clause-$i",
                        BoolLocal.make(initialValue = false) { isEditingSlot =>
                          val sigilSuffix = surrogateFrame.args.get(argSlot).get match {
                            case Noun(_) => ""
                            case Prep(p, _) =>
                              if(p.toString.contains(" do")) "[inf]"
                              else if(p.toString.contains(" doing")) "[ng]"
                              else ""
                            case Locative => "[where]"
                          }
                          frameClause.argMapping.get(argSlot).map(s => <.span(S.argSigil)(s + sigilSuffix): VdomElement).getOrElse(
                            <.span(S.argPlaceholder)(
                              surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
                            )
                          )
                        }
                      )
                    }.map(List(_)).intercalate(List(<.span(" "))).zipWithIndex.map(p => p._1(^.key := "frame-clause-tok-" + p._2.toString)).toVdomArray,
                  )
                )
              }
            )
          )
        }
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
        <.div() // TODO auditing display once we have question connections
        // frameset.value.frame.zipWithIndex
        //   .filter(_._1.exists(fc => getInstancesForSentence(fc.instances).nonEmpty)).zipWithIndex
        //   .toVdomArray { case ((frames, frameIndex), index) =>
        //     val instancesForClauseSet = clauseSet.map(_.instances).map(getInstancesForSentence)
        //     val qidsForSigils = clauseSet.zip(instancesForClauseSet).foldMap { case (frameClause, instances) =>
        //       frameClause.argMapping.map { case (argSlot, sigil) =>
        //         sigil -> instances.get(argSlot).combineAll
        //       }.toMap
        //     }
        //     val spansForSigils = qidsForSigils.flatMap { case (sigil, qids) =>
        //       getSpanForQids(qids.toList).map(sigil -> _)
        //     }
        //     <.div(S.singleFrameAuditingDisplay)(
        //       ^.key := "frame-audit-" + index.toString,
        //       <.div(S.frameHeading)(
        //         <.span(S.frameHeadingText)(
        //           s"Frame $clauseSetIndex"
        //         )
        //       ),
        //       <.div(S.clauseSetDisplay)(
        //         <.ul(
        //           clauseSet.zip(instancesForClauseSet).zipWithIndex.toVdomArray { case ((frameClause, instances), index) =>
        //             val argSlots = instances.keySet ++ frameClause.argMapping.keySet
        //             val argMap = argSlots.toList.flatMap { argSlot =>
        //               frameClause.argMapping
        //                 .get(argSlot)
        //                 .flatMap(spansForSigils.get)
        //                 .orElse(instances.get(argSlot).map(_.toList).flatMap(getSpanForQids))
        //                 .map(argSlot -> _)
        //             }.toMap
        //             val surrogateFrame = Frame(frameClause.args, verbInflectedForms, mightTan)
        //               <.li(
        //                 ^.key := "audited-clause-set-" + index.toString,
        //                 <.span(
        //                   surrogateFrame.clausesWithArgMarkers.head.map {
        //                     case Left(s) => <.span(s)
        //                     case Right(argSlot) =>
        //                       argMap.get(argSlot).fold(
        //                         <.span(surrogateFrame.args.get(argSlot).get.placeholder.mkString(" "))
        //                       )(phrase => <.span(S.substitutedArgString)(phrase))
        //                   }.map(List(_)).intercalate(List(<.span(" ")))
        //                     .zipWithIndex.map(p => p._1(^.key := "audit-clause-token-" + p._2.toString)).toVdomArray
        //                 )
        //               )
        //           }
        //         )
        //       )
        //     )
        // }
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
              FramesetLocal.make(initialValue = None) { curFrameset =>
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
                            <.div(S.dataContainer)(
                              curFrameset.value.filter(_.inflectedForms == curVerb.value) match {
                                case None =>
                                  Mounting.make(
                                    Callback.future(
                                      props.verbService.getFrame(curVerb.value)
                                        .map(f => curFrameset.setState(Some(f)))))(
                                    <.div(S.loadingNotice)("Loading frame..."))
                                case Some(frame) => frameDisplayPane(
                                  dataIndex, curVerb.value, curDocMetasOpt, curSentenceOpt.value,
                                  frame, hoveredQids, navQuery
                                )
                              },
                              (searchCtx, zoomOpt(curDocMetaOpt)(Reusability.by_==[DocumentMetadata])) match {
                                case (None, _) | (_, None) => <.div(S.loadingNotice)("Loading document list...")
                                case (Some((_, docMetas, _)), Some(curDocMeta)) =>
                                  <.div(S.dataContainer)(
                                    docSelectionPane(
                                      dataIndex.documents(DatasetPartition.Train).size,
                                      docMetas,
                                      curDocMeta
                                    ),
                                    (docCtx, zoomOpt(curSentenceOpt)(Reusability.by_==[Sentence])) match {
                                      case (None, _) | (_, None) => <.div(S.loadingNotice)("Loading document...")
                                      case (Some((doc, curSentences, _)), Some(curSentence)) =>
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
                                            curFrameset.value,
                                            hoveredQids,
                                            navQuery
                                          )
                                        )
                                    }
                                  )
                              }
                            )
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
