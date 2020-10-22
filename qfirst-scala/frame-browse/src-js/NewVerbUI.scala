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

trait VerbTypeRendering[VerbType] {
  def fromString(x: String): VerbType
  def toString(verbType: VerbType): String
}
object VerbTypeRendering {
  implicit val inflectedFormsVerbTypeRendering = new VerbTypeRendering[InflectedForms] {
    import io.circe.syntax._
    import io.circe.parser.decode
    val printer = io.circe.Printer.noSpaces
    def fromString(x: String) = decode[InflectedForms](x).right.get
    def toString(verbType: InflectedForms): String = printer.pretty(verbType.asJson)
  }
  implicit val stringVerbTypeRendering = new VerbTypeRendering[String] {
    def fromString(x: String) = x
    def toString(verbType: String): String = verbType
  }
}

trait ArgRendering[VerbType, Arg] {
  def toString(sentence: SentenceInfo[VerbType, Arg], arg: Arg): String
}
object ArgRendering {
  implicit val qasrlArgRendering = new ArgRendering[InflectedForms, ClausalQuestion] {
    def toString(sentence: SentenceInfo[InflectedForms, ClausalQuestion], arg: ClausalQuestion): String = arg.questionString
  }
  implicit val ontonotes5ArgRendering = new ArgRendering[String, ESpan] {
    def toString(sentence: SentenceInfo[String, ESpan], arg: ESpan): String = Text.renderSpan(sentence.tokens, arg)
  }
  implicit val conll08ArgRendering = new ArgRendering[String, Int] {
    def toString(sentence: SentenceInfo[String, Int], arg: Int): String = sentence.tokens(arg)
  }
}

class NewVerbUI[VerbType, Arg: Order](
  implicit VerbType: VerbTypeRendering[VerbType],
  Arg: ArgRendering[VerbType, Arg],
){

  implicit val callbackMonoid = new Monoid[Callback] {
    override def empty = Callback.empty
    override def combine(x: Callback, y: Callback) = x >> y
  }

  val S = VerbAnnStyles
  import HOCs._

  val VerbsFetch = new CacheCallContent[Unit, Map[VerbType, Int]]
  val VerbModelFetch = new CacheCallContent[VerbType, VerbClusterModel[VerbType, Arg]]
  val SentencesFetch = new CacheCallContent[VerbType, Set[String]]
  val SentenceFetch = new CacheCallContent[String, SentenceInfo[VerbType, Arg]]

  val DataFetch = new CacheCallContent[Unit, (DataIndex, Map[VerbType, Int])]
  val DocFetch = new CacheCallContent[DocumentId, Document]
  val DocOptFetch = new CacheCallContent[Option[DocumentId], Option[Document]]
  val SearchFetch = new CacheCallContent[Search.Query, Set[DocumentId]]
  // val FramesetFetch = new CacheCallContent[InflectedForms, VerbFrameset]
  val EvalItemFetch = new CacheCallContent[Int, ParaphrasingInfo]
  val IntLocal = new LocalState[Int]
  val VerbModelLocal = new LocalState[Option[VerbClusterModel[VerbType, Arg]]]
  val DocMetaOptLocal = new LocalState[Option[DocumentMetadata]]
  val SentOptLocal = new LocalState[Option[Sentence]]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]
  val IntSetLocal = new LocalState[Set[Int]]
  val FrameChoiceLocal = new LocalState[Set[(ArgumentId[Arg], ArgStructure, ArgumentSlot)]]
  val QuestionSetLocal = new LocalState[Set[ArgumentId[Arg]]]

  val ClusterSplittingSpecLocal = new LocalState[ClusterSplittingSpec]
  val GoldParaphrasesLocal = new LocalState[VerbParaphraseLabels]
  val DoubleLocal = new LocalState[Double]

  val colspan = VdomAttr("colspan")
  val textFocusingRef = Ref[html.Element]

  // val ArgStructureOptLocal = new LocalState[Option[(ArgStructure, ArgumentSlot)]]

  case class Props(
    verbService: VerbFrameService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    urlNavQuery: NavQuery,
    mode: RunMode
  )

  case class State()
  object State {
    val initial = State()
  }

  @Lenses case class ClusterCriterionControl(
    numClusters: Int,
    entropyPenalty: Double
  )
  object ClusterCriterionControl {
    def default = ClusterCriterionControl(1, 0.0)
  }

  val LocalClusterCriterionControl = new LocalState[ClusterCriterionControl]

  def clusterCriterionField(
    label: String,
    criterion: StateSnapshot[ClusterSplittingCriterion]
  ): VdomElement = {
    <.div(
      LocalClusterCriterionControl.make(ClusterCriterionControl.default) { criterionControl =>
        <.span(
          label + " ",
          <.span(S.disabledCriterionText.unless(criterion.value.isNumber))(
            "clusters",
            ^.onClick --> criterion.value.getEntropy.foldMap(_ =>
              criterion.setState(
                ClusterSplittingCriterion.Number(criterionControl.value.numClusters)
              )
            )
          ),
          " / ",
          <.span(S.disabledCriterionText.unless(criterion.value.isEntropy))(
            "entropy",
            ^.onClick --> criterion.value.getNumber.foldMap(_ =>
              criterion.setState(
                ClusterSplittingCriterion.Entropy(criterionControl.value.entropyPenalty)
              )
            )
          ),
          ": ",
          zoomStateP(criterion, ClusterSplittingCriterion.number).whenDefined(numClusters =>
            View.intArrowField(S.shortTextField)(
              None,
              numClusters,
              criterionControl.zoomStateL(ClusterCriterionControl.numClusters).setState(_)
            )
          ),
          zoomStateP(criterion, ClusterSplittingCriterion.entropy)(Reusability.double(1e-3)).whenDefined(entropyPenalty =>
            View.doubleTextField(S.shortTextField)(
              None,
              entropyPenalty,
              criterionControl.zoomStateL(ClusterCriterionControl.entropyPenalty).setState(_)
            )
          )
        )
      }
    )
  }

  val queryKeywordHighlightLayer = Rgba(255, 255, 0, 0.4)

  val highlightLayerColors = List(
    // Rgba(255, 255,   0, 0.2), // yellow
    Rgba(  0, 128, 255, 0.1), // green-blue
    Rgba(255,   0, 128, 0.1), // magenta?
    Rgba( 64, 192,   0, 0.1), // something. idk
    Rgba(128,   0, 255, 0.1), // mystery
    Rgba(  0, 255, 128, 0.1)  // blue-green
  )

  // def paraphrasingHighlightStyle(
  //   structure: (ArgStructure, ArgumentSlot),
  //   referenceOpt: Option[(ArgStructure, ArgumentSlot)],
  //   goldParaphrasesOpt: Option[VerbParaphraseLabels]
  // ) = {
  //   (if(referenceOpt.exists(_ == structure)) Some(S.argStructureChoiceIsChosen)
  //    else (referenceOpt, goldParaphrasesOpt).mapN { (reference, goldParaphrases) =>
  //      if(goldParaphrases.paraphrases.equal(reference, structure)) Some(S.argStructureChoiceIsCorrectParaphrase)
  //      else if(goldParaphrases.paraphrases.apart(reference, structure)) Some(S.argStructureChoiceIsIncorrectParaphrase)
  //      else None
  //    }.flatten
  //   ).whenDefined
  // }

  // // beh
  // def tagModForStructureLabel(
  //   structure: (ArgStructure, ArgumentSlot),
  //   argStructureChoiceOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
  //   argStructureHoverOpt: StateSnapshot[Option[(ArgStructure, ArgumentSlot)]],
  //   goldParaphrasesOpt: Option[StateSnapshot[VerbParaphraseLabels]]
  // ) = {
  //   TagMod(
  //     argStructureChoiceOpt.value match {
  //       case None => goldParaphrasesOpt.whenDefined(_ => ^.onClick --> argStructureChoiceOpt.setState(Some(structure)))
  //       case Some(`structure`) => ^.onClick --> argStructureChoiceOpt.setState(None)
  //       case Some(otherStructure) => goldParaphrasesOpt.whenDefined { goldParaphrases =>
  //         val paraphrases = goldParaphrases.zoomStateL(VerbParaphraseLabels.paraphrases)
  //           ^.onClick ==> (
  //             (e: ReactMouseEvent) => e.preventDefaultCB >> (
  //               if(e.altKey) {
  //                 if(paraphrases.value.apart(otherStructure, structure)) paraphrases.modState(_.unseparate(otherStructure, structure))
  //                 else paraphrases.modState(_.separate(otherStructure, structure))
  //               } else {
  //                 if(paraphrases.value.equal(otherStructure, structure)) paraphrases.modState(_.unequate(otherStructure, structure))
  //                 else paraphrases.modState(_.equate(otherStructure, structure))
  //               }
  //             )
  //           )
  //       }
  //     },
  //     paraphrasingHighlightStyle(structure, argStructureChoiceOpt.value.orElse(argStructureHoverOpt.value), goldParaphrasesOpt.map(_.value)),
  //     (^.onMouseMove --> argStructureHoverOpt.setState(Some(structure))).when(argStructureHoverOpt.value.isEmpty),
  //     ^.onMouseOut --> argStructureHoverOpt.setState(None)
  //   )
  // }

  def sentenceSelectionPane(
    sentenceIds: SortedSet[String],
    curSentenceId: StateSnapshot[String]
  ) = {
    val sentencesWord = if(sentenceIds.size == 1) "sentence" else "sentences"
    val sentenceCountLabel = s"${sentenceIds.size} $sentencesWord"

    <.div(S.sentenceSelectionPaneContainer)(
      <.div(S.sentenceCountLabel)(
        <.span(S.sentenceCountLabelText)(
          sentenceCountLabel
        )
      ),
      <.div(S.sentenceSelectionPane)(
        sentenceIds.toVdomArray { sentenceId =>
          <.div(S.sentenceSelectionEntry)(
            ^.key := sentenceId,
            if(sentenceId == curSentenceId.value) S.currentSelectionEntry else S.nonCurrentSelectionEntry,
            ^.onClick --> curSentenceId.setState(sentenceId),
            <.span(S.sentenceSelectionEntryText)(
              sentenceId
              // Text.render(sentence.sentenceTokens) // TODO maybe show sentence text
            )
          )
        }
      )
    )
  }


  def sentenceDisplayPane(
    verb: VerbType,
    sentence: SentenceInfo[VerbType, Arg],
    features: FeatureValues
  ) = {
    val sortedVerbs = sentence.verbs.values.toList.sortBy(_.index)
    val (currentVerbs, otherVerbs) = sortedVerbs.partition(_.verbType == verb)
    val currentVerbIndices = currentVerbs.map(_.index).toSet
    IntSetLocal.make(initialValue = currentVerbIndices) { highlightedVerbIndices =>
      // val spansOpt = features.argSpans
      val answerSpansWithColors = for {
        (verb, index) <- currentVerbs.zipWithIndex
        if highlightedVerbIndices.value.contains(verb.index)
        verbId = VerbId(sentence.sentenceId, verb.index)
        (span, prob) <- features.argSpans.foldMap(spans =>
          verb.args.unorderedFoldMap(arg => spans(ArgumentId(verbId, arg)))
        ).toList
      } yield span -> highlightLayerColors(index % highlightLayerColors.size).copy(a = prob / 4)
      val verbColorMap = currentVerbs.zipWithIndex.map { case (verb, index) =>
          verb.index -> highlightLayerColors(index % highlightLayerColors.size)
      }.toMap

      <.div(S.sentenceDisplayPane)(
        <.div(S.sentenceTextContainer)(
          <.span(S.sentenceText)(
            // Text.render(sentence.tokens)
            View.renderSentenceWithHighlights(
              sentence.tokens,
              View.RenderWholeSentence(answerSpansWithColors),
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
                      ^.onMouseOut --> highlightedVerbIndices.setState(currentVerbIndices)
                    )
                  )
                )
              }
            )
          )
        ),
        <.div(S.verbEntriesContainer)(
          currentVerbs.toVdomArray { verb =>
            val verbId = VerbId(sentence.sentenceId, verb.index)
            val color = verbColorMap(verb.index)
            <.div(S.verbEntryDisplay)(
              <.div(
                <.a(
                  ^.name := s"verb-${verb.index}",
                  ^.display := "block",
                  ^.position := "relative",
                  ^.visibility := "hidden"
                )
              ),
              <.div(S.verbHeading)(
                <.span(S.verbHeadingText)(
                  ^.color := color.copy(a = 1.0).toColorStyleString,
                  // ^.onClick --> (
                  //   navQuery.setState(
                  //     DatasetQuery(
                  //       verb.verbInflectedForms.allForms.toSet,
                  //       Set(SentenceId.fromString(curSentence.sentenceId).documentId.toString.lowerCase),
                  //       Set(curSentence.sentenceId.lowerCase)
                  //     )
                  //   )
                  // ),
                  verb.verbType.toString
                )
              ),
              <.table(S.verbQAsTable)( // arg table
                <.tbody(S.verbQAsTableBody)(
                  verb.args.toVector.sorted.toVdomArray(arg =>
                    <.tr(
                      <.td(Arg.toString(sentence, arg)),
                      features.argSpans.whenDefined(argSpans =>
                        NonEmptyList.fromList(argSpans(ArgumentId(verbId, arg)).toList).whenDefined(spansNel =>
                          <.td(
                            View.makeAllHighlightedAnswer(sentence.tokens, spansNel.map(_._1), color)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )(
              S.hoverHighlightedVerbTable.when(highlightedVerbIndices.value == Set(verb.index)),
              ^.key := verb.index,
              ^.onMouseMove --> (
                if(highlightedVerbIndices.value == Set(verb.index)) {
                  Callback.empty
                } else highlightedVerbIndices.setState(Set(verb.index))
              ),
              ^.onMouseOut --> highlightedVerbIndices.setState(currentVerbIndices)
            )
          },
          <.div(S.verbEntryDisplay)(
            "Other verbs: ",
            otherVerbs.map(verb =>
              Vector(
                <.span(
                  ^.fontWeight := "bold",
                  verb.verbType.toString
                )
              )
            ).intercalate(Vector(<.span(", "))).toVdomArray
          )
        )
      )
    }
  }

  def unsafeListAt[A](index: Int) =
    Lens[List[A], A](s => s(index))(a => s => s.updated(index, a))

  def lensProduct[A, B, C](l1: Lens[A, B], l2: Lens[A, C]) =
    Lens[A, (B, C)](a => l1.get(a) -> l2.get(a)) {
      case (b, c) => a => l2.set(c)(l1.set(b)(a))
    }

  def throughOption[A, B](l: Lens[A, Option[B]])(implicit M: Monoid[B]): Lens[A, B] = {
    Lens[A, B](a => l.get(a).combineAll)(
      b => a => if(b == M.empty) l.set(None)(a) else l.set(Some(b))(a)
    )
  }

  // TODO color-code the answer spans by _question_ instead of by verb

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

  @Lenses case class FeatureOptions(
    questionDist: Boolean,
    argIndex: Boolean,
    argSpans: Boolean,
    argMlmDist: Option[String],
    verbMlmDist: Option[String]
  )
  object FeatureOptions {
    val mlmTypes = Set("masked", "symm_left", "symm_right", "symm_both")
    def init = FeatureOptions(false, false, false, None, None)
  }

  @Lenses case class FeatureValues(
    verbType: VerbType,
    questionDist: Option[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]],
    argIndex: Option[Map[ArgumentId[Arg], Int]],
    argSpans: Option[Map[ArgumentId[Arg], Map[ESpan, Double]]],
    argMlmDist: Option[Map[ArgumentId[Arg], Map[String, Double]]],
    verbMlmDist: Option[Map[VerbId, Map[String, Double]]]
  )
  object FeatureValues {
    def empty(verbType: VerbType) = FeatureValues(verbType, None, None, None, None, None)
  }

  val OptionalStringSelect = new View.OptionalSelect[String](x => x, "-")
  val FeatureOptionsLocal = new LocalState[FeatureOptions]

  import scala.util.{Try, Success, Failure}

  object VerbFeatures {
    case class Props(
      initVerb: VerbType,
      featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
      render: (StateSnapshot[FeatureOptions], StateSnapshot[VerbType], FeatureValues) => VdomElement
    )

    @Lenses case class State(
      options: FeatureOptions,
      features: FeatureValues
    )
    object State {
      def initial(verb: VerbType) = State(FeatureOptions.init, FeatureValues.empty(verb))
    }

    def pullFeature[A](
      featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
      verbFeatures: StateSnapshot[State],
      req: FeatureReq[VerbType, Arg] { type Out = A },
      getLens: FeatureOptions => (Lens[FeatureValues, Option[A]], Boolean)
    ): Callback = {
      val (valueLens, includeFeature) = getLens(verbFeatures.value.options)
      val featureSnap = verbFeatures.zoomStateL(State.features.composeLens(valueLens))
      if(!includeFeature) {
        if(featureSnap.value.isEmpty) Callback.empty else featureSnap.setState(None)
      } else {
        featureService(req).cata(
          pure = feats => if(featureSnap.value != feats) featureSnap.setState(Some(feats)) else Callback.empty,
          wrapped = _.completeWith {
            case Success(feats) => featureSnap.setState(Some(feats))
            case Failure(err) => Callback(err.printStackTrace)
          }
        )
      }
    }

    class Backend(scope: BackendScope[Props, State]) {

      def pullFeatures(
        featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
      ): Callback = scope.state >>= { state =>
        pullFeature(
          featureService, StateSnapshot(state).setStateVia(scope),
          FeatureReq.QuestionDists(state.features.verbType),
          opts => (FeatureValues.questionDist, opts.questionDist)
        ) >>
        pullFeature(
          featureService, StateSnapshot(state).setStateVia(scope),
          FeatureReq.ArgSpans(state.features.verbType),
          opts => (FeatureValues.argSpans, opts.argSpans)
        )
      }

      def render(props: Props, state: State) = {
        val optionsSnap = StateSnapshot(state.options) { (optionsOpt, cb) => 
          val fullCb = cb >> pullFeatures(props.featureService)
          scope.modStateOption(s => optionsOpt.map(State.options.set(_)(s)), fullCb)
        }
        val verbSnap = StateSnapshot(state.features.verbType) { (verbOpt, cb) =>
          val fullCb = cb >> pullFeatures(props.featureService)
          scope.modStateOption(s => verbOpt.map(State.features.composeLens(FeatureValues.verbType).set(_)(s)), fullCb)
        }
        props.render(
          optionsSnap,
          verbSnap,
          state.features
        )
      }
    }

    val Component = ScalaComponent.builder[Props]("Verb Features")
      .initialStateFromProps(p => State.initial(p.initVerb))
      .renderBackend[Backend]
      .build

    def make(
      initVerb: VerbType,
      featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg])(
      render: (StateSnapshot[FeatureOptions], StateSnapshot[VerbType], FeatureValues) => VdomElement
    ) = Component(Props(initVerb, featureService, render))
  }

  def headerContainer(
    featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    sortedVerbCounts: List[(VerbType, Int)],
    verb: StateSnapshot[VerbType],
    verbFeatures: StateSnapshot[FeatureOptions]
  ) = {
    <.div(S.headerContainer)(
      <.select(S.verbDropdown)(
        ^.value := VerbType.toString(verb.value),
        ^.onChange ==> ((e: ReactEventFromInput) =>
          verb.setState(VerbType.fromString(e.target.value))
        ),
        sortedVerbCounts.toVdomArray { case (verb, count) =>
          <.option(
            ^.key := VerbType.toString(verb),
            ^.value := VerbType.toString(verb),
            f"$count%5d ${VerbType.toString(verb)}"
          )
        }
      ),
      <.div(S.featureOptions)(
        View.checkboxToggle("Questions", verbFeatures.zoomStateL(FeatureOptions.questionDist)),
        View.checkboxToggle("Arg index", verbFeatures.zoomStateL(FeatureOptions.argIndex)),
        View.checkboxToggle("Arg spans", verbFeatures.zoomStateL(FeatureOptions.argSpans)),
        <.span(S.labeledDropdown)(
          <.span(S.labeledDropdownLabel)("Arg MLM:"),
          OptionalStringSelect(
            FeatureOptions.mlmTypes,
            verbFeatures.zoomStateL(FeatureOptions.argMlmDist)
          )
        ),
        <.span(S.labeledDropdown)(
          <.span(S.labeledDropdownLabel)("Verb MLM:"),
          OptionalStringSelect(
            FeatureOptions.mlmTypes,
            verbFeatures.zoomStateL(FeatureOptions.verbMlmDist)
          )
        )
      )
    )
  }

  val defaultClusterSplittingSpec = ClusterSplittingSpec(
    ClusterSplittingCriterion.Number(1),
    ClusterSplittingCriterion.Number(5)
  )

  // def makeSurrogateFrame(structure: ArgStructure, forms: InflectedForms, useModal: Boolean) = {
  //   Frame(
  //     forms, structure.args,
  //     tense = (if(useModal) Modal("might".lowerCase) else PresentTense),
  //     isPassive = structure.isPassive,
  //     isPerfect = false,
  //     isProgressive = false,
  //     isNegated = false
  //   )
  // }

  def frameContainer(
    verbService: VerbFrameService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    cachedClusterSplittingSpec: StateSnapshot[ClusterSplittingSpec],
    verbFeatures: FeatureValues
  ) = {
    val verb = verbFeatures.verbType
    VerbModelFetch.make(
      request = verb,
      sendRequest = verb => verbService.getModel(verb)) {
      case VerbModelFetch.Loading => <.div(S.loadingNotice)("Loading verb clusters...")
      case VerbModelFetch.Loaded(model) =>
        val numVerbInstances = model.verbClusterTree.size.toInt
        ClusterSplittingSpecLocal.make(initialValue = cachedClusterSplittingSpec.value) { clusterSplittingSpec =>
          // def isClauseProbabilityAcceptable(p: Double) = true || p >= 0.01 || paraphrasingFilter.value.minClauseProb <= p

          val verbTrees = clusterSplittingSpec.value.verbCriterion.splitTree[Set[VerbId]](model.verbClusterTree, _.size.toDouble)
          val verbIndices = verbTrees.zipWithIndex.flatMap { case (tree, index) =>
            tree.values.flatMap(verbIds => verbIds.toVector.map(_ -> index))
          }.toMap
          // TODO: split down to how it was during verb clustering, then *possibly* re-cluster.
          val argTrees = model.argumentClusterTreeOpt
            .map(_.group(argIds => argIds.groupBy(argId => verbIndices(argId.verbId))))
            .getOrElse(Map())


          <.div(S.framesetContainer)(
            <.div(S.clusterSplittingSpecDisplay)(
              clusterCriterionField("Verb", clusterSplittingSpec.zoomStateL(ClusterSplittingSpec.verbCriterion)),
              clusterCriterionField("Argument", clusterSplittingSpec.zoomStateL(ClusterSplittingSpec.argumentCriterion)),
              <.div(
                <.button(
                  "cache",
                  ^.onClick --> cachedClusterSplittingSpec.setState(clusterSplittingSpec.value)
                )
              )
              // <.div(f"Max Verb Loss/instance: ${maxLoss / numInstances}%.3f")
            ),
            <.div(S.frameSpecDisplay, S.scrollPane) {
              verbTrees.zipWithIndex.toVdomArray { case (verbTree, frameIndex) =>
                val argTree = argTrees(frameIndex)
                val roleTrees = clusterSplittingSpec.value.argumentCriterion
                  .splitTree[Set[ArgumentId[Arg]]](argTree, _.size.toDouble)
                val numInstances = verbTree.size.toInt
                val frameProb = numInstances.toDouble / numVerbInstances
                val isFrameChosen = false // TODO
                <.div(S.frameContainer, S.chosenFrameContainer.when(isFrameChosen))(
                  ^.key := "frame-" + frameIndex.toString,
                  <.div(S.frameHeading, S.chosenFrameHeading.when(isFrameChosen))(
                    <.span(S.frameHeadingText)(
                      f"Frame $frameIndex%s (${frameProb}%.3f)"
                    )
                  ),
                  <.div(S.clauseSetDisplay)(
                    roleTrees.sortBy(-_.size).toVdomArray { roleTree =>
                      val questionDistsOpt = verbFeatures.questionDist.map { dists =>
                        roleTree.unorderedFoldMap(_.unorderedFoldMap(dists))
                      }

                      <.div(
                        <.div(s"Arg: ${roleTree.size} instances."),
                        questionDistsOpt.whenDefined(questionDists =>
                          questionDists.toVector.sortBy(-_._2).toVdomArray { case (template, prob) =>
                            <.div(f"$prob%.3f ", template.toTemplateString)
                          }
                        )
                      )
                    }
                  )
                )
              }

              // val frameList = frameset.frames.zipWithIndex
              // frameList.toVdomArray { case (frame, frameIndex) =>
              //   val isFrameChosen = {
              //     val bools = for {
              //       sentence <- sentenceOpt.toList
              //       verbIndex <- verbIndices.toList
              //     } yield frame.verbIds.contains(VerbId(sentence.sentenceId, verbIndex))
              //     bools.exists(identity)
              //   }
              //   val frameLens = VerbFrameset.frames
              //     .composeLens(unsafeListAt[VerbFrame](frameIndex))
              //   val roleClusters = paraphrasingFilter.value.questionCriterion.splitTree(frame.questionClusterTree)
              //   // clause -> slot -> role -> sorted qids
              //   val argMappings: Map[ArgStructure, Map[ArgumentSlot, Map[Int, SortedSet[ArgumentId[Arg]]]]] = {
              //     roleClusters.zipWithIndex.foldMap { case (tree, roleIndex) =>
              //       tree.unorderedFoldMap { case qid @ ArgumentId(_, question) =>
              //         Map(question.clauseTemplate -> Map(question.slot -> Map(roleIndex -> SortedSet(qid))))
              //       }
              //     }
              //   }
              //   val baseArgSigils = Vector("X", "Y", "Z", "A", "B", "C")
              //   val argSigils = baseArgSigils ++ (2 to 9).toVector.flatMap(i =>
              //     baseArgSigils.map(_ + i.toString)
              //   )
              //   val getArgSigil = argSigils(_)

              //   val frameSentenceDocPairsOpt = (docIdToDocMetaOpt, sentenceOpt).mapN { (docIdToDocMeta, sentence) =>
              //     (frame.verbIds.map(vid => SentenceId.fromString(vid.sentenceId)).toSet + SentenceId.fromString(sentence.sentenceId)).toList
              //       .map(sid => sid -> docIdToDocMeta(sid.documentId))
              //       .sorted(
              //         Order.catsKernelOrderingForOrder(
              //           Order.whenEqual[(SentenceId, DocumentMetadata)](
              //             Order.by[(SentenceId, DocumentMetadata), String](_._2.title),
              //             Order.by[(SentenceId, DocumentMetadata), SentenceId](_._1)
              //           )
              //         )
              //       )
              //   }

              //   def makeNavQueryForSentenceIndexOpt(index: Int) = {
              //     frameSentenceDocPairsOpt.map { allSentencesForFrame =>
              //       val sid = allSentencesForFrame(index)._1
              //       val sidStr = SentenceId.toString(sid)
              //       val docIdStr = sid.documentId.toString
              //       DatasetQuery(verbInflectedForms.allForms.toSet, Set(docIdStr.lowerCase), Set(sidStr.lowerCase))
              //     }
              //   }
              //   val curSentenceIndexOpt = (frameSentenceDocPairsOpt, sentenceIdOpt).mapN { (frameSentenceDocPairs, sentenceId) =>
              //     frameSentenceDocPairs
              //       .zipWithIndex
              //       .find(t => t._1._1 == sentenceId)
              //       .map(_._2)
              //   }.flatten
              //   def makePrevQuery = (frameSentenceDocPairsOpt, curSentenceIndexOpt).mapN { (frameSentenceDocPairs, curSentenceIndex) =>
              //     makeNavQueryForSentenceIndexOpt(
              //       (curSentenceIndex - 1 + frameSentenceDocPairs.size) % frameSentenceDocPairs.size
              //     )
              //   }.flatten
              //   def makeNextQuery = (frameSentenceDocPairsOpt, curSentenceIndexOpt).mapN { (frameSentenceDocPairs, curSentenceIndex) =>
              //     makeNavQueryForSentenceIndexOpt(
              //       (curSentenceIndex + 1) % frameSentenceDocPairs.size
              //     )
              //   }.flatten

              //   def goToPrev(ids: SortedSet[ArgumentId[ClausalQuestion]]) = {
              //     sentenceIdOpt.foldMap { sentenceId =>
              //       val querySentenceIds = {
              //         val sids = ids.map(qid => SentenceId.fromString(qid.verbId.sentenceId))
              //         (sids + sentenceId).toList
              //       }
              //       (querySentenceIds.last :: querySentenceIds).zip(querySentenceIds).find(
              //         _._2 == sentenceId
              //       ).map(_._1).foldMap(newSid =>
              //         navQuery.setState(
              //           DatasetQuery(
              //             verbInflectedForms.allForms.toSet,
              //             Set(newSid.documentId.toString.lowerCase),
              //             Set(SentenceId.toString(newSid).lowerCase)
              //           )
              //         )
              //       )
              //     }
              //   }
              //   def goToNext(ids: SortedSet[ArgumentId[ClausalQuestion]]) = {
              //     sentenceIdOpt.foldMap { sentenceId =>
              //       val querySentenceIds = {
              //         val sids = ids.map(qid => SentenceId.fromString(qid.verbId.sentenceId))
              //         (sids + sentenceId).toList
              //       }
              //       (querySentenceIds.last :: querySentenceIds).zip(querySentenceIds).find(
              //         _._1 == sentenceId
              //       ).map(_._2).foldMap(newSid =>
              //         navQuery.setState(
              //           DatasetQuery(
              //             verbInflectedForms.allForms.toSet,
              //             Set(newSid.documentId.toString.lowerCase),
              //             Set(SentenceId.toString(newSid).lowerCase)
              //           )
              //         )
              //       )
              //     }
              //   }
              //   def sigilNavigationMod(ids: SortedSet[ArgumentId[ClausalQuestion]]) = TagMod(
              //     ^.onClick ==> ((e: ReactMouseEvent) =>
              //       if(e.altKey) goToPrev(ids) else goToNext(ids)
              //     )
              //   )

              //   <.div(S.frameContainer, S.chosenFrameContainer.when(isFrameChosen))(
              //     ^.key := "clause-set-" + frameIndex.toString,
              //     <.div(S.frameHeading, S.chosenFrameHeading.when(isFrameChosen))(
              //       <.span(S.frameHeadingText)(
              //         f"Frame $frameIndex%s (${frame.probability}%.4f)"
              //       ),
              //       makePrevQuery.whenDefined(goToPrev =>
              //         <.span(S.prevFrameInstanceText)(
              //           " (prev)",
              //           ^.onClick --> navQuery.setState(goToPrev))
              //       ),
              //       makeNextQuery.whenDefined(goToNext =>
              //         <.span(S.prevFrameInstanceText)(
              //           " (next)",
              //           ^.onClick --> navQuery.setState(goToNext))
              //       )
              //     ),
              //     <.div(S.clauseSetDisplay)(
              //       frame.clauseTemplates.zipWithIndex
              //         .filter(p => isClauseProbabilityAcceptable(p._1.probability))
              //         .sortBy(-_._1.probability)
              //         .toVdomArray { case (frameClause, clauseIndex) =>
              //           val numQuestions = argMappings(frameClause.args).unorderedFoldMap(_.unorderedFoldMap(_.size))
              //           val surrogateFrame = makeSurrogateFrame(frameClause.args, verbInflectedForms, useModal = false)

              //           <.div(S.clauseDisplay, S.matchingClause.when(predictedParaphraseClauseTemplatesOpt.exists(_.contains(frameClause.args))))(
              //             ^.key := "clause-" + clauseIndex.toString,
              //             <.div(
              //               goldParaphrasesOpt.whenDefined { goldParaphrases =>
              //                 val clauseCorrectLens = VerbParaphraseLabels.correctClauses.composeLens(Optics.at(frameClause.args))
              //                 val clauseIncorrectLens = VerbParaphraseLabels.incorrectClauses.composeLens(Optics.at(frameClause.args))
              //                 val clauseCorrectness = goldParaphrases.zoomStateL(lensProduct(clauseCorrectLens, clauseIncorrectLens))
              //                   <.div(S.goldClauseMarkerDisplay)(
              //                     <.label(S.goldClauseCheckLabel)(
              //                       <.input(
              //                         ^.`type` := "checkbox",
              //                         ^.value := clauseCorrectness.value._1,
              //                         ^.onChange ==> ((e: ReactEventFromInput) =>
              //                           if(clauseCorrectness.value._1) clauseCorrectness.setState(false -> false)
              //                           else clauseCorrectness.setState(true -> false)
              //                         )
              //                       ),
              //                       <.div(S.goldClauseCheck, S.goldClauseCheckCorrect.when(clauseCorrectness.value._1))
              //                     ),
              //                     <.label(S.goldClauseXLabel)(
              //                       <.input(
              //                         ^.`type` := "checkbox",
              //                         ^.value := clauseCorrectness.value._2,
              //                         ^.onChange ==> ((e: ReactEventFromInput) =>
              //                           if(clauseCorrectness.value._2) clauseCorrectness.setState(false -> false)
              //                           else clauseCorrectness.setState(false -> true)
              //                         )
              //                       ),
              //                       <.div(S.goldClauseX, S.goldClauseXIncorrect.when(clauseCorrectness.value._2))
              //                     )
              //                   )
              //               },
              //               <.span(S.shiftedClauseTemplateDisplay.when(goldParaphrasesOpt.nonEmpty))(
              //                 <.span(f"(${frameClause.probability}%.2f) "),
              //                 surrogateFrame.clausesWithArgMarkers.head.zipWithIndex.map {
              //                   case (Left(s), i) => <.span(^.key := s"frame-clause-$i", s)
              //                   case (Right(argSlot), i) => <.span(
              //                     ^.key := s"frame-clause-$i",
              //                     BoolLocal.make(initialValue = false) { isEditingSlot =>
              //                       val sigilSuffix = surrogateFrame.args.get(argSlot).get match {
              //                         case Noun(_) => ""
              //                         case Prep(p, _) =>
              //                           if(p.toString.contains("doing")) "[ng]"
              //                           else if(p.toString.contains(" do")) "[inf]"
              //                           else ""
              //                         case Locative => "[where]"
              //                       }
              //                       val genericGoldMatchingMod = S.genericGoldMatchingArgMarker.when(
              //                         goldStructureRelationOpt.exists(_.preimage(frameClause.args -> argSlot).nonEmpty)
              //                       )
              //                       val selectionMod = tagModForStructureLabel(
              //                         frameClause.args -> argSlot, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt
              //                       )
              //                       def getSigilSpan(roleIndex: Int, ids: SortedSet[ArgumentId[ClausalQuestion]]): VdomElement = {
              //                         val goldMatchingMod = S.goldMatchingArgMarker.when(
              //                           sentenceOpt.exists(sent => ids.exists(_.verbId.sentenceId == sent.sentenceId))
              //                         )

              //                         <.span(
              //                           S.argSigil, genericGoldMatchingMod,
              //                           goldMatchingMod, /* predMatchingMod, */
              //                           S.sigilProportionalColor((ids.size.toDouble / numQuestions * 20).toInt),
              //                           sigilNavigationMod(ids))(
              //                           getArgSigil(roleIndex) + sigilSuffix
              //                         )
              //                       }
              //                       def getRoleSpan(roleCounts: Map[Int, SortedSet[ArgumentId[ClausalQuestion]]]) = {
              //                         <.span(selectionMod)(
              //                           if(roleCounts.size == 1) {
              //                             val (roleIndex, ids) = roleCounts.head
              //                             getSigilSpan(roleIndex, ids)
              //                           } else {
              //                             val argSigils = roleCounts.toList.map(Function.tupled(getSigilSpan(_, _)))
              //                               <.span(
              //                                 "[",
              //                                 argSigils.map(Vector(_))
              //                                   .intercalate(Vector(<.span(" / ")))
              //                                   // .zipWithIndex
              //                                   // .map { case (x, i) => x(^.key := s"sigil-$i") }
              //                                   .toVdomArray,
              //                                 "]"
              //                               )
              //                           }
              //                         )
              //                       }

              //                       argMappings.get(frameClause.args).flatMap(_.get(argSlot)).map { roleCounts =>
              //                         getRoleSpan(roleCounts)
              //                       }.getOrElse {
              //                         <.span(S.argPlaceholder, genericGoldMatchingMod, /* predMatchingMod, */ selectionMod){
              //                           val prefix = surrogateFrame.args.get(argSlot) match {
              //                             case Some(Prep(p, _)) if p.endsWith(" doing".lowerCase) => "doing "
              //                             case Some(Prep(p, _)) if p == "do".lowerCase || p.endsWith(" do".lowerCase) => "do "
              //                             case _ => ""
              //                           }
              //                           prefix + surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
              //                         }
              //                       }
              //                       // TODO integrate these into the common tag mod
              //                       // val predMatchingMod = S.predMatchingArgMarker.when(
              //                       //   predStructureRelationOpt.exists(_.preimage(frameClause.args -> argSlot).nonEmpty)
              //                       // )

              //                       // argMappings.get(frameClause.args).flatMap(_.get(argSlot)).map(s =>
              //                       //   <.span(S.argSigil, goldMatchingMod, /* predMatchingMod, */selectionMod)(s + sigilSuffix): VdomElement
              //                       // ).getOrElse(
              //                       //   <.span(S.argPlaceholder, goldMatchingMod, /* predMatchingMod, */ selectionMod){
              //                       //     val prefix = surrogateFrame.args.get(argSlot) match {
              //                       //       case Some(Prep(p, _)) if p.endsWith(" doing".lowerCase) => "doing "
              //                       //       case Some(Prep(p, _)) if p == "do".lowerCase || p.endsWith(" do".lowerCase) => "do "
              //                       //       case _ => ""
              //                       //     }
              //                       //     prefix + surrogateFrame.args.get(argSlot).get.placeholder.mkString(" ")
              //                       //   }
              //                       // )
              //                     }
              //                   )
              //                 }.map(List(_)).intercalate(List(<.span(" "))).zipWithIndex.toVdomArray(p => p._1(^.key := s"frame-clause-tok-${p._2}"))
              //               ),
              //               <.div(S.adverbialRoles)(
              //                 argMappings.get(frameClause.args).whenDefined { argSlotToRoleCounts =>
              //                   argSlotToRoleCounts.toVector.collect { case (argSlot @ Adv(wh), roleCounts) =>
              //                     val genericGoldMatchingMod = S.genericGoldMatchingArgMarker.when(
              //                       goldStructureRelationOpt.exists(_.preimage(frameClause.args -> argSlot).nonEmpty)
              //                     )
              //                     val selectionMod = tagModForStructureLabel(
              //                       frameClause.args -> argSlot, argStructureChoiceOpt, argStructureHoverOpt, goldParaphrasesOpt
              //                     )
              //                     def getSigilSpan(roleIndex: Int, ids: SortedSet[ArgumentId[ClausalQuestion]]): VdomElement = {
              //                       val goldMatchingMod = S.goldMatchingArgMarker.when(
              //                         sentenceOpt.exists(sent => ids.exists(_.verbId.sentenceId == sent.sentenceId))
              //                       )

              //                       <.span(
              //                         S.argSigil, genericGoldMatchingMod, goldMatchingMod, /* predMatchingMod, */selectionMod,
              //                         S.sigilProportionalColor((ids.size.toDouble / numQuestions * 20).toInt),
              //                         sigilNavigationMod(ids))(
              //                         getArgSigil(roleIndex)
              //                       )
              //                     }
              //                     def getRoleSpan(roleCounts: Map[Int, SortedSet[ArgumentId[ClausalQuestion]]]) = {
              //                       if(roleCounts.size == 1) {
              //                         val (roleIndex, ids) = roleCounts.head
              //                         getSigilSpan(roleIndex, ids)
              //                       } else {
              //                         val argSigils = roleCounts.toList.map(Function.tupled(getSigilSpan(_, _)))
              //                           <.span(
              //                             "[",
              //                             argSigils.map(Vector(_))
              //                               .intercalate(Vector(<.span(" / ")))
              //                               // .zipWithIndex
              //                               // .map { case (x, i) => x(^.key := s"sigil-$i") }
              //                               .toVdomArray,
              //                             "]"
              //                           )
              //                       }
              //                     }

              //                     <.span(S.adverbialRole)(
              //                       <.span(S.adverbialRoleAdverb)(s"$wh: "),
              //                       getRoleSpan(roleCounts)
              //                     )
              //                   }.toVdomArray
              //                 }
              //               )
              //             )
              //           )
              //         }
              //     )
              //   )
              // }
            }
          )
        }
    }
  }

  def sentenceContainer(
    featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    verbFeatures: FeatureValues
  ) = {
    <.div(S.dataContainer)(
      SentencesFetch.make(
        request = verbFeatures.verbType,
        sendRequest = verb => featureService(FeatureReq.Sentences(verb))) {
        case SentencesFetch.Loading => <.div(S.loadingNotice)("Loading sentence IDs...")
        case SentencesFetch.Loaded(_sentenceIds) =>
          // TODO use sorted set
          import scala.collection.immutable.TreeSet
          val sentenceIds: SortedSet[String] = TreeSet(_sentenceIds.toSeq: _*)
          val initSentenceId = sentenceIds.head
          StringLocal.make(initialValue = initSentenceId) { curSentenceId =>
            <.div(S.dataContainer)(
              sentenceSelectionPane(
                sentenceIds,
                curSentenceId
              ),
              SentenceFetch.make(
                request = curSentenceId.value,
                sendRequest = sid => featureService(FeatureReq.Sentence(sid))) {
                case SentenceFetch.Loading => <.div(S.loadingNotice)("Loading sentence...")
                case SentenceFetch.Loaded(sentenceInfo) =>
                  sentenceDisplayPane(
                    verbFeatures.verbType,
                    sentenceInfo,
                    verbFeatures
                  )
              }
            )
          }
      }
    )
  }

  class Backend(scope: BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      VerbsFetch.make(request = (), sendRequest = _ => props.verbService.getVerbs) {
        case VerbsFetch.Loading => <.div(S.loadingNotice)("Waiting for verb data...")
        case VerbsFetch.Loaded(verbCounts) =>
          val sortedVerbCounts = verbCounts.toList.sortBy(p => -p._2 -> VerbType.toString(p._1))
          val initVerb = sortedVerbCounts(scala.math.min(sortedVerbCounts.size - 1, 10))._1
          ClusterSplittingSpecLocal.make(initialValue = defaultClusterSplittingSpec) { cachedClusterSplittingSpec =>
            VerbFeatures.make(initVerb, props.featureService) { (options, verb, features) =>
              <.div(S.mainContainer)(
                headerContainer(props.featureService, sortedVerbCounts, verb, options),
                <.div(S.dataContainer)(
                  frameContainer(props.verbService, cachedClusterSplittingSpec, features),
                  sentenceContainer(props.featureService, features)
                )
              )
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
