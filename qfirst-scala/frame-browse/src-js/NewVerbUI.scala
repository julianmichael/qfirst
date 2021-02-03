package qfirst.frame.browse

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
import qfirst.frame._
import qfirst.frame.math._
import qfirst.frame.eval._

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
import scalacss.internal.Literal.Typed.initial

import com.cibo.evilplot.geometry.CanvasRenderContext
import com.cibo.evilplot.geometry.Drawable
import com.cibo.evilplot.geometry.Extent

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

  val genericVerbForms = InflectedForms.fromStrings("verb", "verbs", "verbing", "verbed", "verben")

  val sentenceIdOrder = Order.by[String, (String, Int)](id =>
    if(id.contains(":")) {
      val index = id.lastIndexOf(":")
      id.substring(0, index) -> id.substring(index + 1).toInt
    } else id -> -1
  )

  implicit val callbackMonoid = new Monoid[Callback] {
    override def empty = Callback.empty
    override def combine(x: Callback, y: Callback) = x >> y
  }

  val S = VerbAnnStyles

  val argSigilLetters = Stream.from(1).flatMap { i =>
    val lets = List("X", "Y", "Z", "A", "B", "C", "D", "E", "F")
    if(i == 1) lets else lets.map(l => s"$l$i")
  }

  val sigilBaseColors = Vector(
    Rgba(220,   0,   0, 1.0),
    Rgba(  0, 220,   0, 1.0),
    Rgba(  0,   0, 220, 1.0),
    Rgba(180, 180,   0, 1.0),
    Rgba(  0, 180, 180, 1.0),
    Rgba(180,   0, 180, 1.0),
    Rgba(  0,   0,   0, 1.0),
    Rgba(128, 128, 128, 1.0)
  )
  val argSigilColors = Stream.from(0).map { i =>
    val n = sigilBaseColors.size
    sigilBaseColors((i + n) % n)
  }

  val argSigils = argSigilLetters.zip(argSigilColors).map { case (let, col) =>
    <.span(
      S.argSigil,
      ^.color := col.toColorStyleString,
      let
    )
  }

  import HOCs._

  val VerbsFetch = new CacheCallContent[Unit, Map[VerbType, Int]]
  val VerbModelFetch = new CacheCallContent[VerbType, VerbClusterModel[VerbType, Arg]]
  val SentencesFetch = new CacheCallContent[VerbType, Set[String]]
  val SentenceFetch = new CacheCallContent[String, SentenceInfo[VerbType, Arg]]
  val InflectedFormSetFetch = new CacheCallContent[VerbType, List[InflectedForms]]

  val DataFetch = new CacheCallContent[Unit, (DataIndex, Map[VerbType, Int])]
  val DocFetch = new CacheCallContent[DocumentId, Document]
  val DocOptFetch = new CacheCallContent[Option[DocumentId], Option[Document]]
  val SearchFetch = new CacheCallContent[Search.Query, Set[DocumentId]]
  // val FramesetFetch = new CacheCallContent[InflectedForms, VerbFrameset]
  val EvalItemFetch = new CacheCallContent[Int, ParaphrasingInfo]
  val IntLocal = new LocalState[Int]
  val IntOptLocal = new LocalState[Option[Int]]
  val VerbModelLocal = new LocalState[Option[VerbClusterModel[VerbType, Arg]]]
  val DocMetaOptLocal = new LocalState[Option[DocumentMetadata]]
  val SentOptLocal = new LocalState[Option[Sentence]]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]
  val IntSetLocal = new LocalState[Set[Int]]
  val FrameChoiceLocal = new LocalState[Set[(ArgumentId[Arg], ArgStructure, ArgumentSlot)]]
  val QuestionSetLocal = new LocalState[Set[ArgumentId[Arg]]]
  val InflectedFormsLocal = new LocalState[Option[InflectedForms]]

  val ClusterSplittingSpecLocal = new LocalState[ClusterSplittingSpec]
  val GoldParaphrasesLocal = new LocalState[VerbParaphraseLabels]
  val DoubleLocal = new LocalState[Double]

  val CanvasRef = new Reference[dom.html.Canvas]

  val colspan = VdomAttr("colspan")
  val textFocusingRef = Ref[html.Element]

  // val ArgStructureOptLocal = new LocalState[Option[(ArgStructure, ArgumentSlot)]]

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

  case class Props(
    verbService: VerbFrameService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    // urlNavQuery: NavQuery,
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

  @Lenses case class FeatureOptions(
    questionDist: Boolean,
    argIndex: Boolean,
    argSyntFunc: Boolean,
    argSpans: Boolean,
    argPrepositions: Boolean,
    argConstituentTypes: Option[String],
    argMlmDist: Option[String],
    argPrepMlmDist: Option[String],
    verbMlmDist: Option[String],
    goldLabels: Boolean
  )
  object FeatureOptions {
    val constituentTypes = Set("ptb", "stripped")
    val mlmTypes = Set("masked", "repeated", "symm_left", "symm_right", "symm_both")
    def init = FeatureOptions(false, false, false, false, false, None, None, None, None, false)
  }

  @Lenses case class FeatureValues(
    verbType: VerbType,
    questionDist: Option[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]],
    argIndex: Option[Map[ArgumentId[Arg], Int]],
    argSyntFunc: Option[Map[ArgumentId[Arg], String]],
    argSpans: Option[Map[ArgumentId[Arg], Map[ESpan, Double]]],
    argPrepositions: Option[Map[ArgumentId[Arg], Map[String, Double]]],
    argConstituentTypes: Option[Map[ArgumentId[Arg], Map[String, Double]]],
    argMlmDist: Option[Map[ArgumentId[Arg], Map[String, Float]]],
    argPrepMlmDist: Option[Map[ArgumentId[Arg], Map[String, Float]]],
    verbMlmDist: Option[Map[VerbId, Map[String, Float]]],
    goldLabels: Option[Option[GoldVerbInfo[Arg]]]
  ) {
    // TODO cache more effectively
    val questionPrior = questionDist.map(_.unorderedFold)
  }
  object FeatureValues {
    def empty(verbType: VerbType) = FeatureValues(verbType, None, None, None, None, None, None, None, None, None, None)
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

    class Backend(scope: BackendScope[Props, State]) {

      def pullFeature[A, B](
        featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
        getLens: FeatureOptions => (Lens[FeatureValues, Option[A]], B),
        makeReq: B => Option[FeatureReq[VerbType, Arg] { type Out = A }]
      ): Callback = scope.state >>= { state =>
        val (valueLens, featureSpec) = getLens(state.options)
        def updateFeature(fOpt: Option[A]) = scope
          .zoomStateL(State.features.composeLens(valueLens))
          .setState(fOpt)
        val curFeat = valueLens.get(state.features)
        makeReq(featureSpec) match {
          case None => if(curFeat.isEmpty) Callback.empty else updateFeature(None)
          case Some(req) => featureService(req).cata(
            pure = feats => if(curFeat != Some(feats)) updateFeature(Some(feats)) else Callback.empty,
            wrapped = _.completeWith {
              // don't use snapshot, which may be out of date here
              case Success(feats) => updateFeature(Some(feats))
              case Failure(err) => Callback(err.printStackTrace)
            }
          )
        }
      }

      def pullFeatures(
        featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
      ): Callback = scope.state >>= { state =>
        pullFeature(
          featureService,
          opts => (FeatureValues.questionDist, opts.questionDist),
          (b: Boolean) => Option(
            FeatureReq.QuestionDists[VerbType, Arg](state.features.verbType)
          ).filter(_ => b)
        ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argIndex, opts.argIndex),
            (b: Boolean) => Option(
              FeatureReq.ArgIndices[VerbType, Arg](state.features.verbType)
            ).filter(_ => b)
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argSyntFunc, opts.argSyntFunc),
            (b: Boolean) => Option(
              FeatureReq.ArgSyntacticFunctions[VerbType, Arg](state.features.verbType)
            ).filter(_ => b)
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argSpans, opts.argSpans),
            (b: Boolean) => Option(
              FeatureReq.ArgSpans[VerbType, Arg](state.features.verbType)
            ).filter(_ => b)
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argPrepositions, opts.argPrepositions),
            (b: Boolean) => Option(
              FeatureReq.ArgPrepositions[VerbType, Arg](state.features.verbType)
            ).filter(_ => b)
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argConstituentTypes, opts.argConstituentTypes),
            (label: Option[String]) => label.map(l =>
              FeatureReq.ArgConstituentTypes[VerbType, Arg](state.features.verbType, l)
            )
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argMlmDist, opts.argMlmDist),
            (label: Option[String]) => label.map(l =>
              FeatureReq.ArgMLMDist[VerbType, Arg](state.features.verbType, l)
            )
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.argPrepMlmDist, opts.argPrepMlmDist),
            (label: Option[String]) => label.map(l =>
              FeatureReq.ArgPrepMLMDist[VerbType, Arg](state.features.verbType, l)
            )
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.verbMlmDist, opts.verbMlmDist),
            (label: Option[String]) => label.map(l =>
              FeatureReq.VerbMLMDist[VerbType, Arg](state.features.verbType, l)
            )
          ) >>
          pullFeature(
            featureService,
            opts => (FeatureValues.goldLabels, opts.goldLabels),
            (b: Boolean) => Option(
              FeatureReq.GoldLabels[VerbType, Arg](state.features.verbType)
            ).filter(_ => b)
          )
      }

      def render(props: Props, state: State) = {
        // def resetValues(vt: VerbType) = scope.modState(State.features.set(FeatureValues.empty(vt)))
        val optionsSnap = StateSnapshot(state.options) { (optionsOpt, cb) => 
          val fullCb = cb >> pullFeatures(props.featureService)
          scope.modStateOption(s => optionsOpt.map(State.options.set(_)(s)), fullCb)
        }
        val verbSnap = StateSnapshot(state.features.verbType) { (verbOpt, cb) =>
          val fullCb = cb >> pullFeatures(props.featureService)
          // clear values before fetching new ones
          scope.modStateOption(s => verbOpt.map(vt => State.features.set(FeatureValues.empty(vt))(s)), fullCb)
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
    // might make sense to add if we want to start with some features.
    // .componentDidMount($ => $.backend.pullFeatures($.props.featureService))

    def make(
      initVerb: VerbType,
      featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg])(
      render: (StateSnapshot[FeatureOptions], StateSnapshot[VerbType], FeatureValues) => VdomElement
    ) = Component(Props(initVerb, featureService, render))
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

  def headerContainer(
    featureService: FeatureService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    sortedVerbCounts: List[(VerbType, Int)],
    verb: StateSnapshot[VerbType],
    verbFeatures: StateSnapshot[FeatureOptions],
    showMetrics: StateSnapshot[Boolean]
  ) = {
    <.div(S.headerContainer)(
      <.div(S.headerColumn)(
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
        <.select(S.verbDropdown)(
          ^.value := VerbType.toString(verb.value),
          ^.onChange ==> ((e: ReactEventFromInput) =>
            verb.setState(VerbType.fromString(e.target.value))
          ),
          sortedVerbCounts.toVdomArray { case (verb, count) =>
            <.option(
              ^.key := VerbType.toString(verb),
              ^.value := VerbType.toString(verb),
              f"${VerbType.toString(verb)} ($count)"
            )
          }
        )
      ),
      <.div(S.featureOptions)(
        <.div(S.headerColumn)(
          View.checkboxToggle("Questions", verbFeatures.zoomStateL(FeatureOptions.questionDist)),
          View.checkboxToggle("Arg syntf", verbFeatures.zoomStateL(FeatureOptions.argSyntFunc)),
        ),
        <.div(S.headerColumn)(
          View.checkboxToggle("Arg index", verbFeatures.zoomStateL(FeatureOptions.argIndex)),
          View.checkboxToggle("Arg spans", verbFeatures.zoomStateL(FeatureOptions.argSpans)),
        ),
        View.checkboxToggle("Preps", verbFeatures.zoomStateL(FeatureOptions.argPrepositions)),
        <.div(S.headerColumn)(
          <.span(S.labeledDropdown)(
            <.span(S.labeledDropdownLabel)("Arg ctypes:"),
            OptionalStringSelect(
              FeatureOptions.constituentTypes,
              verbFeatures.zoomStateL(FeatureOptions.argConstituentTypes)
            )
          ),
          <.span(S.labeledDropdown)(
            <.span(S.labeledDropdownLabel)("Arg MLM:"),
            OptionalStringSelect(
              FeatureOptions.mlmTypes,
              verbFeatures.zoomStateL(FeatureOptions.argMlmDist)
            )
          )
        ),
        <.div(S.headerColumn)(
          <.span(S.labeledDropdown)(
            <.span(S.labeledDropdownLabel)("Arg Prep MLM:"),
            OptionalStringSelect(
              FeatureOptions.mlmTypes,
              verbFeatures.zoomStateL(FeatureOptions.argPrepMlmDist)
            )
          ),
          <.span(S.labeledDropdown)(
            <.span(S.labeledDropdownLabel)("Verb MLM:"),
            OptionalStringSelect(
              FeatureOptions.mlmTypes,
              verbFeatures.zoomStateL(FeatureOptions.verbMlmDist)
            )
          )
        ),
        <.div(S.headerColumn)(
          View.checkboxToggle("Gold labels", verbFeatures.zoomStateL(FeatureOptions.goldLabels)),
          View.checkboxToggle("Metrics", showMetrics),
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

  def questionDistributionTable(
    inflectedForms: InflectedForms,
    dist: Map[QuestionTemplate, Double]
  ) = {
    val total = dist.unorderedFold
    <.table(S.questionDistTable)(
      <.tbody(
        dist.toVector
          .sortBy(-_._2)
          .takeWhile(_._2 / total >= 0.05)
          .map { case (qt, prob) =>
            <.tr(
              ^.key := qt.toString,
              <.td(S.questionProbCell)(f"${prob/total}%.2f"),
              <.td(qt.toSlots.renderQuestionString(inflectedForms.apply))
            )
          }.toVdomArray
      )
    )
  }

  def goldLabelDistDisplay(
    counts: Map[String, Int]
  ) = {
    val total = counts.unorderedFold
    val dist = counts.mapVals(_.toDouble / total)

    <.div(S.mlmItemsBlock)(
      dist.toVector.sortBy(-_._2).toVdomArray { case (label, prob) =>
        val clampedProb = scala.math.min(1.0, prob + 0.1)

        <.span(S.mlmItemText)(
          ^.color := Rgba(0, 0, 0, clampedProb).toColorStyleString,
          ^.onClick --> Callback(println(f"$label%s: $prob%.5f")),
          f"$label%s"
        )
      }
    )
  }

  def distributionDisplay[N](
    counts: Map[String, N],
    numToShow: Int = 20)(
    implicit N: Numeric[N]
  )= {
    val total = N.toDouble(counts.values.sum)
    val dist = counts.mapVals(v => N.toDouble(v) / total)
    val topItems = dist.toVector.sortBy(-_._2).take(numToShow)

    <.div(S.mlmItemsBlock)(
      topItems.toVdomArray { case (word, prob) =>
        val sanitizedWord = if(word == "-PRON-") "<pro>" else word
        val clampedProb = scala.math.min(1.0, prob + 0.1)
          <.span(S.mlmItemText)(
            ^.color := Rgba(0, 0, 0, clampedProb).toColorStyleString,
            ^.onClick --> Callback(println(f"$sanitizedWord%s: $prob%.5f")),
            f"$sanitizedWord%s"
          )
      }
    )
  }

  class ResolvedFrame private (
    val verbTree: MergeTree[Set[VerbId]],
    val roleTrees: Vector[MergeTree[Set[ArgumentId[Arg]]]],
    val extraRoles: Map[String, Set[ArgumentId[Arg]]]
  ) {
    val sents = {
      val base = verbTree.unorderedFold
        .map(_.sentenceId).toVector
        .sorted(sentenceIdOrder.toOrdering)

      base.headOption.fold(base)(base :+ _)
    }
    val roleSents = roleTrees.map { roleTree =>
      val base = roleTree.unorderedFold
        .map(_.verbId.sentenceId).toVector
        .sorted(sentenceIdOrder.toOrdering)

      base.headOption.fold(base)(base :+ _)
    }
    val extraRoleSents = extraRoles.mapVals { argIds =>
      val base = argIds.map(_.verbId.sentenceId).toVector
      .sorted(sentenceIdOrder.toOrdering)

      base.headOption.fold(base)(base :+ _)
    }

    def nextSentence(id: String): Option[String] = sents
      .find(x => sentenceIdOrder.gt(x, id))
      .orElse(sents.headOption)
    def prevSentence(id: String): Option[String] = sents.sliding(2)
      .find(x => sentenceIdOrder.gteqv(x(1), id))
      .map(_(0))
      .orElse(sents.lastOption)

    def nextSentenceForRole(roleId: Either[Int, String], id: String): Option[String] = roleId match {
      case Left(i) => nextSentenceForRole(i, id)
      case Right(x) => nextSentenceForNamedRole(x, id)
    }
    def prevSentenceForRole(roleId: Either[Int, String], id: String): Option[String] = roleId match {
      case Left(i) => prevSentenceForRole(i, id)
      case Right(x) => prevSentenceForNamedRole(x, id)
    }

    def nextSentenceForRole(roleIndex: Int, id: String): Option[String] = {
      val xs = roleSents(roleIndex)
      xs.find(x => sentenceIdOrder.gt(x, id))
        .orElse(xs.headOption)
    }
    def prevSentenceForRole(roleIndex: Int, id: String): Option[String] = {
      val xs = roleSents(roleIndex)
      xs.sliding(2)
        .find(x => sentenceIdOrder.gteqv(x(1), id))
        .map(_(0)).orElse(xs.lastOption)
    }
    def nextSentenceForNamedRole(roleName: String, id: String): Option[String] = {
      val xs = extraRoleSents(roleName)
      xs.find(x => sentenceIdOrder.gt(x, id))
        .orElse(xs.headOption)
    }
    def prevSentenceForNamedRole(roleName: String, id: String): Option[String] = {
      val xs = extraRoleSents(roleName)
      xs.sliding(2)
        .find(x => sentenceIdOrder.gteqv(x(1), id))
        .map(_(0)).orElse(xs.lastOption)
    }
  }
  object ResolvedFrame {
    def apply(
      verbTree: MergeTree[Set[VerbId]],
      roleTrees: Vector[MergeTree[Set[ArgumentId[Arg]]]],
      extraRoles: Map[String, Set[ArgumentId[Arg]]]
    ): ResolvedFrame = new ResolvedFrame(verbTree, roleTrees.sortBy(-_.size), extraRoles)
  }

  def framesetDisplay(
    verbFeatures: FeatureValues,
    inflectedForms: InflectedForms,
    frames: Vector[ResolvedFrame],
    curSentenceId: StateSnapshot[String],
    curHighlightedFrame: StateSnapshot[Option[Int]]
  ) = {
    val numVerbInstances = frames.foldMap(_.verbTree.size.toInt)
    <.div(S.frameSpecDisplay, S.scrollPane) {
      frames.zipWithIndex.toVdomArray { case (frame, frameIndex) =>
        val verbTree = frame.verbTree
        val numInstances = verbTree.size.toInt
        val frameProb = numInstances.toDouble / numVerbInstances
        val numberedRoles = frame.roleTrees.zipWithIndex.map { case (tree, index) =>
          Left(index) -> tree.unorderedFold
        }
        val namedRoles = frame.extraRoles.toList.map { case (name, argIds) =>
          Right(name) -> argIds
        }
        val allRoles = numberedRoles ++ namedRoles
        val isFrameChosen = false // TODO
        val isFrameHighlighted = curHighlightedFrame.value.exists(_ == frameIndex)
        <.div(S.frameContainer, S.chosenFrameContainer.when(isFrameChosen))(
          ^.onMouseMove --> (
            if(isFrameHighlighted) Callback.empty
            else curHighlightedFrame.setState(Some(frameIndex))
          ),
          ^.onMouseOut --> curHighlightedFrame.setState(None),
          S.highlightedFrame.when(isFrameHighlighted),
          ^.key := "frame-" + frameIndex.toString,
          <.div(S.frameHeading, S.chosenFrameHeading.when(isFrameChosen))(
            <.span(S.frameHeadingText)(
              f"Frame $frameIndex%s (${frameProb}%.3f)"
            ),
            <.span(S.prevFrameInstanceText)(
              ^.onClick --> frame.prevSentence(curSentenceId.value).foldMap(curSentenceId.setState),
              " prev "),
            <.span(S.prevFrameInstanceText)(
              ^.onClick --> frame.nextSentence(curSentenceId.value).foldMap(curSentenceId.setState),
              " next ")
          ),
          verbFeatures.goldLabels.flatten.whenDefined { goldLabels =>
            val counts = verbTree.unorderedFoldMap(_.unorderedFoldMap(verbId => Map(goldLabels.verbSenses(verbId) -> 1)))
            goldLabelDistDisplay(counts)
          },
          verbFeatures.verbMlmDist.whenDefined { dists =>
            val senseCounts = verbTree.unorderedFoldMap(_.unorderedFoldMap(dists))
            distributionDisplay(senseCounts)
          },
          <.div(S.clauseSetDisplay)(
            allRoles.toVdomArray { case (nameOrIndex, argIds) =>
              val syntFCountsOpt = verbFeatures.argSyntFunc.map(syntFuncs =>
                argIds.unorderedFoldMap(argId => Map(syntFuncs(argId) -> 1))
              )
              val ctypeDistOpt = verbFeatures.argConstituentTypes.map { dists =>
                argIds.unorderedFoldMap(dists)
              }
              val prepDistOpt = verbFeatures.argPrepositions.map { dists =>
                argIds.unorderedFoldMap(dists)
              }
              val mlmDistOpt = verbFeatures.argMlmDist.map { dists =>
                argIds.unorderedFoldMap(dists)
              }
              val prepMlmDistOpt = verbFeatures.argPrepMlmDist.map { dists =>
                argIds.unorderedFoldMap(dists)
              }
              val questionDistOpt = verbFeatures.questionDist.map { dists =>
                argIds.unorderedFoldMap(dists)
              }
              val sigil = nameOrIndex match {
                case Left(i) => argSigils(i)
                case Right(name) => <.span(
                  S.argSigil,
                  name
                )
              }

              <.div(S.roleDisplay)(
                <.div(
                  sigil, s": ${argIds.size} instances.",
                  <.span(S.prevFrameInstanceText)(
                    ^.onClick --> frame.prevSentenceForRole(nameOrIndex, curSentenceId.value).foldMap(curSentenceId.setState),
                    " prev "),
                  <.span(S.prevFrameInstanceText)(
                    ^.onClick --> frame.nextSentenceForRole(nameOrIndex, curSentenceId.value).foldMap(curSentenceId.setState),
                    " next ")
                ),
                verbFeatures.goldLabels.flatten.whenDefined { goldLabels =>
                  val counts = argIds.unorderedFoldMap(argId => Map(goldLabels.argRoles(argId).role -> 1))
                  goldLabelDistDisplay(counts)
                },
                syntFCountsOpt.whenDefined(goldLabelDistDisplay(_)),
                ctypeDistOpt.whenDefined(distributionDisplay(_)),
                mlmDistOpt.whenDefined(distributionDisplay(_)),
                prepDistOpt.whenDefined(distributionDisplay(_)),
                prepMlmDistOpt.whenDefined(distributionDisplay(_)),
                questionDistOpt.whenDefined { questionDist =>
                  questionDistributionTable(inflectedForms, questionDist)
                }
              )
            }
          )
        )
      }
    }
  }

  def frameContainer(
    verbService: VerbFrameService[OrWrapped[AsyncCallback, ?], VerbType, Arg],
    cachedClusterSplittingSpec: StateSnapshot[ClusterSplittingSpec],
    clusterSplittingSpec: StateSnapshot[ClusterSplittingSpec],
    allInflectedForms: List[InflectedForms],
    inflectedForms: StateSnapshot[Option[InflectedForms]],
    verbFeatures: FeatureValues,
    frames: Vector[ResolvedFrame],
    curSentenceId: StateSnapshot[String],
    curHighlightedFrame: StateSnapshot[Option[Int]]
  ) = {
    // def isClauseProbabilityAcceptable(p: Double) = true || p >= 0.01 || paraphrasingFilter.value.minClauseProb <= p

    <.div(S.framesetContainer)(
      inflectedForms.value match {
        case None => <.div("No inflected forms available.")
        case Some(forms) => View.select[InflectedForms](S.verbDropdown)(
          _.allForms.mkString(", "), allInflectedForms, forms, f => inflectedForms.setState(Some(f))
        ),
      },
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
      framesetDisplay(verbFeatures, inflectedForms.value.getOrElse(genericVerbForms), frames, curSentenceId, curHighlightedFrame)
    )
  }

  def sentenceSelectionPane(
    sentenceIds: List[String],
    curSentenceId: StateSnapshot[String]
  ) = {
    val sentencesWord = if(sentenceIds.size == 1) "sentence" else "sentences"
    val sentenceCountLabel = s"${sentenceIds.size} $sentencesWord"

    <.div(S.sentenceSelectionPaneContainer)(
      <.div(S.sentenceCountLabel)(
        <.span(S.sentenceCountLabelText)(
          sentenceCountLabel,
          " ",
          <.a(
            ^.href := "#",
            ^.onClick --> {
              CallbackTo((new scala.util.Random).nextInt(sentenceIds.size)).flatMap(
                index => curSentenceId.setState(sentenceIds(index))
              )
            },
            "(random)"
          )
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
            )
          )
        }
      )
    )
  }

  @Lenses case class TFIDFConfig(
    use: Boolean,
    priorSmoothingLambda: Double,
    headProbabilityMass: Double)
  object TFIDFConfig {
    def init = TFIDFConfig(false, 0.5, 0.95)
  }
  val TFIDFConfigLocal = new LocalState[TFIDFConfig]

  def sentenceDisplayPane(
    verb: VerbType,
    sentence: SentenceInfo[VerbType, Arg],
    features: FeatureValues,
    inflectedForms: InflectedForms,
    frames: Vector[ResolvedFrame],
    curHighlightedFrame: StateSnapshot[Option[Int]]
  ) = {
    val sortedVerbs = sentence.verbs.values.toList.sortBy(_.index)
    val (currentVerbs, otherVerbs) = sortedVerbs.partition(_.verbType == verb)
    val currentVerbIndices = currentVerbs.map(_.index).toSet
    TFIDFConfigLocal.make(initialValue = TFIDFConfig.init) { tfidfConfig =>
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
          <.span(S.sentenceInfoText)(sentence.sentenceId),
          features.goldLabels.whenDefined(goldLabelsOpt =>
            <.div(S.sentenceInfoContainer)(
              "No gold labels available."
            ).when(goldLabelsOpt.isEmpty)
          ),
          features.questionDist.whenDefined(_ =>
            <.div(
              <.div(View.checkboxToggle("Use TF-IDF", tfidfConfig.zoomStateL(TFIDFConfig.use))),
              <.div(
                View.sliderField("Prior smoothing", 0.0, 2.0, tfidfConfig.zoomStateL(TFIDFConfig.priorSmoothingLambda))
              ).when(tfidfConfig.value.use),
              <.div(
                View.sliderField("Head size", 0.0, 1.0, tfidfConfig.zoomStateL(TFIDFConfig.headProbabilityMass))
              ).when(tfidfConfig.value.use)
            )
          ),
          <.div(S.sentenceTextContainer)(
            <.span(S.sentenceText)(
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
              val (frame, frameIndex) = frames.zipWithIndex
                .find(_._1.verbTree.exists(_.contains(verbId))).get
              val isFrameHighlighted = curHighlightedFrame.value.exists(_ == frameIndex)
              val rolesWithIndices = frame.roleTrees.zipWithIndex
              val getRoleId = verb.args.iterator.map { arg =>
                val argId = ArgumentId(verbId, arg)
                argId -> rolesWithIndices.find(_._1.exists(_.contains(argId))).map(p => Left(p._2))
                  .orElse(frame.extraRoles.iterator.find(_._2.contains(argId)).map(p => Right(p._1)))
              }.toMap
              <.div(S.verbEntryDisplay)(
                S.highlightedFrame.when(isFrameHighlighted),
                ^.onMouseMove --> (
                  if(isFrameHighlighted) Callback.empty
                  else curHighlightedFrame.setState(Some(frameIndex))
                ),
                ^.onMouseOut --> curHighlightedFrame.setState(None),
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
                    verb.verbType.toString,
                  ),
                  features.goldLabels.flatten.whenDefined { goldLabels =>
                    val sense = goldLabels.verbSenses(verbId)
                    val lemma = if(sense.contains(".")) {
                      sense.substring(0, sense.lastIndexOf("."))
                    } else sense

                    <.span(
                      " ",
                      <.a(
                        ^.href := s"http://verbs.colorado.edu/propbank/framesets-english-aliases/$lemma.html#$sense",
                        ^.target := "_blank",
                        sense
                      )
                    )
                  }
                ),
                features.verbMlmDist.whenDefined { dists =>
                  distributionDisplay(dists(verbId))
                },
                <.table(S.verbQAsTable)( // arg table
                  <.tbody(S.verbQAsTableBody)(
                    verb.args.toVector.sorted.flatMap(arg =>
                      List(
                        <.tr(S.argFirstRow)(
                          getRoleId(ArgumentId(verbId, arg)).whenDefined(roleId =>
                            roleId match {
                              case Left(i) => <.td(argSigils(i))
                              case Right(x) => <.td(<.span(S.argSigil, x))
                            }
                          ),
                          features.goldLabels.flatten.whenDefined(goldLabels =>
                            <.td(goldLabels.argRoles(ArgumentId(verbId, arg)).role)
                          ),
                          <.td(Arg.toString(sentence, arg)),
                          features.argIndex.whenDefined(argIndices =>
                            <.td(<.i(sentence.tokens(argIndices(ArgumentId(verbId, arg)))))
                          ),
                          features.argSyntFunc.whenDefined(argSyntFuncs =>
                            <.td(argSyntFuncs(ArgumentId(verbId, arg)))
                          ),
                          features.argSpans.whenDefined(argSpans =>
                            NonEmptyList.fromList(argSpans(ArgumentId(verbId, arg)).toList)
                              .whenDefined(spansNel =>
                                <.td(
                                  View.makeAllHighlightedAnswer(sentence.tokens, spansNel.map(_._1), color)
                                )
                              )
                          )
                        ),
                        <.tr(
                          <.td(
                            ^.colSpan := 6,
                            features.argConstituentTypes.whenDefined { dists =>
                              distributionDisplay(dists(ArgumentId(verbId, arg)))
                            }
                          )
                        ),
                        <.tr(
                          <.td(
                            ^.colSpan := 6,
                            features.argMlmDist.whenDefined { dists =>
                              distributionDisplay(dists(ArgumentId(verbId, arg)))
                            }
                          )
                        ),
                        <.tr(
                          <.td(
                            ^.colSpan := 6,
                            features.argPrepMlmDist.whenDefined { dists =>
                              distributionDisplay(dists(ArgumentId(verbId, arg)))
                            }
                          )
                        ),
                        features.questionDist.whenDefined { questionDist =>
                          val initDist = questionDist(ArgumentId(verbId, arg))
                          val dist = features.questionPrior
                            .filter(_ => tfidfConfig.value.use)
                            .fold(initDist) { prior =>
                              import TFIDF._
                              val truncatedDist = truncate(
                                initDist, tfidfConfig.value.headProbabilityMass
                              )
                              val smoothedPrior = addLambda(
                                prior, tfidfConfig.value.priorSmoothingLambda
                              )
                              rebalance(truncatedDist, smoothedPrior)
                            }

                          <.tr(
                            <.td(
                              ^.colSpan := 6,
                              questionDistributionTable(inflectedForms, dist)
                            )
                          )
                        }
                      )
                    ): _*
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
  }

  class Backend(scope: BackendScope[Props, State]) {

    val npmiChartRef = Ref[dom.html.Canvas]

    def npmiChart(
      features: FeatureValues,
      frames: Vector[ResolvedFrame],
    ) = features.goldLabels.flatten.whenDefined { goldLabels =>
      val clusters = frames.flatMap { frame =>
        frame.roleTrees.map(_.unorderedFold) ++ frame.extraRoles.values
      }
      val goldCounts = clusters.map(instances =>
        instances.unorderedFoldMap(id =>
          Map(goldLabels.argRoles(id).role -> 1)
        )
      )

      val npmis = EvalUtils.calculateNPMIs(goldCounts)(EvalUtils.conll08RoleOrder, implicitly[Numeric[Int]])
      val plot = Plotting.plotNPMI(npmis, Some("Normalized PMIs"))(
        EvalUtils.conll08RoleOrder, implicitly[cats.Show[String]]
      )

      val bCubedByLabel = ClusterPRMetric.b3PerInstanceByLabel(
        goldCounts.combineAll, goldCounts
      )

      def prfCells(prf: WeightedPR, total: Double) = {
        val (p, r, f1) = prf.prf
        val pc = prf.pseudocount
        val err = (1.0 - f1) * (pc / total)
        List(
          <.td(S.numDataCell, S.cellProp((p * 20).toInt), S.cellRightBorder)(f"$p%.2f"),
          <.td(S.numDataCell, S.cellProp((r * 20).toInt), S.cellRightBorder)(f"$r%.2f"),
          <.td(S.numDataCell, S.cellProp((f1 * 20).toInt))(f"$f1%.2f"),
          <.td(S.numDataCell)(pc.toInt),
          <.td(S.numDataCell, S.errColor(scala.math.min(20.0, err * 40).toInt))(f"$err%.2f")
        )
      }


      def prfTable(
        metricName: String,
        metricByLabel: Map[String, WeightedPR]
      ) = {
        val total = metricByLabel.unorderedFoldMap(_.pseudocount)
        <.table(S.metricsTable)(
          <.thead(
            <.tr(S.rowBottomBorder)(
              <.td(),
              <.td(S.tableCell)("Prec."),
              <.td(S.tableCell)("Rec."),
              <.td(S.tableCell)("F1"),
              <.td(S.tableCell)("Count"),
              <.td(S.tableCell)("Error contribution")
            )
          ),
          <.tbody(
            <.tr(S.rowBottomBorder)(<.td(S.tableLeftHeader)("All"))(
              prfCells(metricByLabel.values.toList.combineAll, total): _*
            ),
            metricByLabel.toList.sortBy(-_._2.pseudocount).toVdomArray { case (label, prf) =>
              <.tr(<.td(S.tableLeftHeader)(label))(prfCells(prf, total): _*)
            }
          )
        )
      }

      val bCubedTable = <.div(
        prfTable("B3 P/R/F1", bCubedByLabel)
      )

      // val plotSize = 800.0
      // val extent = Extent(plotSize, plotSize)
      // val ctx = new SVGRenderContext
      // plot.render(extent).draw(ctx)
      // val plotSVG = ctx.getSVG(svgSize, svgSize)

      // plotSVG

      import com.cibo.evilplot.plot.aesthetics.DefaultTheme
      import com.cibo.evilplot.plot.aesthetics.Fonts
      implicit val theme = DefaultTheme.defaultTheme.copy(
        fonts = Fonts(
          titleSize = 88,
          labelSize = 80,
          annotationSize = 40,
          tickLabelSize = 96,
          legendLabelSize = 56,
          facetLabelSize = 56,
          fontFace = "sans-serif"
        ),
        elements = DefaultTheme.DefaultElements.copy(
          categoricalXAxisLabelOrientation = 1.0
        )
      )

      val mountCb = npmiChartRef.foreach { canvas =>
        val ogCtx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val ctx = CanvasRenderContext(ogCtx)
        val deviceRatio = dom.window.devicePixelRatio // js.Dynamic.global.window.dv
        canvas.width = 1600
        canvas.height = 1200
        canvas.style.width = (canvas.width / deviceRatio) + "px"
        canvas.style.height = (canvas.height / deviceRatio) + "px"
        // ogCtx.scale(deviceRatio, deviceRatio)
        val extent = Extent(canvas.width, canvas.height)
        plot.render(extent).draw(ctx)
      }

      val cb = npmiChartRef.foreach { canvas =>
        val ogCtx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val ctx = CanvasRenderContext(ogCtx)
        ctx.canvas.clearRect(0, 0, canvas.width, canvas.height)
        val extent = Extent(canvas.width, canvas.height)
        plot.render(extent).draw(ctx)
      }
      cb.runNow

      <.div(
        Mounting.make(mountCb)(
          <.canvas().withRef(npmiChartRef)
        ),
        bCubedTable,
      )

      // CanvasRef.make(
      //   <.canvas(
      //     ^.width := "400px",
      //     ^.height := "400px"
      //   )
      // ) { (canvasTag, canvasOpt) =>
      //   val cb = Callback {
      //     ()
      //     // canvasOpt
      //     //   .map(_.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
      //     //   .map(CanvasRenderContext(_))
      //     //   .foreach(ctx => Callback(plot.render().draw(ctx)))
      //   }

      //   <.div(canvasTag)

      //   // <.div(
      //   //   Mounting.make(cb)(
      //   //     <.div(canvasTag)
      //   //   )
      //   // )
      // }
    }

    def render(props: Props, state: State) = {
      VerbsFetch.make(request = (), sendRequest = _ => props.verbService.getVerbs) {
        case VerbsFetch.Loading => <.div(S.loadingNotice)("Waiting for verb data...")
        case VerbsFetch.Loaded(verbCounts) =>
          val sortedVerbCounts = verbCounts.toList.sortBy(p => -p._2 -> VerbType.toString(p._1))
          val initVerb = sortedVerbCounts(scala.math.min(sortedVerbCounts.size - 1, 10))._1
          ClusterSplittingSpecLocal.make(initialValue = defaultClusterSplittingSpec) { cachedClusterSplittingSpec =>
            VerbFeatures.make(initVerb, props.featureService) { (options, verb, features) =>
              InflectedFormSetFetch.make(
                request = verb.value,
                sendRequest = verb => props.featureService(FeatureReq.AllInflectedForms(verb))) {
                case InflectedFormSetFetch.Loading => <.div(S.loadingNotice)("Loading inflections...")
                case InflectedFormSetFetch.Loaded(formList) =>
                  VerbModelFetch.make(
                    request = verb.value,
                    sendRequest = verb => props.verbService.getModel(verb)) {
                    case VerbModelFetch.Loading => <.div(S.loadingNotice)("Loading verb clusters...")
                    case VerbModelFetch.Loaded(model) =>
                      InflectedFormsLocal.make(initialValue = formList.headOption) { inflectedForms =>
                        ClusterSplittingSpecLocal.make(initialValue = cachedClusterSplittingSpec.value) { clusterSplittingSpec =>
                          // assume only verb tree. no extra roles. can fix later if necessary
                          val verbTrees = clusterSplittingSpec.value.verbCriterion
                            .splitTree[Set[VerbId]](model.verbClustering.clusterTreeOpt.get, _.size.toDouble)
                          val verbIndices = verbTrees.zipWithIndex.flatMap { case (tree, index) =>
                            tree.values.flatMap(verbIds => verbIds.toVector.map(_ -> index))
                          }.toMap
                          // TODO: split down to how it was during verb clustering, then *possibly* re-cluster.
                          val argClusterings = model.argumentClustering.split(argId => verbIndices(argId.verbId))
                          val frames = verbTrees.zipWithIndex.map { case (verbTree, i) =>
                            argClusterings.get(i) match {
                              case None => ResolvedFrame(verbTree, Vector(), Map())
                              case Some(argClustering) =>
                                val roleTrees = argClustering.clusterTreeOpt.foldMap { argTree =>
                                  clusterSplittingSpec.value.argumentCriterion
                                    .splitTree[Set[ArgumentId[Arg]]](argTree, _.size.toDouble)
                                }
                                ResolvedFrame(verbTree, roleTrees, argClustering.extraClusters)
                            }
                          }

                          BoolLocal.make(false) { showMetrics =>
                            <.div(S.mainContainer)(
                              headerContainer(props.featureService, sortedVerbCounts, verb, options, showMetrics),
                              SentencesFetch.make(
                                request = features.verbType,
                                sendRequest = verb => props.featureService(FeatureReq.Sentences(verb))) {
                                case SentencesFetch.Loading => <.div(S.loadingNotice)("Loading sentence IDs...")
                                case SentencesFetch.Loaded(_sentenceIds) =>
                                  val sentenceIds = _sentenceIds.toList.sorted(sentenceIdOrder.toOrdering)
                                  val initSentenceId = sentenceIds.head
                                  StringLocal.make(initialValue = initSentenceId) { curSentenceId =>
                                    IntOptLocal.make(None) { curHighlightedFrame =>
                                      <.div(S.dataContainer)(
                                        frameContainer(
                                          props.verbService, cachedClusterSplittingSpec, clusterSplittingSpec,
                                          formList, inflectedForms,
                                          features, frames,
                                          curSentenceId,
                                          curHighlightedFrame
                                        ),
                                        if(showMetrics.value) <.div(S.dataContainer)(
                                          npmiChart(features, frames)
                                        ) else <.div(S.dataContainer)(
                                          sentenceSelectionPane(
                                            sentenceIds,
                                            curSentenceId
                                          ),
                                          SentenceFetch.make(
                                            request = curSentenceId.value,
                                            sendRequest = sid => props.featureService(FeatureReq.Sentence(sid))) {
                                            case SentenceFetch.Loading => <.div(S.loadingNotice)("Loading sentence...")
                                            case SentenceFetch.Loaded(sentenceInfo) =>
                                              sentenceDisplayPane(
                                                features.verbType,
                                                sentenceInfo,
                                                features,
                                                inflectedForms.value.getOrElse(genericVerbForms),
                                                frames,
                                                curHighlightedFrame
                                              )
                                          }
                                        )
                                      )
                                    }
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
