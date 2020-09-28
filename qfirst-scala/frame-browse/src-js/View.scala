package qfirst.frame.browse

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import cats.Order
import cats.implicits._

object View {
  val S = VerbAnnStyles
  import HOCs._

  // TODO add typed style argument
  def checkboxToggle(
    label: String,
    isValueActive: StateSnapshot[Boolean],
    didUpdate: Callback = Callback.empty
  ) = <.span(S.checkboxSpan)(
    <.input(S.checkbox)(
      ^.`type` := "checkbox",
      ^.value := label,
      ^.checked := isValueActive.value,
      ^.onChange --> (isValueActive.modState(!_) >> didUpdate)
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
      label.whenDefined(l => s" $l"),
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
    value: StateSnapshot[Double],
    onChange: Double => Callback = _ => Callback.empty
  ): VdomElement = liveTextField[Double](style)(
    label, value, (s: String) => scala.util.Try(s.toDouble).toOption
  )
  // def doubleTextField(style: TagMod)(
  //   label: String,
  //   value: StateSnapshot[Double]
  // ): VdomElement = doubleTextField(style)(Some(label), value)

  def intArrowField(style: TagMod)(
    label: Option[String],
    value: StateSnapshot[Int],
    onChange: Int => Callback = _ => Callback.empty
  ) = {
    <.span(style)(
      label.whenDefined(l => "$l: "),
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
      View.doubleTextField(S.shortTextField)(Some(label), value),
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

  // def liveSlider(
  //   value: StateSnapshot[Double],
  //   min: Double,
  //   max: Double,
  //   step: Double,
  //   fromValue: Double => Double,
  //   toValue: Double => Double,
  //   onChange: Double => Callback,
  // ) = {
  //   <.input(
  //     ^.`type` := "range",
  //     ^.min := min,
  //     ^.max := max,
  //     ^.step := step,
  //     ^.value := fromValue(value.value),
  //     ^.onChange ==> (
  //       (e: ReactEventFromInput) => {
  //         val newValue = toValue(e.target.value.toDouble)
  //         value.setState(newValue) >> onChange(newValue)
  //       })
  //   )
  // }

  class OptionalSelect[A: Order](
    show: A => String,
    none: String = "None") {

    def apply(
      choices: Set[A],
      curChoice: Option[A],
      setChoice: Option[A] => Callback
    ): TagOf[html.Select] = <.select(
      ^.value := curChoice.fold(none)(show),
      ^.onChange ==> (
        (e: ReactEventFrom[org.scalajs.dom.html.Select]) => {
          val valStr = e.target.value
          val value = {
            if(valStr == none) None
            else choices.find(c => show(c) == valStr)
          }
          if(value != curChoice) setChoice(value) else Callback.empty
        }
      ),
      <.option(^.key := none, ^.value := none, none),
      choices.toList.sorted.map(show).zipWithIndex.toVdomArray { case (c, i) =>
        <.option(^.key := s"$c-$i", ^.value := c, c)
      }
    )

    def apply(choices: Set[A], curChoice: StateSnapshot[Option[A]]): TagOf[html.Select] = {
      apply(choices, curChoice.value, curChoice.setState(_))
    }
  }

  import jjm.ling.ESpan
  import jjm.ling.Text

  import cats.data.NonEmptyList
  import cats.implicits._

  import scalajs.js

  val transparent = Rgba(255, 255, 255, 0.0)

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
}
