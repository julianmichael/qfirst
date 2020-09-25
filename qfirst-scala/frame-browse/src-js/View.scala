package qfirst.frame.browse

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import org.scalajs.dom.ext.KeyCode

object View {
  val S = VerbAnnStyles
  import HOCs._

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

}
