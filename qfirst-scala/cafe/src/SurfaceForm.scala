package qfirst.cafe

import qasrl.Tense

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import qasrl.labeling.SlotBasedLabel

sealed trait SurfaceForm extends Product with Serializable
object SurfaceForm {

  case class Excerpt(
    span: ESpan,
    text: String
  ) extends SurfaceForm

  case class Token(
    text: String,
    source: Option[Int] = None
  ) extends SurfaceForm


  // def fromQuestionSlots(
  //   verbIndex: Int,
  //   verbForms: InflectedForms,
  //   slots: SlotBasedLabel
  // ): Vector[Token] = {
  //   Vector(
  //     Token(slots.wh.toString)
  //   )
  //   // SlotBasedLabel.apply
  //   // wh, aux, subj, verbPrefix, verb, obj, prep, obj2

  // }
}
