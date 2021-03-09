package qfirst.cafe

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import qasrl.labeling.SlotBasedLabel
import qasrl.Tense

import cats.implicits._

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


  def fromQuestionSlots(
    verbIndex: Int,
    verbForms: InflectedForms,
    slots: SlotBasedLabel[VerbForm]
  ): Vector[Token] = {
    Vector(
      Vector(Token(slots.wh.toString)),
      slots.aux.map(x => Token(x.toString)).toVector,
      slots.subj.map(x => Token(x.toString)).toVector,
      slots.verbPrefix.map(x => Token(x.toString)).toVector,
      Vector(Token(verbForms.apply(slots.verb).toString, Some(verbIndex))),
      slots.obj.map(x => Token(x.toString)).toVector,
      slots.prep.foldMap(_.toString.split(" ").toVector.map(Token(_))),
      slots.obj2.foldMap(_.toString.split(" ").toVector.map(Token(_)))
    ).flatten
    // SlotBasedLabel.apply
    // wh, aux, subj, verbPrefix, verb, obj, prep, obj2

  }
}
