package qfirst.cafe

import qasrl.Tense

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

sealed trait SurfaceForm extends Product with Serializable
object SurfaceForm {

  case class Excerpt(
    span: ESpan,
    text: String
  ) extends SurfaceForm

  case class Token(
    source: Option[Int],
    text: String
  ) extends SurfaceForm

  // case class Preposition(
  //   source: Option[Int], form: LowerCaseString
  // ) extends SurfaceForm

  // case class Participial(
  //   source: Option[Int], form: LowerCaseString
  // ) extends SurfaceForm

  // case class Copula(feats: SimpleClauseForm) extends SurfaceForm

  // case class Verb(
  //   source: Option[Int], forms: InflectedForms, feats: SimpleClauseForm
  // ) extends SurfaceForm
}
