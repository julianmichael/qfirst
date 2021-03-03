package qfirst.cafe

import qasrl.Tense

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

sealed trait SurfaceForm extends Product with Serializable
object SurfaceForm {

  case class Excerpt(span: ESpan) extends SurfaceForm

  case class Text(text: LowerCaseString) extends SurfaceForm

  case class Preposition(
    source: Option[Int], form: LowerCaseString
  ) extends SurfaceForm

  case class Participial(
    source: Option[Int], form: LowerCaseString
  ) extends SurfaceForm

  case class Copula(feats: SimpleClauseForm) extends SurfaceForm

  case class Verb(
    source: Option[Int], forms: InflectedForms, feats: SimpleClauseForm
  ) extends SurfaceForm
}

sealed trait SimpleClauseForm
object SimpleClauseForm {

  case class Aspect(
    isPerfect: Boolean,
    isProgressive: Boolean,
    isNegated: Boolean
  )

  // case object PastParticiple extends SimpleClauseForm
  case class BareInfinitive(aspect: Aspect) extends SimpleClauseForm
  case class ToInfinitive(aspect: Aspect) extends SimpleClauseForm
  case class Progressive(aspect: Aspect) extends SimpleClauseForm
  case class Finite(tense: Tense.Finite, aspect: Aspect) extends SimpleClauseForm
  // case class Inverted(tense: Tense.Finite, aspect: AspectNeg)
}
