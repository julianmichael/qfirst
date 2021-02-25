package qfirst.cafe

sealed trait ClauseType
object ClauseType {
  // past participle form --- passive/adjective
  case object Attributive extends ClauseType
  sealed trait VerbalClauseType extends ClauseType
  case object BareInfinitive extends VerbalClauseType
  case object ToInfinitive extends VerbalClauseType
  case object Progressive extends VerbalClauseType
  case object Finite extends VerbalClauseType
  case object Inverted extends VerbalClauseType

  val all = List(Attributive, BareInfinitive, ToInfinitive, Progressive, Finite, Inverted)
}
