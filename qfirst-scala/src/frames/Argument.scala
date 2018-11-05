package qfirst.frames

import nlpdata.util.LowerCaseStrings._

sealed trait Argument {
  def placeholder: List[String]
  def gap: List[String]
  def wh: Option[String]

  def isNoun: Boolean = this match {
    case Noun(_) => true
    case _       => false
  }

  def isPreposition: Boolean = this match {
    case Preposition(_, _) => true
    case _          => false
  }

  def isLocative: Boolean = this match {
    case Locative => true
    case _        => false
  }

  def isComplement: Boolean = this match {
    case Complement(_) => true
    case _ => false
  }
}

case class Preposition(
  preposition: LowerCaseString,
  objOpt: Option[NounLikeArgument]
) extends Argument {
  override def placeholder = preposition.toString :: objOpt.toList.flatMap(_.placeholder)
  override def gap = List(preposition.toString) ++ objOpt.toList.flatMap(_.gap)
  override def wh = objOpt.flatMap(_.wh)
}

sealed trait NonPrepArgument extends Argument

sealed trait NounLikeArgument extends NonPrepArgument

case class Noun(
  isAnimate: Boolean
) extends NounLikeArgument {
  override def placeholder = List(if (isAnimate) "someone" else "something")
  override def gap = Nil
  override def wh = if (isAnimate) Some("Who") else Some("What")
}

case object Locative extends NonPrepArgument {
  override def placeholder = List("somewhere")
  override def gap = Nil
  override def wh = Some("Where")
}

case object Gerund extends NounLikeArgument {
  override def placeholder = List("doing", "something")
  override def gap = List("doing")
  override def wh = Some("What")
}

case class Complement(conj: Complement.Form) extends NonPrepArgument {
  override def placeholder = conj.placeholder
  override def gap = conj.gap
  override def wh = conj.wh
}
object Complement {
  sealed trait Form {
    import Form._
    def placeholder = this match {
      case Infinitive => List("to", "do", "something")
      case Bare => List("do", "something")
    }
    def gap = this match {
      case Infinitive => List("to", "do")
      case Bare => List("do")
    }
    def wh = Some("What")
  }
  object Form {
    case object Infinitive extends Form
    case object Bare extends Form
  }
}
