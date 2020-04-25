package qfirst.clause.align

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.{Encoder, Decoder}

sealed trait Tense2
object Tense2 {

  def fromTense(tense: qasrl.Tense): Tense2 = tense match {
    case qasrl.PastTense => Finite.Past
    case qasrl.PresentTense => Finite.Present
    case qasrl.Modal(m) => Finite.Modal(m)
  }

  sealed trait NonFinite extends Tense2 {
    import NonFinite._
    override def toString = this match {
      case Bare     => "bare"
      case To       => "to"
      case Gerund   => "gerund"
    }
  }

  object NonFinite {
    case object Bare extends NonFinite
    case object To extends NonFinite
    case object Gerund extends NonFinite

    def fromString(s: String): Option[NonFinite] = s match {
      case "bare"    => Some(Bare)
      case "to"      => Some(To)
      case "gerund"  => Some(Gerund)
      case _ => None
    }

    implicit val nonFiniteTense2Encoder: Encoder[NonFinite] =
      Encoder.encodeString.contramap[NonFinite](_.toString)
    implicit val nonFiniteTense2Decoder: Decoder[NonFinite] =
      Decoder.decodeString.emap(str =>
        fromString(str).toRight(s"Not a valid non-finite tense: $str")
      )
  }

  sealed trait Finite extends Tense2 {
    import Finite._
    override def toString = this match {
      case Past     => "past"
      case Present  => "present"
      case Modal(m) => m.toString
    }
  }

  object Finite {
    case object Past extends Finite
    case object Present extends Finite
    case class Modal(modalVerb: LowerCaseString) extends Finite
    object Modal {
      val modalVerbStrings: Set[String] =
        Set("can", "will", "might", "would", "should")
      val modalVerbs: Set[LowerCaseString] =
        modalVerbStrings.map(_.lowerCase)
    }

    def fromString(s: String): Option[Finite] = s match {
      case "past"    => Some(Past)
      case "present" => Some(Present)
      case m if Modal.modalVerbStrings.contains(m) => Some(Modal(m.lowerCase))
      case _ => None
    }

    implicit val finiteTense2Encoder: Encoder[Finite] =
      Encoder.encodeString.contramap[Finite](_.toString)
    implicit val finiteTense2Decoder: Decoder[Finite] =
      Decoder.decodeString.emap(str =>
        fromString(str).toRight(s"Not a valid finite tense: $str")
      )
  }
  import NonFinite._
  import Finite._

  implicit val tense2Encoder: Encoder[Tense2] =
    Encoder.encodeString.contramap[Tense2](_.toString)

  implicit val tense2Decoder: Decoder[Tense2] =
    NonFinite.nonFiniteTense2Decoder or Finite.finiteTense2Decoder.map(t => t: Tense2)
}
