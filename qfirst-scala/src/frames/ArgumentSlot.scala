package qfirst.frames

import nlpdata.util.LowerCaseStrings._

sealed trait ArgumentSlot { type Arg }
case object Subj extends ArgumentSlot { type Arg = Noun }
case object Obj extends ArgumentSlot { type Arg = Noun }
case object Prep1 extends ArgumentSlot { type Arg = Preposition }
case object Prep2 extends ArgumentSlot { type Arg = Preposition }
case object Misc extends ArgumentSlot { type Arg = NonPrepArgument }
case class Adv(wh: LowerCaseString) extends ArgumentSlot { type Arg = Unit }

object ArgumentSlot {
  type Aux[A] = ArgumentSlot { type Arg = A }

  def allAdvSlots =
    List("when", "where", "why", "how", "how long", "how much").map(s => Adv(s.lowerCase))

  def toString(slot: ArgumentSlot): String = slot match {
    case Subj    => "subj"
    case Obj     => "obj"
    case Prep1   => "prep1"
    case Prep2   => "prep2"
    case Misc    => "misc"
    case Adv(wh) => wh.toString
  }

  def fromString(str: String): Option[ArgumentSlot] = str match {
    case "subj"  => Some(Subj)
    case "obj"   => Some(Obj)
    case "prep1" => Some(Prep1)
    case "prep2" => Some(Prep2)
    case "misc"  => Some(Misc)
    case wh if allAdvSlots.contains(wh.lowerCase) => Some(Adv(wh.lowerCase))
    case _ => None
  }

  import io.circe.{KeyEncoder, KeyDecoder}
  import io.circe.{Encoder, Decoder}

  implicit val argumentSlotKeyEncoder = KeyEncoder.instance(ArgumentSlot.toString)
  implicit val argumentSlotKeyDecoder = KeyDecoder.instance(ArgumentSlot.fromString)

  import qfirst.frames.implicits.{DependentEncoder, DependentDecoder}
  import cats.Id

  implicit val dependentArgumentEncoder = new DependentEncoder[ArgumentSlot.Aux, Id] {
    final def getEncoder[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Encoder[Noun]]
      case Obj    => implicitly[Encoder[Noun]]
      case Prep1  => implicitly[Encoder[Preposition]]
      case Prep2  => implicitly[Encoder[Preposition]]
      case Misc   => implicitly[Encoder[NonPrepArgument]]
      case Adv(_) => implicitly[Encoder[Unit]].asInstanceOf[Encoder[A]] // TODO this should be acceptable w/o cast?
    }
  }

  implicit val dependentArgumentDecoder = new DependentDecoder[ArgumentSlot.Aux, Id] {
    final def getDecoder[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Decoder[Noun]]
      case Obj    => implicitly[Decoder[Noun]]
      case Prep1  => implicitly[Decoder[Preposition]]
      case Prep2  => implicitly[Decoder[Preposition]]
      case Misc   => implicitly[Decoder[NonPrepArgument]]
      case Adv(_) => implicitly[Decoder[Unit]].asInstanceOf[Decoder[A]] // TODO this should be acceptable w/o cast?
    }
  }
}