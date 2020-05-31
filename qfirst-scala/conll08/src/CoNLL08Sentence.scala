package qfirst.conll08

import cats.Order
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

sealed trait CoNLL08Split {
  import CoNLL08Split.{Trial, Train, Dev, TestWSJ, TestBrown}
  override def toString = this match {
    case Train => "train"
    case Dev => "devel"
    case TestWSJ => "test.wsj"
    case TestBrown => "test.brown"
    case Trial => "trial"
  }

  def isTest = this match {
    case TestWSJ => true
    case TestBrown => true
    case _ => false
  }
}
object CoNLL08Split {
  case object Train extends CoNLL08Split
  case object Dev extends CoNLL08Split
  case object TestWSJ extends CoNLL08Split
  case object TestBrown extends CoNLL08Split
  case object Trial extends CoNLL08Split

  val all = Set[CoNLL08Split](Train, Dev, TestWSJ, TestBrown, Trial)

  def unapply(splitStr: String): Option[CoNLL08Split] =
    fromString(splitStr)

  def fromString(x: String): Option[CoNLL08Split] = x match {
    case "train" => Some(Train)
    case "devel" => Some(Dev)
    case "test.wsj" => Some(TestWSJ)
    case "test.brown" => Some(TestBrown)
    case "trial" => Some(Trial)
    case _ => None
  }

  val conll08SplitOrder: Order[CoNLL08Split] = Order.by[CoNLL08Split, Int] {
    case Train => 0
    case Dev => 1
    case TestWSJ => 2
    case TestBrown => 3
    case Trial => 4
  }
}

case class CoNLL08SentenceId(
  split: CoNLL08Split, // train, dev, test
  index: Int // sentence index
) {
  override def toString = s"$split:$index"
}
object CoNLL08SentenceId {
  private def isInt(s: String) = scala.util.Try(s.toInt).toOption.nonEmpty
  def fromString(x: String): Option[CoNLL08SentenceId] = x.split(":").toList match {
    case CoNLL08Split(split) :: index :: Nil if isInt(index) =>
      Some(CoNLL08SentenceId(split, index.toInt))
    case _ => None
  }

  // implicit val conll08SentenceIdEncoder: Encoder[CoNLL08SentenceId] =
  //   Encoder.encodeString.contramap[CoNLL08SentenceId](_.toString)
  // implicit val conll08SentenceIdDecoder: Decoder[CoNLL08SentenceId] =
  //   Decoder.decodeString.emap(
  //     x => fromString(x).fold[Either[String, CoNLL08SentenceId]](
  //       Left(s"$x is not a valid CoNLL 2008 Sentence ID"))(
  //       Right(_))
  //   )
}

case class CoNLL08Sentence(
  id: CoNLL08SentenceId,
  tokens: Vector[CoNLL08Token],
  // syntacticHeads: Vector[Int], // don't need this right now
  predicateArgumentStructures: List[PredicateArgumentStructure])


case class PredicateArgumentStructure(
  predicate: Predicate,
  arguments: List[(String, Int)])

case class Predicate(
  index: Int,
  lemma: String,
  sense: String)
