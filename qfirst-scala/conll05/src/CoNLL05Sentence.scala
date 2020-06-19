package qfirst.conll05

import cats.Order
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import jjm.ling.ESpan

case class CoNLL05SentenceId(
  split: CoNLL05Split,
  index: Int // sentence index
) {
  override def toString = s"$split:$index"
}
object CoNLL05SentenceId {
  private def isInt(s: String) = scala.util.Try(s.toInt).toOption.nonEmpty
  def fromString(x: String): Option[CoNLL05SentenceId] = x.split(":").toList match {
    case CoNLL05Split(split) :: index :: Nil if isInt(index) =>
      Some(CoNLL05SentenceId(split, index.toInt))
    case _ => None
  }

  // implicit val conll05SentenceIdEncoder: Encoder[CoNLL05SentenceId] =
  //   Encoder.encodeString.contramap[CoNLL05SentenceId](_.toString)
  // implicit val conll05SentenceIdDecoder: Decoder[CoNLL05SentenceId] =
  //   Decoder.decodeString.emap(
  //     x => fromString(x).fold[Either[String, CoNLL05SentenceId]](
  //       Left(s"$x is not a valid CoNLL 2005 Sentence ID"))(
  //       Right(_))
  //   )
}

case class CoNLL05Sentence(
  id: CoNLL05SentenceId,
  predicateArgumentStructures: List[PredicateArgumentStructure])


case class PredicateArgumentStructure(
  predicate: Predicate,
  arguments: List[(String, ESpan)])

case class Predicate(
  index: Int,
  lemma: String,
  // sense: String // TODO if necessary
)
