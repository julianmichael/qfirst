package qfirst

import cats.kernel.CommutativeMonoid
import cats.implicits._

import io.circe.generic.JsonCodec

// TODO restructure / write custom json codec to prevent invalid construction
@JsonCodec sealed trait FiniteRelation[A, B] {
  def domain: Set[A]
  def range: Set[B]
  def image(a: A): Set[B]
  def preimage(b: B): Set[A]
  def +(a: A, b: B): FiniteRelation[A, B]
  def +(pair: (A, B)): FiniteRelation[A, B]
  def -(a: A, b: B): FiniteRelation[A, B]
  def -(pair: (A, B)): FiniteRelation[A, B]

  def image(as: Set[A]): Set[B] = as.unorderedFoldMap(image)
  def preimage(bs: Set[B]): Set[A] = bs.unorderedFoldMap(preimage)
  def contains(a: A, b: B) = image(a).contains(b)
}
object FiniteRelation {
  private[FiniteRelation] case class FiniteRelationImpl[A, B](
    aToB: Map[A, Set[B]],
    bToA: Map[B, Set[A]]
  ) extends FiniteRelation[A, B] {
    def domain: Set[A] = aToB.keySet
    def range: Set[B] = bToA.keySet
    def image(a: A): Set[B] = aToB.getOrElse(a, Set())
    def preimage(b: B): Set[A] = bToA.getOrElse(b, Set())
    def +(a: A, b: B): FiniteRelation[A, B] = FiniteRelationImpl(
      aToB + (a -> (image(a) + b)),
      bToA + (b -> (preimage(b) + a))
    )
    def +(pair: (A, B)): FiniteRelation[A, B] = FiniteRelationImpl(
      aToB + (pair._1 -> (image(pair._1) + pair._2)),
      bToA + (pair._2 -> (preimage(pair._2) + pair._1))
    )
    def -(a: A, b: B): FiniteRelation[A, B] = FiniteRelationImpl(
      aToB + (a -> (image(a) - b)),
      bToA + (b -> (preimage(b) - a))
    )
    def -(pair: (A, B)): FiniteRelation[A, B] = FiniteRelationImpl(
      aToB + (pair._1 -> (image(pair._1) - pair._2)),
      bToA + (pair._2 -> (preimage(pair._2) - pair._1))
    )
  }
  private[FiniteRelation] object FiniteRelationImpl {
    def fromLists[A, B](
      aToBList: List[(A, Set[B])],
      bToAList: List[(B, Set[A])]
    ) = FiniteRelationImpl(aToBList.toMap, bToAList.toMap)
    import io.circe.{Encoder, Decoder}
    implicit def finiteRelationImplDecoder[A: Decoder, B: Decoder]: Decoder[FiniteRelationImpl[A, B]] = Decoder.forProduct2("aToB", "bToA")(fromLists)
    implicit def finiteRelationImplEncoder[A: Encoder, B: Encoder]: Encoder[FiniteRelationImpl[A, B]] = Encoder.forProduct2("aToB", "bToA")(d =>
      (d.aToB.toList, d.bToA.toList)
    )
  }
  implicit def relationCommutativeMonoid[A, B]: CommutativeMonoid[FiniteRelation[A, B]] = new CommutativeMonoid[FiniteRelation[A, B]] {
    def empty: FiniteRelation[A, B] = FiniteRelationImpl[A, B](Map(), Map())
    def combine(x: FiniteRelation[A, B], y: FiniteRelation[A, B]) = (x, y) match {
      case (FiniteRelationImpl(xFwd, xBwd), FiniteRelationImpl(yFwd, yBwd)) =>
        FiniteRelationImpl(xFwd |+| yFwd, xBwd |+| yBwd)
    }
  }
  def empty[A, B]: FiniteRelation[A, B] = FiniteRelationImpl[A, B](Map(), Map())
  def single[A, B](a: A, b: B): FiniteRelation[A, B] = FiniteRelationImpl[A, B](Map(a -> Set(b)), Map(b -> Set(a)))
}
