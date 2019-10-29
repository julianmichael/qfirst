package qfirst.frame.math

import cats.Foldable
import cats.kernel.CommutativeMonoid
import cats.implicits._

import io.circe.generic.JsonCodec

// TODO restructure / write custom json codec to prevent invalid construction
@JsonCodec case class FiniteSymmetricRelation[A](
  innerRel: FiniteRelation[A, A] // constrained to be symmetric
) {
  def domain: Set[A] = innerRel.domain
  def image(a: A): Set[A] = innerRel.image(a)
  def range: Set[A] = domain
  def preimage(b: A): Set[A] = image(b)
  def +(a: A, b: A): FiniteSymmetricRelation[A] =
    new FiniteSymmetricRelation(innerRel + (a, b) + (b, a))
  def +(pair: (A, A)): FiniteSymmetricRelation[A] =
    new FiniteSymmetricRelation(innerRel + pair + pair.swap)
  def -(a: A, b: A): FiniteSymmetricRelation[A] =
    new FiniteSymmetricRelation(innerRel - (a, b) - (b, a))
  def -(pair: (A, A)): FiniteSymmetricRelation[A] =
    new FiniteSymmetricRelation(innerRel - pair - pair.swap)

  def image(as: Set[A]): Set[A] = as.unorderedFoldMap(image)
  def preimage(bs: Set[A]): Set[A] = bs.unorderedFoldMap(preimage)
  def contains(a: A, b: A) = image(a).contains(b)
  def ++[F[_]: Foldable](pairs: F[(A, A)]): FiniteSymmetricRelation[A] =
    pairs.foldLeft(this)(_ + _)
  def --[F[_]: Foldable](pairs: F[(A, A)]): FiniteSymmetricRelation[A] =
    pairs.foldLeft(this)(_ - _)
}
object FiniteSymmetricRelation {
  def empty[A] = new FiniteSymmetricRelation(FiniteRelation.empty[A, A])
  def single[A](x: A, y: A) = empty[A] + (x, y)
  implicit def finiteSymmetricRelationCommutativeMonoid[A]: CommutativeMonoid[FiniteSymmetricRelation[A]] = {
    new CommutativeMonoid[FiniteSymmetricRelation[A]] {
      def empty: FiniteSymmetricRelation[A] = FiniteSymmetricRelation.empty[A]
      def combine(x: FiniteSymmetricRelation[A], y: FiniteSymmetricRelation[A]): FiniteSymmetricRelation[A] =
        new FiniteSymmetricRelation(x.innerRel |+| y.innerRel)
    }
  }
}
