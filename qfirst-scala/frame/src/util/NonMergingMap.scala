package qfirst.frame.util

import cats.Monoid

class NonMergingMap[A, B](val value: Map[A, B]) extends (A => B) {
  def apply(a: A): B = value.apply(a)
  override def toString: String = value.toString
}
object NonMergingMap {
  def apply[A, B](value: Map[A, B]): NonMergingMap[A, B] = new NonMergingMap(value)
  def apply[A, B](values: (A, B)*): NonMergingMap[A, B] = new NonMergingMap(Map(values: _*))

  implicit def nonMergingMapMonoid[A, B]: Monoid[NonMergingMap[A, B]] =
  new Monoid[NonMergingMap[A, B]] {
    def empty: NonMergingMap[A, B] = NonMergingMap(Map[A, B]())
    def combine(x: NonMergingMap[A, B], y: NonMergingMap[A, B]) = {
      NonMergingMap(x.value ++ y.value)
    }
  }
}
