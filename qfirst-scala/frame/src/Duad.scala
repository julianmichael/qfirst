package qfirst.frame

import cats.Order
import cats.implicits._

sealed trait Duad[A] extends Product with Serializable {
  def min: A
  def max: A
}
object Duad {
  private case class DuadImpl[A](val min: A, val max: A) extends Duad[A]
  def apply[A](_1: A, _2: A)(implicit o: Order[A]): Duad[A] = {
    if(_1 <= _2) DuadImpl(_1, _2) else DuadImpl(_2, _1)
  }
  def unapply[A](d: Duad[A]): Some[(A, A)] = Some(d.min -> d.max)
}
