package qfirst.parsing

import cats.Order

case class Scored[A](item: A, score: Double) {
  def addScore(addition: Double) = Scored(item, score + addition)
  def map[B](f: A => B): Scored[B] = Scored(f(item), score)
  def flatMap[B](f: A => Scored[B]) = f(item).addScore(score)
  def flatten[B](implicit ev: A =:= Scored[B]): Scored[B] = item.addScore(score)
  override def toString = f"$item%s: $score%.2f"

  def commuteOption[B](implicit ev: A <:< Option[B]): Option[Scored[B]] = ev(item).map(Scored(_, score))
}
object Scored {
  implicit def scoredOrdering[A]: Ordering[Scored[A]] = Ordering.by[Scored[A], Double](_.score)
  implicit def scoredOrder[A]: Order[Scored[A]] = Order.fromOrdering(scoredOrdering[A])
  def unit[A](a: A): Scored[A] = Scored(a, 0.0)
}
