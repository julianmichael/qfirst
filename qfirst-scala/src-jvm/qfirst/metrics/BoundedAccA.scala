package qfirst.metrics
import qfirst._

import cats.Monoid
import cats.MonoidK
import cats.implicits._

case class BoundedAccA[A](
  correct:   Vector[A] = Vector(),
  incorrect: Vector[A] = Vector(),
  uncertain: Vector[A] = Vector()
) {
  def boundedAcc = BoundedAcc(
    correct.size,
    incorrect.size,
    uncertain.size
  )
}
object BoundedAccA {
  implicit val boundedAccAMonoidK: MonoidK[BoundedAccA] = {
    import cats.derived.auto.monoidK._
    cats.derived.semi.monoidK
  }
  implicit def boundedAccAMonoid[A]: Monoid[BoundedAccA[A]] = boundedAccAMonoidK.algebra[A]
  implicit def boundedAccAHasMetrics[A] = new HasMetrics[BoundedAccA[A]] {
    def getMetrics(bacc: BoundedAccA[A]) = bacc.boundedAcc.allStats
  }

  def correct[A](a: A) = BoundedAccA[A](correct = Vector(a))
  def incorrect[A](a: A) = BoundedAccA[A](incorrect = Vector(a))
  def uncertain[A](a: A) = BoundedAccA[A](uncertain = Vector(a))
}
