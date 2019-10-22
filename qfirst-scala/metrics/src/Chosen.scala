package qfirst.metrics

import cats.Applicative
import cats.Eval
import cats.Monoid
import cats.Show
import cats.Traverse
import cats.implicits._

import monocle.function.Each

import HasMetrics.ops._

case class Chosen[Param, MetricData](
  data: Map[Param, MetricData]
) {
  // TODO TraverseFilter
  def filter(p: MetricData => Boolean) = Chosen(data.collect { case (k, v) if p(v) => k -> v })
  def keepMaxBy[O](f: MetricData => O)(implicit o: Ordering[O]) = {
    if(data.nonEmpty) Chosen(Map(data.toList.maxBy(p => f(p._2))))
    else Chosen[Param, MetricData](Map())
  }
}
object Chosen {
  implicit def chosenTraverse[Param]: Traverse[Chosen[Param, ?]] = new Traverse[Chosen[Param, ?]] {
    def traverse[G[_]: Applicative, A, B](fa: Chosen[Param, A])(f: A => G[B]): G[Chosen[Param, B]] = {
      fa.data.toList.traverse { case (param, value) => f(value).map(param -> _) }.map(l => Chosen(l.toMap))
    }
    def foldLeft[A, B](fa: Chosen[Param, A], b: B)(f: (B, A) => B): B = fa.data.values.foldLeft(b)(f)
    def foldRight[A, B](fa: Chosen[Param, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.data.values.foldRight(lb)(f)
  }
  implicit def chosenMonoid[Param, A: Monoid]: Monoid[Chosen[Param, A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit def chosenHasMetrics[Param: Show, A: HasMetrics]: HasMetrics[Chosen[Param, A]] = new HasMetrics[Chosen[Param, A]] {
    def getMetrics(ba: Chosen[Param, A]): MapTree[String, Metric] = {
      MapTree.fork(
        ba.data.map { case (param, bucketData) =>
          param.show -> bucketData.getMetrics
        }
      )
    }
  }
  implicit def chosenEach[Param, A] = Each.fromTraverse[Chosen[Param, ?], A]
}
