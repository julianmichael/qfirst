package qfirst.metrics

import cats.Monoid
import cats.implicits._

case class Proportion[A](
  included: Vector[A] = Vector(),
  excluded: Vector[A] = Vector()) {
  def stats = Proportion.Stats(included.size, excluded.size)
}
object Proportion {
  def included[A](as: A*) = Proportion(included = as.toVector)
  def excluded[A](as: A*) = Proportion(excluded = as.toVector)
  case class Stats(
    included: Int = 0,
    excluded: Int = 0
  ) {
    def total = included + excluded
    def proportion = included.toDouble / total

    def getTree: MapTree[String, Metric] = MapTree.fromPairs(
      "total" -> Metric.int(total),
      "proportion" -> Metric.double(proportion)
    )
  }
  object Stats {
    implicit val proportionStatsMonoid: Monoid[Proportion.Stats] = {
      import cats.derived.auto.monoid._
      cats.derived.semi.monoid
    }
    implicit val proportionStatsHasMetrics = new HasMetrics[Proportion.Stats] {
      def getMetrics(acc: Proportion.Stats) = acc.getTree
    }
  }
  implicit def proportionMonoid[A]: Monoid[Proportion[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit def proportionHasMetrics[A] = new HasMetrics[Proportion[A]] {
    def getMetrics(acc: Proportion[A]) = acc.stats.getTree
  }
}
