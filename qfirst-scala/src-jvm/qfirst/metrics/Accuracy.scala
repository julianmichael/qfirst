package qfirst.metrics
import qfirst._

import cats.Monoid
import cats.implicits._

case class Accuracy(
  correct: Int = 0,
  incorrect: Int = 0
) {
  def predicted = correct + incorrect
  def accuracy = correct.toDouble / predicted

  def allStats: MapTree[String, Metric] = MapTree.fromPairs(
    "num predicted" -> Metric.int(predicted),
    "accuracy" -> Metric.double(accuracy),
  )
}
object Accuracy {
  implicit val AccuracyMonoid: Monoid[Accuracy] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit val AccuracyHasMetrics = new HasMetrics[Accuracy] {
    def getMetrics(acc: Accuracy) = acc.allStats
  }
}
