package qfirst.metrics
import qfirst._

import cats.Monoid
import cats.implicits._

case class BoundedAcc(
  correct: Int = 0,
  incorrect: Int = 0,
  uncertain: Int = 0
) {
  def predicted = correct + incorrect + uncertain
  def accuracyLowerBound = correct.toDouble / predicted
  def accuracyUpperBound = (correct + uncertain).toDouble / predicted

  def allStats: MapTree[String, Metric] = MapTree.fromPairs(
    "num predicted" -> Metric.int(predicted),
    "acc-lb" -> Metric.double(accuracyLowerBound),
    "acc-ub" -> Metric.double(accuracyUpperBound)
  )
}
object BoundedAcc {
  implicit val boundedAccMonoid: Monoid[BoundedAcc] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit val boundedAccHasMetrics = new HasMetrics[BoundedAcc] {
    def getMetrics(bacc: BoundedAcc) = bacc.allStats
  }
}
