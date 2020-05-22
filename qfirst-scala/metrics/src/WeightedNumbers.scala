package qfirst.metrics

import cats.Monoid
import cats.implicits._


// TODO weighted quartiles, etc.

case class WeightedNumbers[N](values: Vector[(Double, N)])(implicit N: Numeric[N]) {
  def stats = {
    val pcount = values.foldMap(_._1)
    val wsum = values.foldMap { case (w, n) => w * N.toDouble(n) }
    WeightedNumbers.Stats(
      pcount, wsum
    )
  }
}
object WeightedNumbers {
  def apply[N: Numeric](n: N, weight: Double = 1.0): WeightedNumbers[N] = WeightedNumbers(Vector(weight -> n))
  case class Stats(
    pseudocount: Double,
    weightedSum: Double,
  ) {
    def weightedMean: Double = weightedSum / pseudocount
    def getMetrics: MapTree[String, Metric] = MapTree.fork(
      "pseudocount" ->     MapTree.leaf[String](Metric.double(pseudocount)),
      "weighted sum" ->    MapTree.leaf[String](Metric.double(weightedSum)),
      "weighted mean" ->   MapTree.leaf[String](Metric.double(weightedMean))
    )
  }

  implicit def weightedNumbersMonoid[A: Numeric]: Monoid[WeightedNumbers[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit def weightedNumbersHasMetrics[A] = new HasMetrics[WeightedNumbers[A]] {
    def getMetrics(nums: WeightedNumbers[A]) = nums.stats.getMetrics
  }
}
