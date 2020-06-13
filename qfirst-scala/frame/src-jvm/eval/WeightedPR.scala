package qfirst.frame.eval

import cats.Monoid
import qfirst.metrics.Functions
import qfirst.metrics.HasMetrics
import qfirst.metrics.MapTree
import qfirst.metrics.Metric
import qfirst.metrics.WeightedNumbers

case class WeightedPR(
  precisions: WeightedNumbers[Double],
  recalls: WeightedNumbers[Double]
) {
  def pseudocount = precisions.stats.pseudocount
  def precision = precisions.stats.weightedMean
  def recall = recalls.stats.weightedMean
  def f1 = Functions.harmonicMean(precision, recall)
  def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
  def normalize = WeightedPR(precisions.normalize, recalls.normalize)
}
object WeightedPR {
  implicit val weightedPRMonoid: Monoid[WeightedPR] = {
    import cats.derived.auto.monoid._
    cats.derived.semi.monoid
  }
  implicit val weightedPRHasMetrics = new HasMetrics[WeightedPR] {
    def getMetrics(pr: WeightedPR) = MapTree.fromPairs(
      // "pseudocount" -> Metric.double(pr.pseudocount),
      "precision" -> Metric.double(pr.precision),
      "recall" -> Metric.double(pr.recall),
      "f1" -> Metric.double(pr.f1)
    )
  }
}
