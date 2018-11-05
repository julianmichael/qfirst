package qfirst.probability

import cats.implicits._

// TODO: decide on what to do about Equals for this
class CategoricalDistribution[A] private (
  // assumptions:
  // - values are nonzero probabilities, i.e., 0 < x ≤ 1 and sum to 1
  private val probabilities: Map[A, Double]
) {
  def support: Set[A] = probabilities.keySet
  def probability(a: A) = probabilities.getOrElse(a, 0.0)
  // assume 0 <= thisLambda <= 1
  def interpolate(thisLambda: Double, that: CategoricalDistribution[A]) = {
    CategoricalDistribution(
      probabilities.map { case (k, v) => k -> (thisLambda * v) } |+|
        that.probabilities.map { case (k, v) => k -> ((1.0 - thisLambda) * v) }
    )
  }
}
object CategoricalDistribution {
  def apply[A, N](counties: Map[A, N])(implicit N: Numeric[N]) = {
    val sum = counties.toList
      .map(p => N.toDouble(p._2))
      .filter(_ >= 0.0)
      .sum
    val probs = counties.mapValues(N.toDouble).collect {
      case (k, v) if v > 0.0 => k -> (v / sum)
    }
    new CategoricalDistribution[A](probs)
  }

  def uniform[A](support: Set[A]) = {
    new CategoricalDistribution(support.toList.map(a => a -> (1.0 / support.size)).toMap)
  }
}
