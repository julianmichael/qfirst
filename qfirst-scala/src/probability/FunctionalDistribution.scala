package qfirst.probability

class FunctionalDistribution[A](
  val probability: A => Double
) {
  def interpolate(thisLambda: Double, that: FunctionalDistribution[A]) = {
    new FunctionalDistribution(
      (a: A) => (thisLambda * probability(a)) + ((1.0 - thisLambda) * that.probability(a))
    )
  }
}
object FunctionalDistribution {
  def interpolate[A](dists: (Double, FunctionalDistribution[A])*) = {
    val total = dists.unzip._1.sum
    new FunctionalDistribution((a: A) =>
      dists.map { case (distCoeff, dist) =>
        dist.probability(a) * distCoeff / total
      }.sum
    )
  }
}
