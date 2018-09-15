package qfirst

case class BoundedAcc(
  numValidReferences: Int,
  numInvalidReferences: Int,
  numCorrect: Int,
  numIncorrect: Int,
  numUncertain: Int
) {
  def numPredicted = numCorrect + numIncorrect + numUncertain
  def accuracyLowerBound = numCorrect.toDouble / numPredicted
  def accuracyUpperBound = (numCorrect + numUncertain).toDouble / numPredicted

  def allStats: MapTree[String, MetricValue] = MapTree.fromPairs(
    "num valid refs" -> MetricValue(numValidReferences),
    "num invalid refs" -> MetricValue(numInvalidReferences),
    "num predicted" -> MetricValue(numPredicted),
    "acc-lb" -> MetricValue(accuracyLowerBound),
    "acc-ub" -> MetricValue(accuracyUpperBound)
  )
}
object BoundedAcc {
  // TODO monoid
}
