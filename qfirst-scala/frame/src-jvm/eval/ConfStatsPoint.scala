package qfirst.frame.eval

import qfirst.metrics.Functions

case class ConfStatsPoint(loss: Double, clusterSizes: Vector[Int], weightedPR: WeightedPR) {
  def precision = weightedPR.precision
  def recall = weightedPR.recall
  def numClusters = clusterSizes.size
  def numItems = clusterSizes.sum
  def lossPerItem = loss / numItems
  def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
  def f1 = Functions.harmonicMean(precision, recall)
}
