package qfirst.frame.eval

import qfirst.frame.util.Duad

import cats.Order
import cats.implicits._

object EvalUtils {

  // With or without replacement? currently implemented with replacement
  // i think this is right because pmi with self should never be negative.
  // w/o replacement would mean neg self-pmi possible with fine-grained clusters.
  def calculateNPMIs[A: Order](
    predictedClusters: Vector[Map[A, Int]]
  ): Map[Duad[A], Double] = {
    import scala.math.{pow, log}
    val labels = predictedClusters.foldMap(_.keySet)
    val total = predictedClusters.foldMap(_.unorderedFold)
    val totalPairs = predictedClusters.foldMap(counts => pow(counts.unorderedFold, 2))
    val marginals = predictedClusters.combineAll
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    pairs.iterator.map { pair =>
      val cooccurrences = predictedClusters.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0)
        leftCounts * rightCounts
      }
      val cooccurrencesUnderIndependence = marginals(pair.min) * marginals(pair.max)
      val pnmi = if(cooccurrences == 0) {
        if(cooccurrencesUnderIndependence == 0) {
          assert(false) // should never happen
          0.0
        } else -1.0
      } else {
        val logJointProb = log(cooccurrences.toDouble / totalPairs)
        val probUnderIndependence = cooccurrencesUnderIndependence.toDouble / (total * total)
        val pmi = logJointProb - log(probUnderIndependence)
        pmi / (-logJointProb)
      }
      pair -> pnmi
    }.toMap
  }
}
