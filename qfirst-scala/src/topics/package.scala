package qfirst

import cats.Foldable
import cats.implicits._

import scala.util.Random

package object topics {
  def mean[F[_]: Foldable](fa: F[Double]) = {
    fa.combineAll / fa.size
  }

  // assume normalized
  def sample[F[_]: Foldable](dist: F[Double], rand: Random): Int = {
    dist.toList.zipWithIndex.foldM[Either[Int, ?], Double](rand.nextDouble) {
      case (mass, (prob, index)) =>
        if(mass <= prob) Left(index)
        else Right(mass - prob)
    }.left.get // will always be present for prob dist
  }

  def logSumExp[F[_]: Foldable](args: F[Double]): Double = {
    val max = args.maximumOption.getOrElse(0.0)
    val sumExp = args.foldMap(x => math.exp(x - max))
    max + math.log(sumExp)
  }

  def makeClusterFromCounts[A](counts: Map[Int, A], numItems: Int, clusterSmoothingCounts: Double)(implicit N: Numeric[A]) = {
    val countSum = N.toDouble(counts.values.sum)
    val denom = countSum + clusterSmoothingCounts
    val smoothNum = clusterSmoothingCounts / numItems
    counts.foldLeft(Vector.fill(numItems)(smoothNum / denom)) {
      case (vec, (idx, count)) =>
        vec.updated(idx, (smoothNum + N.toDouble(count)) / denom)
    }
  }
}
