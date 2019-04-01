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

  def uniform(supportSize: Int) = {
    Vector.fill(supportSize)(1.0 / supportSize)
  }

  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromDense[A](pseudoCounts: Vector[Double], alpha: Double) = {
    val priorCount = alpha / pseudoCounts.size
    val normalization = pseudoCounts.sum + alpha
    pseudoCounts.map(pc => (pc + priorCount) / normalization)
  }

  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromSparse[A](pseudoCounts: Map[Int, A], supportSize: Int, alpha: Double)(implicit N: Numeric[A]) = {
    val normalization = N.toDouble(pseudoCounts.values.sum) + alpha
    val priorCount = alpha / supportSize
    pseudoCounts.foldLeft(Vector.fill(supportSize)(priorCount / normalization)) {
      case (vec, (idx, count)) =>
        vec.updated(idx, (priorCount + N.toDouble(count)) / normalization)
    }
  }
}
