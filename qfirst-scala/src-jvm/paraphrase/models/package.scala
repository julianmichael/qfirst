package qfirst.paraphrase

import cats.Foldable
import cats.implicits._

import scala.util.Random

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.distributions.Multinomial
import scala.collection.immutable.Vector

package object models {

  type DenseMultinomial = Multinomial[DenseVector[Double], Int]

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

  import breeze.linalg._
  import breeze.stats.distributions._
  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromSparseNew[A](pseudoCounts: Map[Int, A], supportSize: Int, alpha: Double)(implicit N: Numeric[A]) = {
    val priorCount = alpha / supportSize
    Multinomial(
      DenseVector.tabulate[Double](supportSize)(i =>
        N.toDouble(pseudoCounts.getOrElse(i, N.zero)) + priorCount
      )
    )
  }
}
