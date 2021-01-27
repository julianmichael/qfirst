package qfirst.frame.eval

import qfirst.frame._
import qfirst.frame.util.Duad

import cats.Order
import cats.implicits._

object EvalUtils {

  val conll08RoleIndices = List(
    "A0", "A1", "A2", "A3", "A4",
    "AM-LOC", "AM-MNR",
    "AM-ADV",
    "AM-PNC",
    "AM-CAU",
    "AM-TMP",
    "AM-DIS",
    "AM-DIR", "AM-EXT",
    "AM-MOD", "AM-NEG",
    "AM-PRD",
    "A5", "AA",
  ).zipWithIndex.toMap
  val conll08RoleOrder = Order.whenEqual(
    Order.by[String, Int](x => conll08RoleIndices.getOrElse(x, 100)),
    Order[String]
  )

  def calculateNPMIs[A: Order, N: Numeric](
    groupings: Vector[Map[A, N]]
  ): Map[Duad[A], Double] = {
    val groups = groupings.map(_.mapVals(implicitly[Numeric[N]].toDouble))
    import scala.math.{pow, log}
    val labels = groups.foldMap(_.keySet)
    val groupSizes = groups.map(_.unorderedFold)
    val total = groupSizes.combineAll
    val totalPairs = groupSizes.foldMap(pow(_, 2))
    val marginals = groups.foldMap { counts =>
      val size = counts.unorderedFold
      counts.mapVals(_ * size)
    }
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    val marginalProbs = groups.combineAll.mapVals(_ / total)

    pairs.iterator.map { pair =>
      // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
      val prob = groups.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0.0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0.0)
        (leftCounts * rightCounts).toDouble / goldCounts.unorderedFold
      } / total
      val independentProb = {
        marginalProbs(pair.min) * marginalProbs(pair.max)// / (totalPairs * totalPairs)
      }
      val npmi = if(prob == 0.0) {
        if(independentProb == 0.0) {
          assert(false) // should never happen
          0.0
        } else -1.0
      } else {
        val logJointProb = log(prob)
        val pmi = logJointProb - log(independentProb)
        pmi / (-logJointProb)
      }
      pair -> npmi
    }.toMap
  }

  // here the "independent" case gets to assume the two are drawn from the same verb,
  // basically forming a stronger baseline.
  def calculateAggregateNPMIs[A: Order](
    clusterings: Vector[Vector[Map[A, Int]]]
  ): Map[Duad[A], Double] = {
    val allClusterings = clusterings.flatten
    val localCounts = clusterings.map(_.unorderedFold)
    val total = allClusterings.foldMap(_.unorderedFold)
    val labels = allClusterings.foldMap(_.keySet)
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    pairs.iterator.map { pair =>
      // generative process: pick one point at random (left counts / total),
      // then pick another in its same cluster (right counts / cluster size)
      val jointProb = allClusterings.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0)
        val subtotal = goldCounts.unorderedFold
        if(subtotal == 0) 0.0 else {
          (leftCounts * rightCounts).toDouble / subtotal
        }
      } / total

      // generative process: pick one point at random (left counts / total),
      // then look at the probability of picking the same label randomly among that verb (marginal / verb total).
      // incidentally, it looks exactly like the above, after collapsing all sub-verb clusterings. huh
      val independentProb = localCounts.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0)
        val subtotal = goldCounts.unorderedFold
        if(subtotal == 0) 0.0 else {
          (leftCounts * rightCounts).toDouble / subtotal
        }
      } / total

      import scala.math.{pow, log}
      val npmi = if(jointProb == 0.0) -1.0 else {
        val logJointProb = log(jointProb)
        val pmi = logJointProb - log(independentProb)
        pmi / -logJointProb
      }
      pair -> npmi
    }.toMap
  }
}
