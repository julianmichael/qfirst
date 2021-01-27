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

  // TODO: enhance into more general cooccurrence utility?
  // first step: take weighted groups to make the pmi/npmi calculation more straightforward.
  // second step: maybe put together into a general cooccurrence utility.

  // sourceDistribution is a probability distribution over sources where the pair appears.
  // TODO: change this to compute sample covariance correctly (with n-1 denom)
  case class NPMIResult[A](
    pmi: Double,
    npmi: Double,
    covariance: Double,
    correlation: Double,
    sourceDistribution: Map[A, Double]
  )
  object NPMIResult {
    private[EvalUtils] def never[A](covariance: Double, correlation: Double) = NPMIResult[A](
      pmi = Double.NegativeInfinity,
      npmi = -1.0,
      covariance = covariance,
      correlation = correlation,
      sourceDistribution = Map()
    )
  }

  def calculateNPMIsLoggingEfficient[Source, A: Order, N: Numeric](
    groupings: Vector[(Source, Map[A, N])])(
    implicit N: Numeric[N],
    Log: freelog.SequentialEphemeralTreeLogger[cats.effect.IO, String]
  ): cats.effect.IO[Map[Duad[A], NPMIResult[Source]]] = {
    import freelog.implicits._
    val groups = groupings.map(_._2.mapVals(N.toDouble))
    import scala.math.{pow, log, sqrt}
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

    // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
    groupings.infoBarFoldMapM("Computing joint probabilities") { case (source, _goldCounts) =>
      val goldCounts = _goldCounts.mapVals(N.toDouble)
      cats.effect.IO {
        val groupTotal = goldCounts.unorderedFold
        goldCounts.toList.foldMap { case (left, leftCount) =>
          val leftProb = leftCount / total
          goldCounts.filter(_._1 >= left).map { case (right, rightCount) =>
            val rightProb = rightCount / groupTotal
            Duad(left, right) -> Map(source -> (leftProb * rightProb))
          }
        }
      }
    }.flatMap { jointProbsWithSources =>
      val stdevs = marginalProbs.map { case (a, marginal) =>
        val selfJointProb = jointProbsWithSources(Duad(a, a)).unorderedFold
        a -> sqrt(selfJointProb - pow(marginal, 2))
      }
      Log.info(s"${pairs.size} pairs to compute.") >>
      pairs.toList.infoBarTraverse("Computing NPMIs") { pair =>
        cats.effect.IO {
          // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
          val sources = jointProbsWithSources.getOrElse(pair, Map())
          val jointProb = sources.unorderedFold
          val independentProb = {
            marginalProbs(pair.min) * marginalProbs(pair.max)
          }
          val covariance = jointProb - independentProb
          val correlation = covariance / (stdevs(pair.min) * stdevs(pair.max))
          val result = if(jointProb == 0.0) NPMIResult.never[Source](covariance, correlation) else {
            assert(independentProb != 0.0)
            val logJointProb = log(jointProb)
            val pmi = logJointProb - log(independentProb)
            val npmi = pmi / (-logJointProb)
            NPMIResult(
              pmi = pmi,
              npmi = npmi,
              covariance = covariance,
              correlation = correlation,
              sourceDistribution = sources)
          }
          pair -> result
        }
      }.map(_.toMap)
    }
  }

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
