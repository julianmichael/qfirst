package qfirst.frame

import java.nio.file.{Path => NIOPath}

import cats.Order
import cats.implicits._

import cats.effect.IO

import io.circe.Decoder
import io.circe.Encoder

import freelog.SequentialEphemeralTreeLogger
import freelog.implicits._

import jjm.LowerCaseString
import jjm.io.FileUtil
import jjm.implicits._

import qfirst.frame.features.PropBankFeatures
import qfirst.frame.util.Duad
import qfirst.frame.eval.EvalUtils
import freelog.EphemeralTreeLogger
import qfirst.metrics.WeightedNumbers

case class Analysis[Arg: Encoder : Decoder : Order](
  features: PropBankFeatures[Arg],
  outDir: NIOPath
) {

  def run(shouldDo: String => Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    _ <- IO(shouldDo("role-questions")).ifM(reportRoleQuestionDists(features, outDir), IO.unit)
    _ <- IO(shouldDo("question-verb-infogain")).ifM(reportQuestionPairInfoGain(features, outDir), IO.unit)
    _ <- IO(shouldDo("question-relatedness")).ifM(reportQuestionPairRelatedness(features, outDir), IO.unit)
    _ <- IO(shouldDo("rule-lexica")).ifM(reportRuleLexica(features, outDir), IO.unit)
  } yield ()

  def reportRoleQuestionDists(
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    argRoleLabels <- features.argRoleLabels.get
    questionDists <- features.argQuestionDists.get
    args <- features.args.get
    roleQuestionDists <- args.toList.infoBarFoldMapM("Aggregating question distributions") {
      case (verbType, argIds) =>
        IO {
          val roleLabels = argRoleLabels(verbType)
          val questions = questionDists(verbType)
          argIds.unorderedFoldMap(argId =>
            Map(roleLabels(argId).role -> questions(argId))
          )
        }
    }
    _ <- FileUtil.writeString(outDir.resolve("questions.txt"))(
      roleQuestionDists.toList.map { case (role, qdist) =>
        val total = qdist.unorderedFold
        s"$role:\n" + qdist.toList
          .sortBy(-_._2)
          .map(p => p.copy(_2 = p._2 / total))
          .takeWhile(_._2 > 0.005)
          .map { case (qt, prob) => f"${qt.toQuestionString}%-60s$prob%.3f" }
          .mkString("\n")
      }.mkString("\n==========\n")
    )
  } yield ()

  val reasonableQuestionFrequencyCutoff = 5.0
  val reasonablePairOneSidedFrequencyCutoff = 15.0

  def writeMaxStatQuestions(
    npmis: Vector[(Duad[QuestionTemplate],EvalUtils.NPMIResult[String])],
    outDir: NIOPath, label: String, stat: EvalUtils.NPMIResult[String] => Double)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val path = outDir.resolve(s"question-$label.tsv")
    Log.infoBranch(s"Writing question ${label}s to $path") {
      FileUtil.writeString(path)(
        npmis
          // .filter(p => stat(p._2) > 0.0)
          .sortBy(p => -stat(p._2))
          .map { case (Duad(q1, q2), result) =>
            val topSources = result.sourceDistribution.toVector.sortBy(-_._2).take(20)
            val topSourcesString = topSources.map(_._1).mkString(", ")
            val topSourceLogProbsString = topSources.map { case (source, prob) =>
              f"${scala.math.log(prob) / scala.math.log(2)}%.2f)"
            }.mkString(", ")
            s"${q1.toQuestionString}\t${q2.toQuestionString}\t${stat(result)}\t${topSourcesString}\t${topSourceLogProbsString}"
          }.mkString("\n")
      )
    }
  }

  case class CovarianceResult(
    verbCovariances: Map[String, Double],
    covarianceVariance: Double
  )

  def mapSqEuclideanDistance[A](x: Map[A, Double], y: Map[A, Double]) = {
    val keys = x.keySet ++ y.keySet
    keys.unorderedFoldMap(k => scala.math.pow(x.getOrElse(k, 0.0) - y.getOrElse(k, 0.0), 2))
  }

  def mapEntropy[A](x: Map[A, Double]): Double = {
    val total = x.unorderedFold
    - x.unorderedFoldMap(v => scala.math.log(v / total) * v / total)
  }

  def mapJensenShannonDivergence[A](x: Map[A, Double], y: Map[A, Double]) = {
    val xTot = x.unorderedFold; val yTot = y.unorderedFold
    val xx = x.mapVals(_ / xTot); val yy = y.mapVals(_ / yTot)
    val mixture = (xx |+| yy).mapVals(_ / 2)
    mapEntropy(mixture) - ((mapEntropy(xx) + mapEntropy(yy) / 2))
  }

  // for each verb, calc covariance
  // for each question pair, calculate variance of covariance across verbs
  // choose maximum
  def reportQuestionPairInfoGain(
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch("Reporting question pair information gain") {
    for {
      questionDists <- features.argQuestionDists.get
      verbQuestionDists <- Log.infoBranch("Verbing question distributions") {
        IO(questionDists.mapVals(_.value.values.toVector).filter(_._2.size > 10))
      }
      verbFlatQuestionDists <- Log.infoBranch("Verbing distributions more") {
        IO(verbQuestionDists.mapVals(_.unorderedFold))
      }
      verbMarginals = verbFlatQuestionDists.mapVals(_.unorderedFold)
      total = verbMarginals.unorderedFold
      marginals <- verbQuestionDists.toVector.infoBarFoldMapM("Computing question marginals")(
        _._2.combineAll.pure[IO]
      )
      totalQuestionCount = marginals.unorderedFold
      acceptableQuestions = {
        marginals
          .filter(_._2 > reasonableQuestionFrequencyCutoff).keySet
          .filter(_.wh == "what".lowerCase)
      }
      _ <- Log.info(s"""|${acceptableQuestions.size} "what" questions
                        |of frequency > $reasonableQuestionFrequencyCutoff."""
                      .stripMargin.replace("\n", " "))
      verbNpmis <- Log.infoBranch("Calculating question NPMIs") {
        verbQuestionDists.toList.traverse { case (verb, dists) =>
          EvalUtils.calculateNPMIsLoggingEfficient(
            dists.map { counts =>
              () -> counts.filter(p => acceptableQuestions.contains(p._1))
            }
          ).map(_.filter(p => p._1.min != p._1.max)).map(verb -> _)
        }.map(_.toMap)
      }
      verbCovariances = verbNpmis.mapVals(_.mapVals(_.covariance))
      // allQPairs = verbNpmis.unorderedFoldMap(_.keySet)
      // qPairVars <- allQPairs.toVector.infoBarFoldMapM("Calculating covariance variances") { qpair =>
      //   val verbCovariances = verbNpmis
      //     .flatMap { case (verb, results) => results.get(qpair).map(verb -> _.covariance) }
      //     // .mapVals(_.apply(qpair).covariance) // TODO will fail?
      //   val meanCov = verbCovariances.toVector.foldMap { case (verb, cov) =>
      //     WeightedNumbers(cov, weight = verbMarginals(verb) / total)
      //   }.stats.weightedMean
      //   // val meanCov = covariances.meanOpt.get
      //   val covVar = verbCovariances.toVector.foldMap { case (verb, cov) =>
      //     WeightedNumbers(cov * cov, weight = verbMarginals(verb) / total)
      //   }.stats.weightedMean - (meanCov * meanCov)
      //   List(qpair -> CovarianceResult(verbCovariances, covVar)).pure[IO]
      // }
      // path = outDir.resolve(s"question-pair-covariance-variances.txt")
      // _ <- Log.infoBranch(s"Writing question pair covariance variances to $path") {
      //   FileUtil.writeString(path)(
      //     qPairVars.sortBy(-_._2.covarianceVariance)
      //       .map { case (Duad(q1, q2), CovarianceResult(verbCovariances, covarianceVariance)) =>
      //         val verbs = verbCovariances.toVector.sortBy(-_._2)
      //         val topVerbs = verbs.take(10)
      //           .map { case (v, cov) => f"$v%s ($cov%.4f)" }
      //           .mkString(", ")
      //         val bottomVerbs = verbs.takeRight(10).reverse
      //           .map { case (v, cov) => f"$v%s ($cov%.4f)" }
      //           .mkString(", ")
      //         f"${q1.toQuestionString}%-45s ${q2.toQuestionString}%-45s $covarianceVariance%.6f $topVerbs // $bottomVerbs"
      //       }.mkString("\n")
      //   )
      // }

      verbMeanNNPath = outDir.resolve(s"verb-qdist-mean-jsd-nn.txt")
      _ <- Log.infoBranch(s"Writing verb question mean nearest neighbors to $verbMeanNNPath") {
        verbFlatQuestionDists.toList.infoBarTraverse("Computing JSD nearest neighbors") { case (v1, qd1) =>
          IO {
            val nns = verbFlatQuestionDists.toList
              .map { case (v2, qd2) => v2 -> mapJensenShannonDivergence(qd1, qd2) }
              .sortBy(-_._2).take(15).mkString(", ")
              // .sortBy(-_._2).take(15).map(_._1).mkString(", ")
            f"$v1%-15s ${nns}%s"
          }
        }.map(_.mkString("\n")).flatMap(FileUtil.writeString(verbMeanNNPath))
      }

      verbQPairCovNNPath = outDir.resolve(s"verb-qpair-cov-nn.txt")
      _ <- Log.infoBranch(s"Writing verb question pair covariance nearest neighbors to $verbQPairCovNNPath") {
        verbCovariances.toList.infoBarTraverse("Computing covariance nearest neighbors") { case (v1, qd1) =>
          IO {
            val nns = verbCovariances.toList
              .map { case (v2, qd2) => v2 -> mapSqEuclideanDistance(qd1, qd2) }
              .sortBy(-_._2).take(15).map(_._1).mkString(", ")
            f"$v1%-15s ${nns}%s"
          }
        }.map(_.mkString("\n")).flatMap(FileUtil.writeString(verbQPairCovNNPath))
      }
    } yield ()
  }

  def reportQuestionPairRelatedness(
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch("Reporting question pair relatedness") {
    for {
      questionDists <- features.argQuestionDists.get
      flatQuestionDists <- Log.infoBranch("Flattening question distributions") {
        IO(questionDists.toList.flatMap { case (verb, argQs) => argQs.value.values.toVector.map(verb -> _) })
      }
      marginals <- flatQuestionDists.toVector.infoBarFoldMapM("Computing question marginals")(_._2.pure[IO])
      totalQuestionCount = marginals.unorderedFold
      marginalsPath = outDir.resolve("question-marginals.txt")
      _ <- Log.infoBranch(s"Writing question marginals to $marginalsPath") {
        FileUtil.writeString(marginalsPath)(
          marginals.toVector.sortBy(-_._2).map { case (q, c) =>
            f"${q.toQuestionString}%-50s $c%9.1f ${c/totalQuestionCount}%.10f"
          }.mkString("\n")
        )
      }
      acceptableQuestions = {
        marginals
          .filter(_._2 > reasonableQuestionFrequencyCutoff).keySet
          .filter(_.wh == "what".lowerCase)
      }
      _ <- Log.info(s"""|${acceptableQuestions.size} "what" questions
                        |of frequency > $reasonableQuestionFrequencyCutoff."""
                      .stripMargin.replace("\n", " "))
      unfilteredNpmis <- Log.infoBranch("Calculating question NPMIs") {
        EvalUtils.calculateNPMIsLoggingEfficient(
          flatQuestionDists.map { case (verb, counts) =>
            verb -> counts.filter(p => acceptableQuestions.contains(p._1))
          }.toVector
        )
      }
      npmis = unfilteredNpmis.toVector.filter(
        p => p._1.min != p._1.max &&
          List(p._1.min, p._1.max).exists(q => marginals(q) > reasonablePairOneSidedFrequencyCutoff)
      )
      dir <- outDir.resolve("question-relatedness").pure[IO].flatTap(createDir)
      _ <- writeMaxStatQuestions(npmis, dir, "npmi", _.npmi)
      _ <- writeMaxStatQuestions(npmis, dir, "pmi", _.pmi)
      _ <- writeMaxStatQuestions(npmis, dir, "correlation", _.correlation)
      _ <- writeMaxStatQuestions(npmis, dir, "covariance", _.covariance)
      _ <- writeMaxStatQuestions(
        npmis.filter(p => !isBoringPair(p._1)),
        dir, "covariance-exciting", _.covariance
      )
    } yield ()
  }

  def isBoringPair(pair: Duad[QuestionTemplate]) = {
    pair.min.copy(prep = pair.max.prep, obj2 = pair.max.obj2) == pair.max
  }

  def reportRuleLexica(
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    lexicaDir <- IO(outDir.resolve("lexica")).flatTap(createDir)
    _ <- {
      def writeLexicon(name: String, lexicon: Set[LowerCaseString], italicize: Boolean) = {
        FileUtil.writeString(lexicaDir.resolve(s"$name.txt"))(
          s"${lexicon.size} items.\n" +
            lexicon.toList.sorted.map(x => if(italicize) s"\\textit{$x}" else x.toString).mkString(", ")
        )
      }
      List(
        "negation" -> SideClusteringModel.negationWords,
        "modals" -> SideClusteringModel.modals,
        "discourse" -> SideClusteringModel.discourseExpressions
      ).traverse { case (name, vocab) =>
          writeLexicon(name, vocab, false) >> writeLexicon(s"$name-it", vocab, true)
      }
    }
  } yield ()
}
