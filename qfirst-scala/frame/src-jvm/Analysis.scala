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

case class Analysis[Arg: Encoder : Decoder : Order](
  features: PropBankFeatures[Arg],
  outDir: NIOPath
) {

  def run(shouldDo: String => Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    _ <- IO(shouldDo("role-questions")).ifM(reportRoleQuestionDists(features, outDir), IO.unit)
    _ <- IO(shouldDo("question-npmi")).ifM(reportQuestionPairNPMIs(features, outDir), IO.unit)
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

  def reportQuestionPairNPMIs(
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch("Reporting question pair NPMIs") {
    for {
      argRoleLabels <- features.argRoleLabels.get
      questionDists <- features.argQuestionDists.get
      flatQuestionDists <- Log.infoBranch("Flattening question distributions") {
        IO(questionDists.toList.foldMap(_._2.value.values.toVector))
      }
      marginals <- flatQuestionDists.infoBarFoldMapM("Computing question marginals")(IO.pure)
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
      npmis <- Log.infoBranch("Calculating question NPMIs") {
        EvalUtils.calculateNPMIsLoggingEfficient(
          flatQuestionDists.map(
            counts => counts.filter(p => acceptableQuestions.contains(p._1))
          )
        )
      }
      outPath = outDir.resolve("question-npmi.txt")
      _ <- Log.infoBranch(s"Writing question NPMIs to $outPath") {
        FileUtil.writeString(outPath)(
          npmis.toVector.sortBy(-_._2).toList.filter(
            p => List(p._1.min, p._1.max).exists(q => marginals(q) > reasonablePairOneSidedFrequencyCutoff)
          ).filter(_._2 > -0.0).map { case (Duad(q1, q2), npmi) =>
              f"${q1.toQuestionString}%-45s ${q2.toQuestionString}%-45s $npmi%.6f ${marginals(q1)}%.6f ${marginals(q2)}%.6f"
          }.mkString("\n")
        )
      }
    } yield ()
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
