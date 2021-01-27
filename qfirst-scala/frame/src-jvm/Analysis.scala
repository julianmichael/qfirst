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
          .map { case (qt, prob) => f"${qt.toTemplateString}%-60s$prob%.3f" }
          .mkString("\n")
      }.mkString("\n==========\n")
    )
  } yield ()

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
      npmis <- Log.infoBranch("Calculating question NPMIs") {
        IO(eval.EvalUtils.calculateNPMIs(flatQuestionDists))
      }
      outPath = outDir.resolve("question-npmi.txt")
      _ <- Log.infoBranch(s"Writing question NPMIs to $outPath") {
        FileUtil.writeString(outPath)(
          npmis.toVector.sortBy(-_._2).toList.map { case (Duad(q1, q2), npmi) =>
            f"${q1.toTemplateString}%-50s ${q2.toTemplateString}%-50s $npmi%.6f"
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
