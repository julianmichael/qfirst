package qfirst.clause.align

import java.io.File
import java.nio.file.Paths
import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._

import com.monovore.decline._
import com.monovore.decline.effect._

import shapeless.syntax.singleton._
// import shapeless.labelling._
import shapeless._

import cats.effect._
import cats.implicits._

import fs2.Stream

import jjm.ling.en.InflectedForms

import qasrl.PresentTense
import qasrl.labeling.SlotBasedLabel
import qasrl.bank.Data

import qfirst.clause._
import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

object Convert extends CommandIOApp(
  name = "qfirst.clause-align.Convert",
  header = "Runs clause resolution on CSV inputs and reports results.") {

  def main: Opts[IO[ExitCode]] = {
    val inPathO = Opts.option[NIOPath](
      "in", metavar = "path", help = "Input CSV path."
    )
    val outPathO = Opts.option[NIOPath](
      "out", metavar = "path", help = "Output path."
    )
    (inPathO, outPathO).mapN(run)
  }

  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

  def readCSV[A](path: NIOPath)(f: (List[String], Stream[IO, Map[String, String]]) => IO[A]): IO[A] = {
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVReader.open(new File(path.toString))))(
      use = reader => {
        IO(reader.readNext.get) >>= (headers =>
          f(headers,
            Stream.fromIterator[IO, Map[String, String]](
              reader.all.iterator.map(line => headers.zip(line).toMap)
            )
          )
        )
      })(
      release = reader => IO(reader.close()))
  }

  def writeCSV(path: NIOPath, headers: List[String])(lines: Stream[IO, Map[String, String]]): IO[Unit] = {
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVWriter.open(new File(path.toString))))(
      use = writer => {
        IO(writer.writeRow(headers)) >>
          lines.evalMap { line =>
            IO(writer.writeRow(headers.map(line)))
          }.compile.drain
      })(
      release = writer => IO(writer.close()))
  }

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List("mean" :: inc)
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  val newHeaders = List("clause+arg", "templated_clause+arg", "aligned_question")

  case class VerbInfo(
    sentenceId: String,
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    sentenceTokens: Vector[String],
    originalLines: List[Map[String, String]]
  ) {
    val questions = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
      sentenceTokens, verbInflectedForms, originalLines.map(_.apply("question"))
    ).map(_.getOrElse((println(this): Id[Unit]) >> ???))

    val resolvedQAPairs = ClauseResolution.getResolvedFramePairs(
      verbInflectedForms, questions
    ).zip(originalLines.map(_.apply("answer")))

    val resolvedQAPairsByFrame = resolvedQAPairs.groupBy(_._1._1)

    val resolvedQAPairsByStructure = resolvedQAPairs.groupBy(
      p => ArgStructure(p._1._1.args, p._1._1.isPassive)
    )

    val newLines = (originalLines, resolvedQAPairs).zipped.map {
      case (fields, ((frame, slot), answer)) =>
        val templatedFrame = frame.copy(
          tense = PresentTense, isPerfect = false,
          isProgressive = false, isNegated = false
        )
        val argMappings = resolvedQAPairsByStructure(
          ArgStructure(frame.args, frame.isPassive)
        ).map { case ((_, slot), answer) =>
            answer.split("~!~").toList.map(slot -> _)
        }.sequence.map(_.toMap)
        fields +
          ("clause+arg" -> s"${frame.clauses.head}: $slot") +
          ("templated_clause+arg" -> s"${templatedFrame.clauses.head}: $slot") +
          ("aligned_question" -> argMappings.map(argMapping =>
             frame.questionsForSlotWithArgs(slot, argMapping).head
           ).mkString("~!~"))
    }

    val metrics = {
      "placeholder coverage (same clause, by clause)" ->>
        resolvedQAPairsByFrame.toList.foldMap { case (frame, questions) =>
          val (covered, uncovered) = frame.args.keySet
            .partition(questions.map(_._1._2).toSet.contains)
          Proportion.Stats(covered.size, uncovered.size)
        } ::
      "placeholder coverage (same clause, by question)" ->>
        resolvedQAPairsByFrame.toList.foldMap { case (frame, questions) =>
          val total = questions.size
          val (covered, uncovered) = frame.args.keySet
            .partition(questions.map(_._1._2).toSet.contains)
          Proportion.Stats(covered.size * total, uncovered.size * total)
        } ::
      "placeholder coverage (same clause (no tense), by clause (no tense))" ->>
        resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
          val (covered, uncovered) = frame.args.keySet
            .partition(questions.map(_._1._2).toSet.contains)
          Proportion.Stats(covered.size, uncovered.size)
        } ::
      "placeholder coverage (same clause (no tense), by question)" ->>
        resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
          val total = questions.size
          val (covered, uncovered) = frame.args.keySet
            .partition(questions.map(_._1._2).toSet.contains)
          Proportion.Stats(covered.size * total, uncovered.size * total)
        } ::
      HNil
    }
  }

  def run(inPath: NIOPath, outPath: NIOPath): IO[ExitCode] = for {
    qasrlData <- IO(Data.readFromQasrlBank(qasrlBankPath).get).map(_.all)
    (verbs, headers) <- readCSV(inPath)(
      (headers, lines) => lines
        .groupAdjacentBy(fields => fields("qasrl_id") -> fields("verb_idx"))
        .map { case ((sid, verbIndex), chunk) =>
          val sentence = qasrlData.sentences(sid)
          VerbInfo(
            sid,
            verbIndex.toInt,
            sentence.verbEntries(verbIndex.toInt).verbInflectedForms,
            sentence.sentenceTokens,
            chunk.toList)
        }.compile.toList.map(_ -> headers)
    )
    _ <- IO(println(getMetricsString(verbs.foldMap(_.metrics))))
    _ <- writeCSV(outPath, headers ++ newHeaders)(
      Stream.emits(verbs.flatMap(_.newLines))
    )
  } yield ExitCode.Success
}
