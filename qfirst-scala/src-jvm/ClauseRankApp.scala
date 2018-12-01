package qfirst

import qfirst.frames._
import qfirst.metrics._
import FrameDataWriter.FrameInfo

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import io.circe.{Encoder, Decoder}
import io.circe.{KeyEncoder, KeyDecoder}
import io.circe.Json
import io.circe.HCursor

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths
import java.nio.file.Files

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.{AnswerJudgment, Answer, InvalidQuestion}
import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.QuestionLabel
import qasrl.data.Sentence
import qasrl.data.VerbEntry

import HasMetrics.ops._

import fs2.Stream

case class ClauseStructure(
  structure: ArgStructure,
  tan: TAN,
  argSpans: Map[ArgumentSlot, AnswerSpan]
) {
  def getClauseString(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms
  ): String = {
    val frame = Frame(structure, verbInflectedForms, tan)
    val argSpanStrings = argSpans.transform { case (_, span) =>
      Text.renderSpan(sentenceTokens, (span.begin until span.end).toSet)
    }
    frame.clausesWithArgs(argSpanStrings).head
  }
}
object ClauseStructure {
  import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}
  import io.circe.syntax._

  def toJson(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms)(
    struct: ClauseStructure,
    prob: Double
  ): Json = {
    import io.circe.syntax._
    Json.obj(
      "string" -> Json.fromString(struct.getClauseString(sentenceTokens, verbInflectedForms)),
      "structure" -> struct.structure.asJson,
      "tan" -> struct.tan.asJson,
      "argSpans" -> struct.argSpans.asJson,
      "prob" -> prob.asJson
    )
  }

  implicit val clauseStructureDecoder: Decoder[ClauseStructure] = {
    io.circe.generic.semiauto.deriveDecoder[ClauseStructure]
  }
}

case class ClauseInstance(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  clauses: List[(ClauseStructure, Double)], // 0.0 negative, 1.0 positive
)
object ClauseInstance {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  import io.circe.syntax._

  implicit val clauseInstanceEncoder: Encoder[ClauseInstance] = new Encoder[ClauseInstance] {
    final def apply(instance: ClauseInstance): Json = {
      Json.obj(
        "sentenceId" -> Json.fromString(instance.sentenceId),
        "sentenceTokens" -> instance.sentenceTokens.asJson,
        "verbIndex" -> Json.fromInt(instance.verbIndex),
        "verbInflectedForms" -> instance.verbInflectedForms.asJson,
        "clauses" -> instance.clauses.map {
          case (clause, prob) => ClauseStructure.toJson(
            instance.sentenceTokens, instance.verbInflectedForms)(
            clause, prob)
        }.asJson
      )
    }
  }

  implicit val clauseInstanceDecoder: Decoder[ClauseInstance] = new Decoder[ClauseInstance] {
    final def apply(c: HCursor): Decoder.Result[ClauseInstance] = for {
      sentenceId <- c.get[String]("sentenceId")
      sentenceTokens <- c.get[Vector[String]]("sentenceTokens")
      verbIndex <- c.get[Int]("verbIndex")
      verbInflectedForms <- c.get[InflectedForms]("verbInflectedForms")
      clauseJsons <- c.get[List[Json]]("clauses")
      clauses <- clauseJsons.traverse(lcj =>
        lcj.hcursor.get[ClauseStructure]("clause") >>= (clause =>
          lcj.hcursor.get[Double]("prob") map (label =>
            (clause, label)
          )
        )
      )
    } yield ClauseInstance(sentenceId, sentenceTokens, verbIndex, verbInflectedForms, clauses)
  }
}

object ClauseRankApp extends App {
  val sortSpec = {
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
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc
    )
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  val questionThreshold = 0.01
  val spanThreshold = 0.15

  def getGoldClauseStructures(
    verbEntry: VerbEntry,
    goldVerbFrameInfo: Map[String, FrameInfo]
  ) = {
    verbEntry.questionLabels.keys.toList.groupBy(q => goldVerbFrameInfo(q).frame)
      .toList.flatMap { case (frame, qStrings) =>
        qStrings
          .filter(qString => goldVerbFrameInfo(qString).answerSlot match { case Adv(_) => false; case _ => true })
          .map { qString =>
          val answerSlot = goldVerbFrameInfo(qString).answerSlot
          verbEntry.questionLabels(qString)
                 .answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toList
                 .map(s => answerSlot -> Some(s)) ++ List(answerSlot -> None)
        }.sequence
          .map(_.collect { case (slot, Some(span)) => slot -> span }.toMap)
          .map(argMap => ClauseStructure(frame.structure, frame.tan, argMap))
    }
  }

  def getPredClauseStructures(
    verbPred: ClausalVerbPrediction
  ) = {
    verbPred.questions
      .filter(_.questionSlots.answerSlot match { case Adv(_) => false; case _ => true })
      .groupBy(qPred => qPred.questionSlots.structure -> qPred.questionSlots.tan)
      .toList.flatMap { case ((argStructure, tan), qPreds) =>
        qPreds.map { qPred =>
          val answerSlot = qPred.questionSlots.answerSlot
          qPred.answerSpans
            .filter(_._2 >= spanThreshold).map(_._1)
            .map(s => answerSlot -> Some(s)) ++ List(answerSlot -> None)
        }.sequence
          .map(_.collect { case (slot, Some(span)) => slot -> span }.toMap)
          .map(argMap => ClauseStructure(argStructure, tan, argMap))
    }
  }

  def getInstancesForSentence(
    goldSentence: Sentence,
    goldSentenceFrameInfo: Map[Int, Map[String, FrameInfo]],
    sentencePred: ClausalSentencePrediction
  ) = sentencePred.verbs.map { verbPred =>
    val goldClauseStructures = getGoldClauseStructures(
      goldSentence.verbEntries(verbPred.verbIndex), goldSentenceFrameInfo(verbPred.verbIndex)
    )
    val predictedClauseStructures = getPredClauseStructures(verbPred)
    val (positiveClauses, negativeClauses) = predictedClauseStructures.partition(goldClauseStructures.contains)
    val clauses = positiveClauses.map(_ -> 1.0) ++ negativeClauses.map(_ -> 0.0)
    ClauseInstance(
      goldSentence.sentenceId,
      goldSentence.sentenceTokens,
      verbPred.verbIndex,
      verbPred.verbInflectedForms,
      clauses)
  }

  def readClausalPredictions(path: NIOPath) = {
    import io.circe.jawn
    import fs2.{io, text}
    io.file.readAll[IO](path, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(line => Stream.fromEither[IO](jawn.decode[ClausalSentencePrediction](line).left.map(new RuntimeException(_))))
      .handleErrorWith(e => Stream.eval(IO(println(s"Unexpected error when reading prediction JSON: $e"))).drain)
  }

  def readClauseInfo(path: NIOPath) = {
    import qfirst.frames._
    import io.circe.jawn
    import fs2.{io, text}
    io.file.readAll[IO](path, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(line => Stream.fromEither[IO](jawn.decode[FrameInfo](line).left.map(new RuntimeException(_))))
      .handleErrorWith(e => Stream.eval(IO(println(s"Unexpected error when reading clause info JSON: $e"))).drain)
      .map(fi => Map(fi.sentenceId -> Map(fi.verbIndex -> Map(fi.question -> List(fi)))))
      .compile.foldMonoid.map(
      // non-ideal.. would instead want a recursive combine that overrides and doesn't need the end mapping
      _.transform { case (sid, vs) => vs.transform { case (vi, qs) => qs.transform { case (q, fis) => fis.head } } }
    )
  }

  def writeInstances(
    goldPath: NIOPath,
    goldClauseInfoPath: NIOPath,
    predictionsPath: NIOPath,
    outPath: NIOPath
  ): IO[Unit] = {
    import io.circe.syntax._
    import fs2.text
    import io.circe.jawn
    val printer = io.circe.Printer.noSpaces
    for {
      data <- IO(Data.readDataset(goldPath))
      clauseInfo <- readClauseInfo(goldClauseInfoPath)
      _ <- {
        readClausalPredictions(predictionsPath)
          .flatMap(pred => getInstancesForSentence(
                     data.sentences(pred.sentenceId),
                     clauseInfo(pred.sentenceId),
                     pred
                   ).map(IO(_)).foldMap(Stream.eval))
          .map(instance => printer.pretty(instance.asJson))
          .intersperse("\n")
          .through(text.utf8Encode)
          .through(fs2.io.file.writeAll(outPath))
          .compile.drain
      }
    } yield ()
  }

  // dense dev
  println("Dev:")
  writeInstances(
    goldPath = Paths.get("qasrl-v2_1/dense/dev.jsonl.gz"),
    goldClauseInfoPath = Paths.get("clause-data-train-dev.jsonl"),
    predictionsPath = Paths.get("predictions/qfirst-clause-2/predictions-dense.jsonl"),
    outPath = Paths.get("predictions/qfirst-clause-2/ranking-dev.jsonl")
  ).unsafeRunSync()

  // train
  println("Train:")
  writeInstances(
    goldPath = Paths.get("qasrl-v2_1/orig/train.jsonl.gz"),
    goldClauseInfoPath = Paths.get("clause-data-train-dev.jsonl"),
    predictionsPath = Paths.get("predictions/qfirst-clause-2/predictions-train.jsonl"),
    outPath = Paths.get("predictions/qfirst-clause-2/ranking-train.jsonl")
  ).unsafeRunSync()
}
