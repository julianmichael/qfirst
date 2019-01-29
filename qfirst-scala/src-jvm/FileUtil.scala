package qfirst
import qfirst.frames._
import FrameDataWriter.FrameInfo

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource}

import io.circe.jawn
import io.circe.{Encoder, Decoder}

import fs2.text
import fs2.Stream

import java.nio.file.{Path => NIOPath}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService

import io.circe.Printer

object FileUtil {

  val blockingExecutionContext =
    Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))))(ec => IO(ec.shutdown()))

  def streamJsonLines[A](
    path: NIOPath, ec: ExecutionContext)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = fs2.io.file.readAll[IO](path, ec, 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .filter(_.nonEmpty)
    .flatMap(line => Stream.fromEither[IO](jawn.decode[A](line).left.map(new RuntimeException(_))))

  def readJsonLines[A](
    path: NIOPath)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = {
    Stream.resource(blockingExecutionContext).flatMap { _ec =>
      streamJsonLines(path, _ec)
    }
  }

  def readJson[A: Decoder](path: NIOPath): IO[A] = {
    IO(io.circe.jawn.decodeFile[A](new java.io.File(path.toString)).right.get)
  }

  def writeJson[A: Encoder](path: NIOPath, printer: Printer)(a: A): IO[Unit] = {
    import io.circe.syntax._
    IO(java.nio.file.Files.write(path, printer.pretty(a.asJson).getBytes("UTF-8")))
  }

  def readClauseInfo(
    path: NIOPath)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, Map[Int, Map[String, FrameInfo]]]] = {
    readJsonLines[FrameInfo](path)
      .map(fi => Map(fi.sentenceId -> Map(fi.verbIndex -> Map(fi.question -> List(fi)))))
      .compile.foldMonoid.map(
      // non-ideal.. would instead want a recursive combine that overrides and doesn't need the end mapping
      _.transform { case (sid, vs) => vs.transform { case (vi, qs) => qs.transform { case (q, fis) => fis.head } } }
    )
  }

  def readClausalPredictions(
    path: NIOPath)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, ClausalSentencePrediction]] = {
    readJsonLines[ClausalSentencePrediction](path)
      .compile.fold(Map.empty[String, ClausalSentencePrediction]) {
        (m, pred) => m + (pred.sentenceId -> pred)
      }
  }

  def readPredictions(
    path: NIOPath, clausalInputType: Boolean)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, SentencePrediction]] = {
    if(!clausalInputType) {
      readJsonLines[SentencePrediction](path)
        .compile.fold(Map.empty[String, SentencePrediction]) {
          (m, pred) => m + (pred.sentenceId -> pred)
        }
    } else {
      readJsonLines[ClausalSentencePrediction](path)
        .compile.fold(Map.empty[String, SentencePrediction]) {
          (m, pred) => m + (pred.sentenceId -> pred.toSentencePrediction)
        }
    }
  }

  def streamE2EPredictions(
    clauseStringToStructure: Map[String, ArgStructure],
    path: NIOPath)(
    implicit cs: ContextShift[IO]
  ): Stream[IO, E2ESentencePrediction] = {
    Stream.resource(blockingExecutionContext).flatMap { _ec =>
      streamJsonLines[E2ESentencePrediction](path, _ec)(cs, E2ESentencePrediction.decoder(clauseStringToStructure))
    }
  }

  def readE2EPredictions(
    clauseStringToStructure: Map[String, ArgStructure],
    path: NIOPath)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, E2ESentencePrediction]] = {
    readJsonLines[E2ESentencePrediction](path)(cs, E2ESentencePrediction.decoder(clauseStringToStructure))
      .compile.fold(Map.empty[String, E2ESentencePrediction]) {
        (m, pred) => m + (pred.sentenceId -> pred)
      }
  }

  def readRankingPredictions(
    path: NIOPath)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, Map[Int, ClauseInstance]]] = {
    readJsonLines[ClauseInstance](path)
      .compile.toList
      .map(_.groupBy(_.sentenceId).transform { case (_, p) => p.map(i => i.verbIndex -> i).toMap })
  }

}
