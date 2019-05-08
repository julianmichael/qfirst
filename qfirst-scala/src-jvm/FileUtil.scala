package qfirst
import qfirst.frames._
import FrameDataWriter.FrameInfo

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource, Timer}

import io.circe.jawn
import io.circe.{Encoder, Decoder}

import fs2.Stream

import java.nio.file.{Path => NIOPath}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService

import io.circe.Printer

object FileUtil {

  sealed trait CompressionSetting
  object CompressionSetting {
    case object Compressed extends CompressionSetting
    case object Uncompressed extends CompressionSetting
    case object Auto extends CompressionSetting
  }

  def useCompression(path: NIOPath, setting: CompressionSetting): Boolean = setting match {
    case CompressionSetting.Compressed => true
    case CompressionSetting.Uncompressed => false
    case CompressionSetting.Auto => path.toString.endsWith(".gz")
  }

  val bufferNumBytes = 4 * 4096

  val blockingExecutionContext =
    Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))))(ec => IO(ec.shutdown()))

  def streamJsonLines[A](
    path: NIOPath, ec: ExecutionContext, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = {
    val fileBytes = fs2.io.file.readAll[IO](path, ec, bufferNumBytes)
    val textBytes = if(useCompression(path, compression)) {
      fileBytes.through(fs2.compress.gunzip(bufferNumBytes))
    } else fileBytes
    textBytes
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .filter(_.nonEmpty)
      .flatMap(line => Stream.fromEither[IO](jawn.decode[A](line).left.map(e => new RuntimeException(s"${e.show}\n$line"))))
  }

  def readJsonLines[A](
    path: NIOPath, compression: CompressionSetting = CompressionSetting.Auto)(
    implicit cs: ContextShift[IO], decoder: Decoder[A]
  ): Stream[IO, A] = {
    Stream.resource(blockingExecutionContext).flatMap { _ec =>
      streamJsonLines(path, _ec)
    }
  }

  def readJson[A: Decoder](path: NIOPath): IO[A] = {
    IO(
      io.circe.jawn.decodeFile[A](new java.io.File(path.toString)) match {
        case Right(a) => a
        case Left(e) => throw new RuntimeException(s"${e.show}")
      }
    )
  }

  def writeString(path: NIOPath)(a: String): IO[Unit] = {
    IO(java.nio.file.Files.write(path, a.getBytes("UTF-8")))
  }

  def writeJson[A: Encoder](path: NIOPath, printer: Printer)(a: A): IO[Unit] = {
    import io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      IO(java.nio.file.Files.write(path, printer.pretty(a.asJson).getBytes("UTF-8")))
  }

  def writeJsonLinesStreaming[A](path: NIOPath, printer: Printer, compression: CompressionSetting = CompressionSetting.Auto)(as: Stream[IO, A])(
    implicit cs: ContextShift[IO], encoder: Encoder[A]
  ): IO[Unit] = {
    import io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      Stream.resource(blockingExecutionContext).flatMap { _ec =>
        val textOut = as
          .map(a => printer.pretty(a.asJson))
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
        val compressedTextOut = if(useCompression(path, compression)) {
          textOut.through(fs2.compress.gzip(bufferNumBytes))
        } else textOut
        compressedTextOut.through(fs2.io.file.writeAll(path, _ec))
      }.compile.drain
  }

  def writeJsonLines[A](path: NIOPath, printer: Printer = io.circe.Printer.noSpaces, compression: CompressionSetting = CompressionSetting.Auto)(as: Seq[A])(
    implicit cs: ContextShift[IO], encoder: Encoder[A]
  ): IO[Unit] = {
    import io.circe.syntax._
    IO(Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))) >>
      Stream.resource(blockingExecutionContext).flatMap { _ec =>
        val textOut = Stream.emits[IO, A](as)
          .map(a => printer.pretty(a.asJson))
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
        val compressedTextOut = if(useCompression(path, compression)) {
          textOut.through(fs2.compress.gzip(bufferNumBytes))
        } else textOut
        compressedTextOut.through(fs2.io.file.writeAll(path, _ec))
      }.compile.drain
  }

  import java.nio.ByteBuffer
  import java.nio.ByteOrder
  import breeze.linalg._
  import scala.concurrent.duration._

  def readDenseFloatVectors(path: NIOPath, dim: Int)(
    implicit cs: ContextShift[IO], t: Timer[IO]
  ) = {
    Stream.resource(blockingExecutionContext).flatMap { _ec =>
      fs2.io.file.readAll[IO](path, _ec, bufferNumBytes)
        .groupWithin(4, 1.minute) // should always do 4 chunk
        .map { c =>
          val bytes = c.toBytes
          ByteBuffer.wrap(bytes.values, bytes.offset, bytes.length).order(ByteOrder.LITTLE_ENDIAN).getFloat
        }
        .groupWithin(dim, 1.minute) // should always do dim chunk
        .map { c =>
          val floats = c.toFloats
          DenseVector.create[Float](floats.values, floats.offset, 1, floats.length)
        }
    }
  }

  def readDenseFloatVectorsNIO(path: NIOPath, dim: Int) = IO {
    import java.nio.file.Files
    val bytes = Files.readAllBytes(path)
    if(bytes.length % 4 != 0) println(s"[=== WARNING ===] number of bytes in embedding file (${bytes.length}) is not divisible by 4")
    val numFloats = bytes.length / 4
    val floats = new Array[Float](numFloats)
    if(numFloats % dim != 0) println(s"[=== WARNING ===] number of floats in embedding file (${numFloats}) is not divisible by embedding dimension ($dim)")
    // val numVectors = numFloats / dim
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).asFloatBuffer.get(floats) // put data in float array
    var offset = 0
    var res = List.empty[DenseVector[Float]]
    while(offset < numFloats) {
      res = DenseVector.create(floats, offset, 1, dim) :: res
      offset = offset + dim
    }
    val reverseRes = res.reverse
    print(s" Read ${res.size} vectors of dimensionality $dim.")
    reverseRes
  }
}
