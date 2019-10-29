package qfirst.frame

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource, Timer}

import fs2.Stream

import java.nio.file.{Path => NIOPath}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService

object VectorFileUtil {

  import java.nio.ByteBuffer
  import java.nio.ByteOrder
  import breeze.linalg._
  import scala.concurrent.duration._

  val bufferNumBytes = 4 * 4096

   val blockingExecutionContext = Resource.make(
     IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2)))
   )(ec => IO(ec.shutdown()))

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
