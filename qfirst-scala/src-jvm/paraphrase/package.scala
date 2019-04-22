package qfirst

import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

package object paraphrase {
  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))
}

