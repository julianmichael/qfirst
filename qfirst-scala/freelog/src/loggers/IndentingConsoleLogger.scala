package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingLogger(
  indentLevel: Ref[IO, Int],
  getIndent: Int => String = i => "  " * i,
  putStrLn: String => IO[Unit] = s => IO(println(s))
) extends TreeLogger[String, IO] {
  def log(msg: String) = for {
    i <- indentLevel.get
    _ <- putStrLn(getIndent(i) + msg)
  } yield ()

  def branch[A](msg: String)(body: IO[A]): IO[A] = for {
    i <- indentLevel.get
    _ <- putStrLn(getIndent(i) + msg)
    _ <- indentLevel.update(_ + 1)
    res <- body
    _ <- indentLevel.update(_ - 1)
  } yield res
}
