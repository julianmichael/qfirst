package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingConsoleLogger(
  indentLevel: Ref[IO, Int],
  indent: String = "  "
) extends TreeLogger[String, IO] {
  def log(msg: String) = for {
    i <- indentLevel.get
    indentString = indent * i
    _ <- IO(println(indentString + msg))
  } yield ()

  def branch[A](msg: String)(body: IO[A]): IO[A] = for {
    i <- indentLevel.get
    indentString = indent * i
    _ <- IO(println(indentString + msg))
    _ <- indentLevel.update(_ + 1)
    res <- body
    _ <- indentLevel.update(_ - 1)
  } yield res
}
