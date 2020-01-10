package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingLogger(
  indentLevel: Ref[IO, Int],
  putStrLn: String => IO[Unit],
  getIndent: Int => String = i => "  " * i
) extends TreeLogger[IO, String] {
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
object IndentingLogger {
  def console(
    initIndentLevel: Int = 0,
    getIndent: Int => String = i => "  " * i
  ): IO[IndentingLogger] = {
    Ref[IO].of(initIndentLevel).map(indentLevel =>
      IndentingLogger(indentLevel, s => IO(println(s)), getIndent)
    )
  }
}
