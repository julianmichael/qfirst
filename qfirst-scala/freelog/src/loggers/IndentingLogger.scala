package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingLogger(
  indentLevel: Ref[IO, Int],
  putLog: (String, LogLevel) => IO[Unit],
  getIndent: Int => String = i => "  " * i
) extends TreeLogger[IO, String] {
  def emit(msg: String, logLevel: LogLevel) = for {
    i <- indentLevel.get
    _ <- putLog(getIndent(i) + msg, logLevel)
  } yield ()

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A]
  ): IO[A] = for {
    i <- indentLevel.get
    _ <- putLog(getIndent(i) + msg, logLevel)
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
      IndentingLogger(indentLevel, (s, _) => IO(println(s)), getIndent)
    )
  }
}
