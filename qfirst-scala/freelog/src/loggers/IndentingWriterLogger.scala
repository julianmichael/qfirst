package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingWriterLogger(
  indent: String = "  ",
  getLogMessage: (String, LogLevel) => String
) extends TreeLogger[Writer[String, *], String] {
  def emit(msg: String, logLevel: LogLevel) = Writer.tell(getLogMessage(msg, logLevel) + "\n")

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: Writer[String, A]
  ): Writer[String, A] = for {
    _ <- Writer.tell(getLogMessage(msg, logLevel))
    (nestedLog, res) = body.run
    _ <- Writer.tell(s"\n$indent" + nestedLog.init.replaceAll("\n", s"\n$indent") + "\n")
  } yield res
}
