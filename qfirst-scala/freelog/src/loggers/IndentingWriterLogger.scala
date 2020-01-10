package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingWriterLogger(
  indent: String = "  "
) extends TreeLogger[Writer[String, ?], String] {
  def log(msg: String) = Writer.tell(msg + "\n")

  def branch[A](msg: String)(body: Writer[String, A]): Writer[String, A] = for {
    _ <- Writer.tell(msg)
    (nestedLog, res) = body.run
    _ <- Writer.tell(s"\n$indent" + nestedLog.init.replaceAll("\n", s"\n$indent") + "\n")
  } yield res
}
