package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class IndentingWriterLogger(
  indent: String = "  "
) extends TreeLogger[String, Writer[String, ?]] {
  def log(msg: String) = Writer.tell(msg + "\n")

  def branch[A](msg: String)(body: Writer[String, A]): Writer[String, A] = for {
    _ <- Writer.tell(msg + s"\n$indent")
                    (nestedLog, res) = body.run
    _ <- Writer.tell(nestedLog.replaceAll("\n", s"\n$indent"))
  } yield res
}
