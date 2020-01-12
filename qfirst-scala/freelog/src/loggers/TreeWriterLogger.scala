package freelog
package loggers

import cats.Applicative
import cats.Monad
import cats.data.Writer
import cats.data.WriterT
import cats.implicits._

case class TreeWriterLogger[Msg](
  getLogMessage: (Msg, LogLevel) => Msg
) extends TreeLogger[Writer[LogTree[Msg], ?], Msg] {
  def emit(msg: Msg, logLevel: LogLevel): Writer[LogTree[Msg], Unit] = Writer.tell(LogTree.labeled(getLogMessage(msg, logLevel), Nil))
  def branch[A](msg: Msg)(body: Writer[LogTree[Msg], A]): Writer[LogTree[Msg], A] = {
    val (logTree, res) = body.run
    val newLogTree = logTree match {
      case LogTree.Unlabeled(children) => LogTree.labeled(msg, children)
      case l @ LogTree.Labeled(_, _) => LogTree.labeled(msg, List(l))
    }
    Writer.tell(newLogTree).as(res)
  }
}

case class TreeWriterTLogger[Msg, F[_]: Applicative](
  getLogMessage: (Msg, LogLevel) => Msg
) extends TreeLogger[WriterT[F, LogTree[Msg], ?], Msg] {
  def emit(msg: Msg, logLevel: LogLevel): WriterT[F, LogTree[Msg], Unit] = WriterT.tell[F, LogTree[Msg]](
    LogTree.labeled(getLogMessage(msg, logLevel), Nil)
  )
  def branch[A](msg: Msg)(body: WriterT[F, LogTree[Msg], A]): WriterT[F, LogTree[Msg], A] = {
    WriterT(
      body.run.map { case (logTree, res) =>
        val newLogTree = logTree match {
          case LogTree.Unlabeled(children) => LogTree.labeled(getLogMessage(msg, LogLevel.Info), children) // TODO
          case l @ LogTree.Labeled(_, _) => LogTree.labeled(getLogMessage(msg, LogLevel.Info), List(l)) // TODO
        }
        newLogTree -> res
      }
    )
  }
}
