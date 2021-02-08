package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class RewindingConsoleLineLogger(
  checkpointState: Ref[IO, List[Int]],
  pendingCheckpoint: Ref[IO, Option[Int]],
  putStr: String => IO[Unit] = x => IO(print(x)),
  createLogMessage: (String, LogLevel) => String = (x, _) => x
) extends RewindingLogger[IO, String] with ProgressBarLogger[IO] {
  val F = implicitly[Monad[IO]]

  override def getLoggableLineLength: IO[Option[Int]] =
    freelog.util.getTerminalWidth[IO]

  private[this] def flushWithMessage(msg: String) = for {
    backtrackingStr <- pendingCheckpoint.get.flatMap {
      case None => IO.pure("")
      case Some(pendingLinesBack) =>
        pendingCheckpoint.set(None).as(
          "\r" + ("\u001b[K\u001b[1A" * pendingLinesBack) + "\u001b[K"
        )
    }
    _ <- checkpointState.update {
      case Nil => Nil
      case linesUp :: rest =>
        // accommodate for 1 up-movement, specifically for the fansi logger.
        // TODO: clean up the fansi logger so this isn't necessary, or,
        // clean this up to properly rewind cursor movement.
        (linesUp + msg.count(_ == '\n') - (msg.split("\u001b\\[1A", -1).size - 1) ) :: rest
    }
    _ <- putStr(backtrackingStr + msg)
  } yield ()

  def emit(msg: String, logLevel: LogLevel): IO[Unit] = flushWithMessage(createLogMessage(msg, logLevel) + "\n")

  def save: IO[Unit] = checkpointState.update(0 :: _)

  def restore: IO[Unit] = checkpointState.get.flatMap {
    case Nil => IO.unit // no checkpoint to restore to
    case prev :: rest => pendingCheckpoint.update {
      case None => Some(prev)
      case Some(cur) =>
        Some(cur + prev)
    } >> checkpointState.set(rest)
  }

  def commit: IO[Unit] = checkpointState.update {
    case Nil => Nil // no previous checkpoint; nothing to do
    case _ :: Nil => Nil // only one checkpoint; all logs fully committed
    case x :: y :: rest => (x + y) :: rest // merge last two checkpoints
  }

  // can implement directly?
  def flush: IO[Unit] = flushWithMessage("")
}

object RewindingConsoleLineLogger {
  def create(
    putStr: String => IO[Unit] = x => IO(print(x)),
    createLogMessage: (String, LogLevel) => String = (x, _) => x
  ): IO[RewindingConsoleLineLogger] = for {
    checkpointState <- Ref[IO].of(List.empty[Int])
    pendingCheckpoint <- Ref[IO].of(Option.empty[Int])
  } yield RewindingConsoleLineLogger(checkpointState, pendingCheckpoint, putStr, createLogMessage)
}
