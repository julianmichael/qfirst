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
  putStr: String => IO[Unit] = x => IO(print(x))
) extends RewindingLogger[String, IO] {
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
        (linesUp + msg.count(_ == '\n')) :: rest
    }
    _ <- putStr(backtrackingStr + msg)
  } yield ()

  def log(msg: String): IO[Unit] = flushWithMessage(msg + "\n")

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

  def rewind: IO[Unit] = restore >> save

  def replace(msg: String): IO[Unit] = restore >> save >> log(msg)

  def block[A](fa: IO[A]): IO[A] = save >> fa <* commit
}

object RewindingConsoleLineLogger {
  def create(putStr: String => IO[Unit] = x => IO(print(x))): IO[RewindingConsoleLineLogger] = for {
    checkpointState <- Ref[IO].of(List.empty[Int])
    pendingCheckpoint <- Ref[IO].of(Option.empty[Int])
  } yield RewindingConsoleLineLogger(checkpointState, pendingCheckpoint, putStr)
}
