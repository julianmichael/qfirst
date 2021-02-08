package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class RewindingConsoleStringLogger(
  checkpointState: Ref[IO, RewindingConsoleStringLogger.CheckpointState],
  pendingCheckpoint: Ref[IO, Option[RewindingConsoleStringLogger.ConsoleCheckpoint]],
  putStr: String => IO[Unit] = x => IO(print(x)),
  getLogMessage: (String, LogLevel) => String = (x, _) => x
) extends RewindingLogger[IO, String] with ProgressBarLogger[IO] {
  val F = implicitly[Monad[IO]]

  override def getLoggableLineLength: IO[Option[Int]] =
    freelog.util.getTerminalWidth[IO].flatMap(widthOpt =>
      widthOpt.traverse(width =>
        checkpointState.get.map(_.curColumn).map(width - _)
      )
    )

  import RewindingConsoleStringLogger.{CheckpointState, ConsoleCheckpoint}
  private[this] def getCheckpointRestorationStr = {
    pendingCheckpoint.get.flatMap {
      case None => IO.pure("")
      case Some(ConsoleCheckpoint(linesUp, column)) =>
        pendingCheckpoint.set(None) >> checkpointState.get.map(_.curColumn).map(
          curColumn => "\r" + ("\u001b[K\u001b[1A" * linesUp) + (if(column > 0) s"\u001b[${column}C" else "") + "\u001b[K"
        )
    }
  }

  private[this] def getNewColumnForMessage(column: Int, msg: String): Int = {
    if(msg.contains('\n')) {
      msg.size - msg.lastIndexOf('\n') - 1
    } else column + msg.size
  }

  private[this] def sendString(msg: String): IO[Unit] = for {
    // state <- checkpointState.get
    // msg = _msg + s" ($state) "
    backtrackingStr <- getCheckpointRestorationStr
    _ <- checkpointState.update {
      case CheckpointState(Nil, column) =>
        CheckpointState(Nil, getNewColumnForMessage(column, msg))
      case CheckpointState(
        ConsoleCheckpoint(
          linesUp, checkpointColumn
        ) :: rest,
        column
      ) => CheckpointState(
        ConsoleCheckpoint(
          linesUp + msg.count(_ == '\n'), checkpointColumn
        ) :: rest,
        getNewColumnForMessage(column, msg)
      )
    }
    _ <- putStr(backtrackingStr + msg)
  } yield ()

  def emit(msg: String, logLevel: LogLevel): IO[Unit] = {
    sendString(getLogMessage(msg, logLevel))
  }

  def save: IO[Unit] = checkpointState.update(cs =>
    // CheckpointState.checkpoints.modify(ConsoleCheckpoint(0, cs.curColumn) :: _)(cs)
    CheckpointState(
      ConsoleCheckpoint(0, cs.curColumn) :: cs.checkpoints,
      cs.curColumn
    )
  )
  def restore: IO[Unit] = checkpointState.get.map(_.checkpoints).flatMap {
    case Nil => IO.unit // no checkpoint to restore to
    case top :: rest => pendingCheckpoint.update {
      case None => Some(top)
      case Some(curPendingCheckpoint) =>
        Some(curPendingCheckpoint.mergeWithPreceding(top))
    } >> checkpointState.set(CheckpointState(rest, top.column))
  }

  def commit: IO[Unit] = checkpointState.update { cs =>
    cs.checkpoints match {
      case Nil => cs // no previous checkpoint; nothing to do
      case _ :: Nil => cs.copy(checkpoints = Nil) // only one checkpoint; all logs fully committed
      case x :: y :: rest => cs.copy(checkpoints = x.mergeWithPreceding(y) :: rest) // merge last two checkpoints
    }
  }

  // can implement directly?
  def flush: IO[Unit] = sendString("")
}
object RewindingConsoleStringLogger {

  case class ConsoleCheckpoint(linesUp: Int, column: Int) {
    def mergeWithPreceding(preceding: ConsoleCheckpoint) =
      ConsoleCheckpoint(linesUp + preceding.linesUp, preceding.column)
  }
  case class CheckpointState(checkpoints: List[ConsoleCheckpoint], curColumn: Int)

  def create(
    putStr: String => IO[Unit] = x => IO(print(x)),
    getLogMessage: (String, LogLevel) => String = (x, _) => x
  ): IO[RewindingConsoleStringLogger] = for {
    checkpointState <- Ref[IO].of(CheckpointState(List(), 0))
    pendingCheckpoint <- Ref[IO].of(Option.empty[ConsoleCheckpoint])
  } yield RewindingConsoleStringLogger(checkpointState, pendingCheckpoint, putStr, getLogMessage)
}
