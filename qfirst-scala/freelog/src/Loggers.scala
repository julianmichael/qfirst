package freelog

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

trait EphemeralLogger[Msg, F[_]] extends Logger[Msg, F] {
  // /** Write a log message past the current checkpoint. */
  // def log(msg: Msg): F[Unit]

  /** Save a new checkpoint. */
  def save: F[Unit]

  /** Restore to the last checkpoint, deleting logs and the checkpoint.
    * Can delete logs lazily, i.e., wait until the next log op to do so.
    */
  def restore: F[Unit]

  /** Commit the last checkpoint, keeping logs and folding the last two checkpoints together. */
  def commit: F[Unit]

  /** Flush a pending restore, i.e., force lazy deletes through. */
  def flush: F[Unit]

  /** Restore to last checkpoint without deleting it. */
  def rewind: F[Unit]// = restore >> save

  /** Replace everything since last checkpoint with msg, keeping the checkpoint. */
  def replace(msg: Msg): F[Unit]// = restore >> save >> log(msg)

  // def reset: F[Unit] // must be equivalent to restore >> save
  // def resetWithLog(msg: Msg): F[Unit] // must be equivalent to restore >> save >> log(msg)
}

object Loggers {

  import monocle.function.{all => Optics}
  import monocle.macros._

  case class ConsoleCheckpoint(linesUp: Int, column: Int) {
    def mergeWithPreceding(preceding: ConsoleCheckpoint) =
      ConsoleCheckpoint(linesUp + preceding.linesUp, preceding.column)
  }
  @Lenses case class CheckpointState(checkpoints: List[ConsoleCheckpoint], curColumn: Int)

  case class EphemeralConsoleLogger(
    checkpointState: Ref[IO, CheckpointState],
    pendingCheckpoint: Ref[IO, Option[ConsoleCheckpoint]],
    putStr: String => IO[Unit] = x => IO(print(x))
  ) extends EphemeralLogger[String, IO] {
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

    def log(msg: String): IO[Unit] = for {
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

    def commit: IO[Unit] = checkpointState.update {
      CheckpointState.checkpoints.modify {
        case Nil => Nil // no previous checkpoint; nothing to do
        case _ :: Nil => Nil // only one checkpoint; all logs fully committed
        case x :: y :: rest => x.mergeWithPreceding(y) :: rest // merge last two checkpoints
      }
    }

    // can implement directly?
    def flush: IO[Unit] = log("")

    def rewind: IO[Unit] = restore >> save

    def replace(msg: String): IO[Unit] = restore >> save >> log(msg)
  }
  object EphemeralConsoleLogger {
    def create(putStr: String => IO[Unit] = x => IO(print(x))): IO[EphemeralConsoleLogger] = for {
      checkpointState <- Ref[IO].of(CheckpointState(List(), 0))
      pendingCheckpoint <- Ref[IO].of(Option.empty[ConsoleCheckpoint])
    } yield EphemeralConsoleLogger(checkpointState, pendingCheckpoint, putStr)
  }

  // case class EphemeralConsoleLogger(
  //   checkpoints: Ref[IO, List[Int]],
  //   pendingDeletes: Ref[IO, Int],
  //   putStr: String => IO[Unit] = x => IO(print(x))
  // ) extends EphemeralLogger[String, IO] {
  //   def log(msg: String): IO[Unit] = for {
  //     numDeletes <- pendingDeletes.getAndSet(0)
  //     _ <- checkpoints.update {
  //       case Nil => Nil
  //       case top :: rest => (top - numDeletes + msg.size) :: rest
  //     }
  //     _ <- putStr(("" * numDeletes) + msg)
  //   } yield ()

  //   def save: IO[Unit] = checkpoints.update(0 :: _)
  //   def restore: IO[Unit] = checkpoints.get.flatMap {
  //     case Nil => IO.unit // no checkpoint to restore to
  //     case top :: rest =>
  //       pendingDeletes.update(_ + top) >>
  //         checkpoints.set(rest)
  //   }
  //   def commit: IO[Unit] = checkpoints.get.flatMap {
  //     case Nil => IO.unit // no prev checkpoint; committed by default
  //     case _ :: Nil => checkpoints.set(Nil) // only checkpoint; all logs fully committed
  //     case x :: y :: rest => checkpoints.set((x + y) :: rest) // merge last two checkpoints
  //   }
  // }

  case class IndentingWriterLogger(
    indent: String = "  "
  ) extends TreeLogger[String, Writer[String, ?]] {
    def log(msg: String) = Writer.tell(msg + "\n")

    def log[A](msg: String, body: Writer[String, A]): Writer[String, A] = for {
      _ <- Writer.tell(msg + s"\n$indent")
      (nestedLog, res) = body.run
      _ <- Writer.tell(nestedLog.replaceAll("\n", s"\n$indent"))
    } yield res
  }

  case class IndentingConsoleLogger(
    indentLevel: Ref[IO, Int],
    indent: String = "  "
  ) extends TreeLogger[String, IO] {
    def log(msg: String) = for {
      i <- indentLevel.get
      indentString = indent * i
      _ <- IO(println(indentString + msg))
    } yield ()

    def log[A](msg: String, body: IO[A]): IO[A] = for {
      i <- indentLevel.get
      indentString = indent * i
      _ <- IO(println(indentString + msg))
      _ <- indentLevel.update(_ + 1)
      res <- body
      _ <- indentLevel.update(_ - 1)
    } yield res
  }
}
