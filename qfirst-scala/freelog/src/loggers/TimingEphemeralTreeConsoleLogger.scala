package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration

case class TimingEphemeralTreeConsoleLogger(
  logger: RewindingConsoleLineLogger,
  branchBeginTimesMillis: Ref[IO, List[Long]],
  createLogMessage: (String, LogLevel) => String,
  minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS))(
  implicit timer: Timer[IO]
) extends EphemeralTreeLogger[IO, String] {
  private[this] val branchEnd = "\u2514"
  private[this] val lastBranch = "\u2514"
  private[this] val midBranch = "\u251C"
  private[this] val vertBranch = "\u2502"

  // private[this] def getActiveIndent(level: Int) = {
  //   if(level < 1) "" else (vertBranch * (level - 1)) + midBranch
  // }
  private[this] def getPassiveIndent(level: Int) = {
    if(level < 1) "" else ((vertBranch + " ") * (level - 1))
  }

  private[this] val getIndent = {
    branchBeginTimesMillis.get
      .map(times => getPassiveIndent(times.size))
  }

  // private[this] val getActiveIndent = {
  //   branchBeginTimesMillis.get
  //     .map(times =>
  //       if(times < 1) ""
  //       else getPassiveIndent(times.size) + midBranch + " "
  //     )
  // }

  def emit(msg: String, logLevel: LogLevel) = branchBeginTimesMillis.get.map(_.size)
    .flatMap { indentLevel =>
      val passiveIndent = getPassiveIndent(indentLevel)
      val activeIndent = {
        if(indentLevel < 1) ""
        else passiveIndent + midBranch + " "
      }
      logger.emit(activeIndent + createLogMessage(msg.replaceAll("\n", "\n" + passiveIndent), logLevel), logLevel)
    }

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A]
  ): IO[A] = for {
    _ <- emit(msg, logLevel)
    beginTime <- timer.clock.monotonic(duration.MILLISECONDS)
    _ <- branchBeginTimesMillis.update(beginTime :: _)
    a <- block(body)
    endTime <- timer.clock.monotonic(duration.MILLISECONDS)
    indent <- getIndent
    _ <- {
      val delta = FiniteDuration(endTime - beginTime, duration.MILLISECONDS)
      if(delta > minElapsedTimeToLog) {
        logger.emit(indent + branchEnd + s" Done (${freelog.util.getTimingString(delta)})", logLevel)
      } else IO.unit
    }
    _ <- branchBeginTimesMillis.update {
      case Nil => Nil
      case _ :: rest => rest
    }
  } yield a

  // No weirdness needs to happen here because branches are guaranteed to block
  def block[A](fa: IO[A]): IO[A] = logger.block(fa)

  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: IO[Unit] = logger.rewind

  /** Flush the buffer to effect the last call to `rewind` */
  def flush: IO[Unit] = logger.flush
}
object TimingEphemeralTreeConsoleLogger {
  def create(
    putStr: String => IO[Unit] = x => IO(print(x)),
    getLogMessage: (String, LogLevel) => String = (x, _) => x,
    minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS)
  )(implicit timer: Timer[IO]) = for {
    lineLogger <- RewindingConsoleLineLogger.create(putStr)
    timings <- Ref[IO].of(List.empty[Long])
  } yield TimingEphemeralTreeConsoleLogger(lineLogger, timings, getLogMessage, minElapsedTimeToLog)
}
