package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration

import fansi.Attr

case class TimingEphemeralTreeFansiLogger(
  logger: RewindingConsoleLineLogger,
  branchBeginTimesMillisAndLevels: Ref[IO, List[(Long, LogLevel)]],
  getLogLevelAttr: LogLevel => Attr,
  timingAttr: Attr,
  minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS))(
  implicit timer: Timer[IO]
) extends EphemeralTreeLogger[IO, String] {
  private[this] val branchEnd = "\u2514"
  private[this] val lastBranch = "\u2514"
  private[this] val midBranch = "\u251C"
  private[this] val vertBranch = "\u2502"

  case class Indents(
    active: String,
    passive: String,
    last: String
  )

  private[this] val getIndents = {
    branchBeginTimesMillisAndLevels.get
      .map(_.map(_._2)).map {
        case Nil => Indents("", "", "")
        case head :: tail =>
          val preIndent = tail.reverse.foldMap(level =>
            getLogLevelAttr(level)(vertBranch).toString + " "
          )
          val headAttr = getLogLevelAttr(head)
          Indents(
            active = preIndent + headAttr(midBranch) + " ",
            passive = preIndent + headAttr(vertBranch) + " ",
            last = preIndent + headAttr(lastBranch) + " "
          )
      }
  }

  private[this] val bigUnitSpecs = {
    import duration._
    List[(TimeUnit, FiniteDuration => Long, String)](
      (DAYS,         (_.toDays),    "d"),
      (HOURS,        (_.toHours),   "h"),
      (MINUTES,      (_.toMinutes), "m"),
      (SECONDS,      (_.toSeconds), "s")
    )
  }

  private[this] val smallUnitSpecs = {
    import duration._
    List[(TimeUnit, FiniteDuration => Long, String)](
      (MILLISECONDS, (_.toMillis),  "ms"),
      (NANOSECONDS, (_.toMillis),  "ns")
    )
  }

  private[this] def getTimingString(_delta: FiniteDuration): String = {
    var delta = _delta
    import duration._
    val bigRes = bigUnitSpecs.flatMap { case (unit, convert, label) => 
      val numUnits = convert(delta)
      if(numUnits > 0) {
        delta = delta - FiniteDuration(numUnits, unit)
        Some(s"${numUnits}${label}")
      } else None
    }.mkString(" ")
    if(bigRes.nonEmpty) bigRes else {
      smallUnitSpecs.map { case (unit, convert, label) =>
        val numUnits = convert(delta)
        if(numUnits > 0) {
          Some(s"${numUnits}${label}")
        } else None
      }.foldK.getOrElse("instant")
    }
  }

  def emit(msg: String, logLevel: LogLevel) =
    getIndents >>= { indents =>
      logger.emit(indents.active + getLogLevelAttr(logLevel)(msg.replaceAll("\n", "\n" + indents.passive)).toString, logLevel)
    }

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A])(
    implicit ambientLevel: LogLevel
  ): IO[A] = for {
    _ <- emit(msg, logLevel)
    beginTime <- timer.clock.monotonic(duration.MILLISECONDS)
    _ <- branchBeginTimesMillisAndLevels.update((beginTime -> logLevel) :: _)
    a <- block(body)
    endTime <- timer.clock.monotonic(duration.MILLISECONDS)
    indents <- getIndents
    _ <- {
      val delta = FiniteDuration(endTime - beginTime, duration.MILLISECONDS)
      if(delta > minElapsedTimeToLog) {
        logger.emit(indents.last + timingAttr(s"Done (${getTimingString(delta)})").toString, logLevel)
      } else IO.unit
    }
    _ <- branchBeginTimesMillisAndLevels.update {
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
object TimingEphemeralTreeFansiLogger {
  def create(
    putStr: String => IO[Unit] = x => IO(print(x)),
    getLogLevelAttr: LogLevel => Attr = freelog.emitters.fansiColorMap,
    timingAttr: Attr = fansi.Color.Blue,
    minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS)
  )(implicit timer: Timer[IO]) = for {
    lineLogger <- RewindingConsoleLineLogger.create(putStr)
    timings <- Ref[IO].of(List.empty[(Long, LogLevel)])
  } yield TimingEphemeralTreeFansiLogger(lineLogger, timings, getLogLevelAttr, timingAttr, minElapsedTimeToLog)
}
