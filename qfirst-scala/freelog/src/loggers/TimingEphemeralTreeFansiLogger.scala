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
  justDoneMessageBuffer: Ref[IO, Option[(String, NonEmptyList[LogLevel])]],
  getLogLevelAttr: LogLevel => Attr,
  timingAttr: Attr,
  minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS))(
  implicit timer: Timer[IO]
) extends EphemeralTreeLogger[IO, String] {
  val monad: Monad[IO] = implicitly[Monad[IO]]

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


  def emit(msg: String, logLevel: LogLevel) =
    justDoneMessageBuffer.set(None) >> getIndents >>= { indents =>
      logger.emit(indents.active + getLogLevelAttr(logLevel)(msg.replaceAll("\n", "\n" + indents.passive)).toString, logLevel)
    }

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A]
  ): IO[A] = for {
    _ <- emit(msg, logLevel)
    beginTime <- timer.clock.monotonic(duration.MILLISECONDS)
    _ <- branchBeginTimesMillisAndLevels.update((beginTime -> logLevel) :: _)
    a <- block(body)
    endTime <- timer.clock.monotonic(duration.MILLISECONDS)
    indents <- getIndents
    justDoneMsgOpt <- justDoneMessageBuffer.get
    delta = FiniteDuration(endTime - beginTime, duration.MILLISECONDS)
    timingString = freelog.util.getTimingString(delta)
    _ <- {
      justDoneMsgOpt.filter(_._1 == timingString) match {
        case None =>
          logger.emit(indents.last + timingAttr(s"Done ($timingString)").toString, logLevel) >>
            justDoneMessageBuffer.set(Some(timingString -> NonEmptyList.of(logLevel)))
        case Some((timingString, innerLogLevels)) =>
          val boxTee = "\u2534"
          val horiz = "\u2500"
          val upOne = "\r" + ("\u001b[K\u001b[1A" * 1) + "\u001b[K"
          logger.emit(
            upOne +
              indents.last.init + getLogLevelAttr(logLevel)(horiz) +
              innerLogLevels.init.map(level => getLogLevelAttr(level)(boxTee + horiz)).mkString +
              getLogLevelAttr(innerLogLevels.last)(boxTee) +
              timingAttr(s" Done ($timingString)"),
            logLevel
          ) >> justDoneMessageBuffer.set(Some(timingString -> NonEmptyList(logLevel, innerLogLevels.toList)))
      }
    }.whenA(delta > minElapsedTimeToLog)
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
    justDoneMessageBuffer <- Ref[IO].of[Option[(String, NonEmptyList[LogLevel])]](None)
  } yield TimingEphemeralTreeFansiLogger(lineLogger, timings, justDoneMessageBuffer, getLogLevelAttr, timingAttr, minElapsedTimeToLog)
}
