package freelog
package loggers

import freelog.instances.fansi._

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
) extends SequentialEphemeralTreeLogger[IO, String] with ProgressBarLogger[IO] {
  val F: Monad[IO] = implicitly[Monad[IO]]

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

  override def getLoggableLineLength: IO[Option[Int]] =
    freelog.util.getTerminalWidth[IO].flatMap(widthOpt =>
      widthOpt.traverse(width =>
        getIndents.map(_.active.length).map(indentLength =>
          scala.math.max(0, width - indentLength)
        )
      )
    )


  def emit(msg: String, logLevel: LogLevel) =
    justDoneMessageBuffer.set(None) >>
      getIndents >>= { indents =>
        getLoggableLineLength >>= { lineLengthOpt =>
          logger.emit(
            indents.active + (
              msg.split("\n")
                .toList
                .flatMap(line =>
                  lineLengthOpt.fold(List(line))(lengthLimit =>
                    line.grouped(lengthLimit).toList
                  )
                )
                .map(getLogLevelAttr(logLevel).apply(_: String))
                .intercalate(fansi.Str("\n") ++ indents.passive)
            ).toString,
            logLevel
          )
        }
      }

  override def beginBranch(msg: String, logLevel: LogLevel): IO[Unit] =
    emit(msg, logLevel) >> timer.clock.monotonic(duration.MILLISECONDS) >>= (
      beginTime => branchBeginTimesMillisAndLevels.update((beginTime -> logLevel) :: _)
    )

  override def endBranch(logLevel: LogLevel): IO[Unit] = for {
    beginTime <- branchBeginTimesMillisAndLevels.get.map(_.head._1)
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
  } yield ()

  override def beginBlock: IO[Unit] = logger.beginBlock
  override def endBlock: IO[Unit] = logger.endBlock

  def rewind: IO[Unit] = logger.rewind

  def flush: IO[Unit] = logger.flush
}
object TimingEphemeralTreeFansiLogger {
  def create(
    putStr: String => IO[Unit] = x => IO(print(x)),
    getLogLevelAttr: LogLevel => Attr = freelog.emitters.fansiColorMap,
    timingAttr: Attr = fansi.Color.Blue,
    minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS))(
    implicit timer: Timer[IO]
  ) = for {
    lineLogger <- RewindingConsoleLineLogger.create(putStr)
    timings <- Ref[IO].of(List.empty[(Long, LogLevel)])
    justDoneMessageBuffer <- Ref[IO].of[Option[(String, NonEmptyList[LogLevel])]](None)
  } yield TimingEphemeralTreeFansiLogger(lineLogger, timings, justDoneMessageBuffer, getLogLevelAttr, timingAttr, minElapsedTimeToLog)

  def debounced(
    putStr: String => IO[Unit] = x => IO(print(x)),
    getLogLevelAttr: LogLevel => Attr = freelog.emitters.fansiColorMap,
    timingAttr: Attr = fansi.Color.Blue,
    minElapsedTimeToLog: FiniteDuration = FiniteDuration(1, duration.SECONDS),
    debounceTime: FiniteDuration = FiniteDuration(20, duration.MILLISECONDS))(
    implicit cs: ContextShift[IO], timer: Timer[IO]
  ) = for {
    baseLogger <- create(putStr, getLogLevelAttr, timingAttr, minElapsedTimeToLog)
    messageQueue <-  Ref[IO].of[List[List[Debounced.LogCommand[String]]]](Nil)
  } yield Debounced(messageQueue, baseLogger, debounceTime)
}
