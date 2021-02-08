package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration

case class TimingDelayedLogger(
  buffer: Ref[IO, List[List[String]]],
  branchBeginTimesMillis: Ref[IO, List[Long]],
  emitMessage: String => IO[Unit])(
  implicit timer: Timer[IO]
) extends RewindingLogger[IO, String]
    with SequentialEphemeralTreeLogger[IO, String]
    with ProgressBarLogger[IO] {

  val F = implicitly[Monad[IO]]

  override def getLoggableLineLength: IO[Option[Int]] =
    freelog.util.getTerminalWidth[IO].flatMap(widthOpt =>
      widthOpt.traverse(width =>
        branchBeginTimesMillis.get.map(_.size).map(level =>
          (level - 1) + (2 * scala.math.min(level, 1))
        )
      )
    )

  private[this] val lastBranch = "\u2514"
  private[this] val midBranch = "\u251C"
  private[this] val vertBranch = "\u2502"

  def emitBareMsg(msg: String, logLevel: LogLevel) =
    buffer.get >>= {
      case Nil => emitMessage(msg)
      case head :: tail => buffer.set((msg :: head) :: tail)
    }

  override def emit(msg: String, logLevel: LogLevel) = for {
    level <- branchBeginTimesMillis.get.map(_.size)
    indentedMsg = {
      val indent = vertBranch * (level - 1)
      val activeIndent = indent + ((midBranch + " ") * scala.math.min(level, 1))
      val passiveIndent = indent + ((vertBranch + " ") * scala.math.min(level, 1))
      activeIndent + msg.replaceAll("\n", s"\n$passiveIndent")
    }
    _ <- emitBareMsg(indentedMsg, logLevel)
  } yield ()


  override def beginBranch(msg: String, logLevel: LogLevel): IO[Unit] =
    emit(msg, logLevel) >> timer.clock.monotonic(duration.MILLISECONDS) >>= (
      beginTime => branchBeginTimesMillis.update(beginTime :: _)
    )

  override def endBranch(logLevel: LogLevel): IO[Unit] = for {
    beginTime <- branchBeginTimesMillis.get.map(_.head)
    endTime <- timer.clock.monotonic(duration.MILLISECONDS)
    // indents <- getIndents
    delta = FiniteDuration(endTime - beginTime, duration.MILLISECONDS)
    timingString = freelog.util.getTimingString(delta)
    level <- branchBeginTimesMillis.get.map(_.size)
    _ <- {
      val indent = (vertBranch * (level - 1)) + lastBranch
      emitBareMsg(s"$indent Done ($timingString)", logLevel)
    }
    _ <- branchBeginTimesMillis.update {
      case Nil => Nil
      case _ :: rest => rest
    }
  } yield ()

  def restore: IO[Unit] = buffer.get >>= {
    case Nil => IO.unit // nothing to rewind to
    case head :: tail => buffer.set(tail)
  }

  def save: IO[Unit] = buffer.update(Nil :: _)

  def commit: IO[Unit] = buffer.get >>= {
    case Nil => IO.unit // shouldn't happen
    case head :: Nil => head.reverse.traverse(emitMessage) >> buffer.set(Nil) // finally print the logs
    case head :: snd :: tail => buffer.set((head ++ snd) :: tail) // fold into previous checkpoint
  }

  def flush: IO[Unit] = IO.unit // ignore since we're "delayed"
}
object TimingDelayedLogger {
  def console(implicit t: Timer[IO]): IO[TimingDelayedLogger] = for {
    buffer <- Ref[IO].of(List.empty[List[String]])
    branchBeginTimes <- Ref[IO].of(List.empty[Long])
  } yield TimingDelayedLogger(buffer, branchBeginTimes, x => IO(println(x)))

  def file[A](path: java.nio.file.Path)(run: TimingDelayedLogger => IO[A])(implicit t: Timer[IO]): IO[A] =
    for {
      buffer <- Ref[IO].of(List.empty[List[String]])
      branchBeginTimes <- Ref[IO].of(List.empty[Long])
      res <- IO(
        new java.io.PrintWriter(
          java.nio.file.Files.newBufferedWriter(
            path, java.nio.charset.Charset.forName("UTF-8")
          )
        )
      ).bracket(out =>
        run(TimingDelayedLogger(buffer, branchBeginTimes, x => IO(out.println(x))))
      )(out =>
        buffer.get.flatMap((buf: List[List[String]]) =>
          buf.flatten.reverse.traverse(x => IO(out.println(x)))
        ) >> IO(out.close())
      )
    } yield res
}
