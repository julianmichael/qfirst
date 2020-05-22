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
) extends EphemeralTreeLogger[IO, String] {

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
      s"$indent$midBranch " + msg.replaceAll("\n", s"\n$indent$vertBranch ")
    }
    _ <- emitBareMsg(indentedMsg, logLevel)
  } yield ()

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A])(
    implicit ambientLevel: LogLevel
  ): IO[A] = for {
    _ <- emit(msg, logLevel)
    beginTime <- timer.clock.monotonic(duration.MILLISECONDS)
    _ <- branchBeginTimesMillis.update(beginTime :: _)
    a <- body
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
  } yield a

  def block[A](body: IO[A]): IO[A] =
    buffer.update(Nil :: _) *> body <* (
      buffer.get >>= {
        case Nil => IO.unit // shouldn't happen
        case head :: Nil => head.reverse.traverse(emitMessage) >> buffer.set(Nil)
        case head :: snd :: tail => buffer.set((head ++ snd) :: tail)
      }
    )

  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: IO[Unit] = buffer.get >>= {
    case Nil => IO.unit // nothing to rewind to
    case head :: tail => buffer.set(Nil :: tail)
  }

  /** Flush the buffer to effect the last call to `rewind` */
  def flush: IO[Unit] = IO.unit // ignore since we're "delayed"
}
object TimingDelayedLogger {
  def console(implicit t: Timer[IO]): IO[TimingDelayedLogger] = for {
    buffer <- Ref[IO].of(List.empty[List[String]])
    branchBeginTimes <- Ref[IO].of(List.empty[Long])
  } yield TimingDelayedLogger(buffer, branchBeginTimes, x => IO(println(x)))

  def file[A](path: java.nio.file.Path)(implicit t: Timer[IO]): (TimingDelayedLogger => IO[A]) => IO[A] =
    (run: TimingDelayedLogger => IO[A]) => for {
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