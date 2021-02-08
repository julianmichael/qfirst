package freelog
package loggers

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

case class EphemeralTreeConsoleLogger(
  logger: RewindingConsoleLineLogger,
  distances: Ref[IO, List[Int]],
  distanceHistory: Ref[IO, List[List[Int]]]
) extends EphemeralTreeLogger[IO, String] with ProgressBarLogger[IO] {
  private[this] val lastBranch = "\u2514"
  private[this] val midBranch = "\u251C"
  private[this] val vertBranch = "\u2502"

  val F = implicitly[Monad[IO]]

  override def getLoggableLineLength: IO[Option[Int]] =
    for {
      dists <- distances.get
      innerLength <- logger.getLoggableLineLength
    } yield innerLength.map(length =>
      scala.math.max(0, length - (dists.size * 2) - 2)
    )

  private[this] def    up(i: Int) = if(i == 0) "" else s"\u001b[${i}A"
  private[this] def  down(i: Int) = if(i == 0) "" else s"\u001b[${i}B"
  private[this] def right(i: Int) = if(i == 0) "" else s"\u001b[${i}C"
  private[this] def  left(i: Int) = if(i == 0) "" else s"\u001b[${i}D"

  def emit(msg: String, logLevel: LogLevel) = distances.get >>= {
    case Nil => logger.emit(msg, logLevel)
    case 0 :: parentDistances =>
      val depth = parentDistances.size
      distances.set(1 :: parentDistances) >>
        logger.emit(("  " * depth) + lastBranch + " " + msg, logLevel)
    case width :: parentDistances =>
      val depth = parentDistances.size
      val backtrack = "\r" + up(width) + right(2 * depth) + midBranch
      val traceDown1 = left(1) + down(1) + vertBranch
      val traceDown = traceDown1 * (width - 1)
      val finish = down(1) + "\r"
      val indent = "  " * depth
      val indentedMsg = msg.replaceAll("\n", s"\n$indent")
      val lastLine = indent + lastBranch + " " + indentedMsg
      val fullMsg = backtrack + traceDown + finish + lastLine
      distances.set(
        parentDistances match {
          case Nil => 1 :: Nil
          case top :: rest => 1 :: (width + top) :: rest
        }
      ) >> logger.emit(fullMsg, logLevel)
  }

  def emitBranch[A](
    msg: String, logLevel: LogLevel)(
    body: IO[A]
  ): IO[A] = {
    emit(msg, logLevel) >> distances.update(0 :: _) >> body <* distances.update {
      case Nil => Nil
      case _ :: Nil => Nil // forget about it if it's the root branch
      case x :: y :: rest => (x + y) :: rest
    }
  }

  def block[A](fa: IO[A]): IO[A] =
    distances.get >>= (curDistances =>
      distanceHistory.update(curDistances :: _) >>
        logger.block(fa) <*
        distanceHistory.update {
          case Nil => ??? // should not happen
          case x :: xs => xs
        }
    )

  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: IO[Unit] = logger.rewind >> distanceHistory.get >>= {
    case Nil => IO.unit // nothing to rewind to
    case top :: _ => distances.set(top)
  }

  /** Flush the buffer to effect the last call to `rewind` */
  def flush: IO[Unit] = logger.flush
}
object EphemeralTreeConsoleLogger {
  def create(putStr: String => IO[Unit] = x => IO(print(x))) = for {
    lineLogger <- RewindingConsoleLineLogger.create(putStr)
    distances <- Ref[IO].of(List.empty[Int])
    distanceHistory <- Ref[IO].of(List.empty[List[Int]])
  } yield EphemeralTreeConsoleLogger(
    lineLogger,
    distances,
    distanceHistory)
}
