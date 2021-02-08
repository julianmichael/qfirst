package freelog

import cats.Applicative
import cats.Monad
import cats.Monoid
import cats.implicits._

trait SequentialEphemeralTreeLogger[F[_], Msg]
    extends SequentialTreeLogger[F, Msg]
    with SequentialEphemeralLogger[F, Msg]
    with EphemeralTreeLogger[F, Msg]

object SequentialEphemeralTreeLogger {

  def unit[F[_]: Monad, Msg] = new SequentialEphemeralTreeLogger[F, Msg] {
    val F = implicitly[Monad[F]]
    def emit(msg: Msg, level: LogLevel): F[Unit] = F.unit

    def emitProgress(
      prefix: Option[Msg],
      sizeHint: Option[Long],
      logLevel: LogLevel,
      current: Long
    ): F[Unit] = F.unit

    def beginBranch(msg: Msg, logLevel: LogLevel): F[Unit] = F.unit
    def endBranch(logLevel: LogLevel): F[Unit] = F.unit

    def beginBlock: F[Unit] = F.unit
    def endBlock: F[Unit] = F.unit

    def rewind: F[Unit] = F.unit
    def flush: F[Unit] = F.unit
  }

  case class Tee[F[_]: Monad, Msg](
    first: SequentialEphemeralTreeLogger[F, Msg],
    second: SequentialEphemeralTreeLogger[F, Msg]
  ) extends SequentialEphemeralTreeLogger[F, Msg] {
    val F = implicitly[Monad[F]]

    def emitProgress(
      prefix: Option[Msg],
      sizeHint: Option[Long],
      logLevel: LogLevel,
      current: Long
    ): F[Unit] = first.emitProgress(prefix, sizeHint, logLevel, current) *>
      second.emitProgress(prefix, sizeHint, logLevel, current)

    def emit(msg: Msg, level: LogLevel): F[Unit] = first.emit(msg, level) *> second.emit(msg, level)

    def beginBranch(msg: Msg, logLevel: LogLevel): F[Unit] =
      first.beginBranch(msg, logLevel) *> second.beginBranch(msg, logLevel)
    def endBranch(logLevel: LogLevel): F[Unit] =
      first.endBranch(logLevel) *> second.endBranch(logLevel)

    def beginBlock: F[Unit] =
      first.beginBlock *> second.beginBlock
    def endBlock: F[Unit] =
      second.endBlock *> first.endBlock

    def rewind: F[Unit] = first.rewind *> second.rewind
    def flush: F[Unit] = first.flush *> second.flush
  }

  // implicit def loggerContravariant[F[_]]: Contravariant[Logger[F, ?]] = new Contravariant[Logger[F, ?]] {
  //   def contramap[A, B](fa: Logger[F, A])(f: B => A): Logger[F, B] = new Logger[F, B] {
  //     def emit(msg: B, level: LogLevel): F[Unit] = fa.emit(f(msg), level)
  //   }
  // }

  class SequentialEphemeralTreeLoggerMonoid[F[_]: Monad, Msg] extends Monoid[SequentialEphemeralTreeLogger[F, Msg]] {
    def combine(x: SequentialEphemeralTreeLogger[F, Msg], y: SequentialEphemeralTreeLogger[F, Msg]): SequentialEphemeralTreeLogger[F, Msg] =
      Tee(x, y)
    def empty: SequentialEphemeralTreeLogger[F, Msg] =
      SequentialEphemeralTreeLogger.unit[F, Msg]
  }
  implicit def sequentialEphemeralTreeLoggerMonoid[F[_]: Monad, Msg]: Monoid[SequentialEphemeralTreeLogger[F, Msg]] =
    new SequentialEphemeralTreeLoggerMonoid[F, Msg]
}
