package freelog

import cats.Applicative
import cats.Apply
import cats.Contravariant
import cats.Monoid
import cats.Semigroup
import cats.implicits._

trait Logger[F[_], Msg] {
  def emit(msg: Msg, level: LogLevel): F[Unit]

  def log(msg: Msg, level: LogLevel)(
    implicit ambientLevel: LogLevel,
    F: Applicative[F]
  ): F[Unit] = {
    if(level >= ambientLevel) emit(msg, level)
    else F.unit
  }

  final def debug(msg: Msg)(implicit ambientLevel: LogLevel, F: Applicative[F]): F[Unit] =
    log(msg, LogLevel.Debug)

  final def trace(msg: Msg)(implicit ambientLevel: LogLevel, F: Applicative[F]): F[Unit] =
    log(msg, LogLevel.Trace)

  final def info(msg: Msg)(implicit ambientLevel: LogLevel, F: Applicative[F]): F[Unit] =
    log(msg, LogLevel.Info)

  final def warn(msg: Msg)(implicit ambientLevel: LogLevel, F: Applicative[F]): F[Unit] =
    log(msg, LogLevel.Warn)

  final def error(msg: Msg)(implicit ambientLevel: LogLevel, F: Applicative[F]): F[Unit] =
    log(msg, LogLevel.Error)

}
object Logger {
  def apply[F[_], Msg](send: (Msg, LogLevel) => F[Unit]): Logger[F, Msg] = new Logger[F, Msg] {
    def emit(msg: Msg, level: LogLevel): F[Unit] = emit(msg, level)
  }

  def uniform[F[_], Msg](send: Msg => F[Unit]): Logger[F, Msg] = new Logger[F, Msg] {
    def emit(msg: Msg, level: LogLevel): F[Unit] = send(msg)
  }

  def unit[F[_]: Applicative, Msg] = new Logger[F, Msg] {
    def emit(msg: Msg, level: LogLevel): F[Unit] = Applicative[F].unit
  }

  case class Tee[F[_]: Apply, Msg](
    first: Logger[F, Msg],
    second: Logger[F, Msg]
  ) extends Logger[F, Msg] {
    def emit(msg: Msg, level: LogLevel): F[Unit] = first.emit(msg, level) *> second.emit(msg, level)
  }

  implicit def loggerContravariant[F[_]]: Contravariant[Logger[F, *]] = new Contravariant[Logger[F, *]] {
    def contramap[A, B](fa: Logger[F, A])(f: B => A): Logger[F, B] = new Logger[F, B] {
      def emit(msg: B, level: LogLevel): F[Unit] = fa.emit(f(msg), level)
    }
  }

  class LoggerSemigroup[F[_]: Apply, Msg] extends Semigroup[Logger[F, Msg]] {
    def combine(x: Logger[F, Msg], y: Logger[F, Msg]) = Tee(x, y)
  }
  implicit def loggerSemigroup[F[_]: Apply, Msg]: Semigroup[Logger[F, Msg]] = new LoggerSemigroup[F, Msg]

  class LoggerMonoid[F[_]: Applicative, Msg] extends LoggerSemigroup[F, Msg] with Monoid[Logger[F, Msg]] {
    def empty: Logger[F, Msg] = Logger.unit[F, Msg]
  }
  implicit def loggerMonoid[F[_]: Applicative, Msg]: Monoid[Logger[F, Msg]] = new LoggerMonoid[F, Msg]
}
