package freelog

import cats.Applicative
import cats.Apply
import cats.Contravariant
import cats.Monoid
import cats.Semigroup
import cats.implicits._

trait Logger[F[_], Msg] {
  def log(msg: Msg): F[Unit]
}
object Logger {
  def apply[F[_], Msg](sendLog: Msg => F[Unit]): Logger[F, Msg] = new Logger[F, Msg] {
    def log(msg: Msg): F[Unit] = sendLog(msg)
  }

  def unit[F[_]: Applicative, Msg] = new Logger[F, Msg] {
    def log(msg: Msg): F[Unit] = Applicative[F].unit
  }

  case class Tee[F[_]: Apply, Msg](
    first: Logger[F, Msg],
    second: Logger[F, Msg]
  ) extends Logger[F, Msg] {
    def log(msg: Msg): F[Unit] = first.log(msg) *> second.log(msg)
  }

  implicit def loggerContravariant[F[_]]: Contravariant[Logger[F, ?]] = new Contravariant[Logger[F, ?]] {
    def contramap[A, B](fa: Logger[F, A])(f: B => A): Logger[F, B] = new Logger[F, B] {
      def log(msg: B): F[Unit] = fa.log(f(msg))
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
