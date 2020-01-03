package freelog

import cats.Applicative
import cats.Apply
import cats.Contravariant
import cats.Monoid
import cats.Semigroup
import cats.implicits._

trait Logger[Msg, F[_]] {
  def log(msg: Msg): F[Unit]
}
object Logger {
  def unit[Msg, F[_]: Applicative] = new Logger[Msg, F] {
    def log(msg: Msg): F[Unit] = Applicative[F].unit
  }

  case class Tee[Msg, F[_]: Apply](
    first: Logger[Msg, F],
    second: Logger[Msg, F]
  ) extends Logger[Msg, F] {
    def log(msg: Msg): F[Unit] = first.log(msg) *> second.log(msg)
  }

  def loggerContravariant[F[_]]: Contravariant[Logger[?, F]] = new Contravariant[Logger[?, F]] {
    def contramap[A, B](fa: Logger[A, F])(f: B => A): Logger[B, F] = new Logger[B, F] {
      def log(msg: B): F[Unit] = fa.log(f(msg))
    }
  }

  class LoggerSemigroup[Msg, F[_]: Apply] extends Semigroup[Logger[Msg, F]] {
    def combine(x: Logger[Msg, F], y: Logger[Msg, F]) = Tee(x, y)
  }
  def loggerSemigroup[Msg, F[_]: Apply]: Semigroup[Logger[Msg, F]] = new LoggerSemigroup[Msg, F]

  class LoggerMonoid[Msg, F[_]: Applicative] extends LoggerSemigroup[Msg, F] with Monoid[Logger[Msg, F]] {
    def empty: Logger[Msg, F] = Logger.unit[Msg, F]
  }
  def loggerMonoid[Msg, F[_]: Applicative]: Monoid[Logger[Msg, F]] = new LoggerMonoid[Msg, F]
}

