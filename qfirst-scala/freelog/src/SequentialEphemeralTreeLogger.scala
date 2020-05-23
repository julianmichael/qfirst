package freelog

import cats.Monad
import cats.Monoid
import cats.implicits._

trait SequentialEphemeralTreeLogger[F[_], Msg]
    extends SequentialTreeLogger[F, Msg]
    with SequentialEphemeralLogger[F, Msg]
    with EphemeralTreeLogger[F, Msg]

object SequentialEphemeralTreeLogger {

  def unit[F[_]: Monad, Msg](implicit _F: Monad[F]) = new SequentialEphemeralTreeLogger[F, Msg] {
    implicit val F = _F
    def emit(msg: Msg, level: LogLevel): F[Unit] = F.unit

    type BranchState = Unit
    def beforeBranch(msg: Msg, logLevel: LogLevel): F[BranchState] = F.unit
    def afterBranch(state: BranchState): F[Unit] = F.unit

    type BlockState = Unit
    def beforeBlock: F[BlockState] = F.unit
    def afterBlock(state: BlockState): F[Unit] = F.unit

    def rewind: F[Unit] = F.unit
    def flush: F[Unit] = F.unit
  }

  case class Tee[F[_], Msg](
    first: SequentialEphemeralTreeLogger[F, Msg],
    second: SequentialEphemeralTreeLogger[F, Msg])(
    implicit val F: Monad[F]
  ) extends SequentialEphemeralTreeLogger[F, Msg] {
    def emit(msg: Msg, level: LogLevel): F[Unit] = first.emit(msg, level) *> second.emit(msg, level)

    type BranchState = (first.BranchState, second.BranchState)
    def beforeBranch(msg: Msg, logLevel: LogLevel): F[BranchState] =
      first.beforeBranch(msg, logLevel).product(second.beforeBranch(msg, logLevel))
    def afterBranch(state: BranchState): F[Unit] =
      first.afterBranch(state._1) *> (second.afterBranch(state._2))

    type BlockState = (first.BlockState, second.BlockState)
    def beforeBlock: F[BlockState] =
      first.beforeBlock.product(second.beforeBlock)
    def afterBlock(state: BlockState): F[Unit] =
      second.afterBlock(state._2) *> first.afterBlock(state._1)

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
