package freelog

import cats.Monad
import cats.implicits._

trait SequentialTreeLogger[F[_], Msg] extends TreeLogger[F, Msg] {
  val monad: Monad[F]
  type BranchState
  def beforeBranch(msg: Msg, logLevel: LogLevel): F[BranchState]
  def afterBranch(state: BranchState): F[Unit]
  def emitBranch[A](
    msg: Msg, logLevel: LogLevel)(
    body: F[A]
  ): F[A] = monad.flatMap(beforeBranch(msg, logLevel))(bs =>
    monad.productL(body)(afterBranch(bs))
  )
}
object SequentialTreeLogger {
  // class TreeLoggerSemigroup[F[_]: Apply, Msg] extends Semigroup[TreeLogger[F, Msg]] {
  //   def combine(x: TreeLogger[F, Msg], y: TreeLogger[F, Msg]) = Tee(x, y)
  // }
  // implicit def treeLoggerSemigroup[F[_]: Apply, Msg]: Semigroup[TreeLogger[F, Msg]] = new TreeLoggerSemigroup[F, Msg]

  // class TreeLoggerMonoid[F[_]: Applicative, Msg] extends TreeLoggerSemigroup[F, Msg] with Monoid[TreeLogger[F, Msg]] {
  //   def empty: TreeLogger[F, Msg] = TreeLogger.unit[F, Msg]
  // }
  // implicit def treeLoggerMonoid[F[_]: Applicative, Msg]: Monoid[TreeLogger[F, Msg]] = new TreeLoggerMonoid[F, Msg]
}
