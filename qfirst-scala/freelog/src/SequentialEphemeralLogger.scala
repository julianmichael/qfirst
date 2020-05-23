package freelog

import cats.Monad

trait SequentialEphemeralLogger[F[_], Msg] extends EphemeralLogger[F, Msg] {
  val monad: Monad[F]
  type BlockState
  def beforeBlock: F[BlockState]
  def afterBlock(state: BlockState): F[Unit]
  override def block[A](fa: F[A]): F[A] =
    monad.flatMap(beforeBlock)(bs =>
      monad.productL(fa)(afterBlock(bs))
    )
}
object SequentialEphemeralLogger
