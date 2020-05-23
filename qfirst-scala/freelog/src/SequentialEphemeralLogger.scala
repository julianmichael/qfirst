package freelog

import cats.Monad
import cats.implicits._

trait SequentialEphemeralLogger[F[_], Msg] extends EphemeralLogger[F, Msg] {
  implicit val F: Monad[F]
  type BlockState
  def beforeBlock: F[BlockState]
  def afterBlock(state: BlockState): F[Unit]
  override def block[A](fa: F[A]): F[A] =
    beforeBlock >>= (bs =>
      fa <* afterBlock(bs)
    )
}
object SequentialEphemeralLogger
