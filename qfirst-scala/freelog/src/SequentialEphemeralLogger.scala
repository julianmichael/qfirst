package freelog

import cats.Apply

trait SequentialEphemeralLogger[F[_], Msg] extends EphemeralLogger[F, Msg] {
  val F: Apply[F]
  def beginBlock: F[Unit]
  def endBlock: F[Unit]
  override def block[A](fa: F[A]): F[A] =
    F.productR(beginBlock)(
      F.productL(fa)(endBlock)
    )
}
object SequentialEphemeralLogger
