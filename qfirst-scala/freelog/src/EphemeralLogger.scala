package freelog

import cats.Applicative
import cats.Monad
import cats.Traverse
import cats.implicits._

trait EphemeralLogger[F[_], Msg] extends Logger[F, Msg] {
  /** Create a rewind block, wherein calls to `rewind` will rewind to the current state */
  def block[A](fa: F[A]): F[A]
  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: F[Unit]
  /** Flush the buffer to effect the last call to `rewind` */
  def flush: F[Unit]
  /** Rewind and log */
  def replace(msg: Msg): F[Unit]

  def blockTraverse[G[_]: Traverse, A, B](fa: G[A])(f: A => F[B])(
    implicit ap: Applicative[F]
  ) = block(fa.traverse(a => rewind *> f(a)))

  def blockTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A])(f: (A, Int) => F[B])(
    implicit ap: Monad[F]
  ) =
    block(fa.traverseWithIndexM((a, i) => rewind >> f(a, i)))


  // bar if arg present
  def progTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: String, sizeHint: Option[Long])(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(prefix, sizeHint)
    blockTraverseWithIndexM(fa)((a, i) =>
      log(renderProgress(i)) >> f(a)
    )
  }
  // never bar
  final def progTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: String)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverse(fa, prefix, None)(f)
  // always bar
  final def progBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: String, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverse(fa, prefix, Some(size))(f)
  // always bar
  final def progBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: String)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverse(fa, prefix, Some(fa.size))(f)

  // sometimes bar
  def progTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: String, sizeHint: Option[Long])(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(prefix, sizeHint)
    blockTraverseWithIndexM(fa)((a, i) =>
      log(renderProgress(i)) >> f(a, i)
    )
  }
  // never bar
  final def progTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: String)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverseWithIndexM(fa, prefix, None)(f)
  // always bar
  final def progBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: String, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverseWithIndexM(fa, prefix, Some(size))(f)
  // always bar
  final def progBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: String)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F]
  ): F[G[B]] = progTraverseWithIndexM(fa, prefix, Some(fa.size))(f)

}
