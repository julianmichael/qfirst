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

  def blockTraverse[G[_]: Traverse, A, B](fa: G[A])(f: A => F[B])(
    implicit ap: Applicative[F]
  ) = block(fa.traverse(a => f(a) <* rewind))

  def blockTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A])(f: (A, Int) => F[B])(
    implicit ap: Monad[F]
  ) =
    block(fa.traverseWithIndexM((a, i) => f(a, i) <* rewind))

  // bar if arg present
  def logTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    logLevel: LogLevel,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(Some(prefix), sizeHint)
    block(
      fa.traverseWithIndexAndSizeM((a, i) =>
        log(renderProgress(i), logLevel) >> f(a) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i), logLevel).as(b)
      }
    )
  }
  // never bar
  final def logTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, None)(f)
  // always bar
  final def logBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, Some(size))(f)
  // always bar
  final def logBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, Some(fa.size))(f)

  // sometimes bar
  def logTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(Some(prefix), sizeHint)
    block(
      fa.traverseWithIndexAndSizeM((a, i) =>
        log(renderProgress(i), logLevel) >> f(a, i) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i), logLevel).as(b)
      }
    )
  }
  // never bar
  final def logTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, logLevel, None)(f)
  // always bar
  final def logBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, logLevel,Some(size))(f)
  // always bar
  final def logBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, logLevel, Some(fa.size))(f)

}
