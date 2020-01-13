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
  final def debugTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Debug, sizeHint)(f)
  final def traceTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Trace, sizeHint)(f)
  final def infoTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Info, sizeHint)(f)
  final def warnTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Warn, sizeHint)(f)
  final def errorTraverse[G[_]: Traverse, A, B](
    fa: G[A], prefix: Msg,
    sizeHint: Option[Long])(
    f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Error, sizeHint)(f)

  // never bar
  final def logTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, None)(f)
  final def debugTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Debug, None)(f)
  final def traceTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Trace, None)(f)
  final def infoTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Info, None)(f)
  final def warnTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Warn, None)(f)
  final def errorTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Error, None)(f)

  // always bar
  final def logBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, Some(size))(f)
  final def debugBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Debug, Some(size))(f)
  final def traceBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Trace, Some(size))(f)
  final def infoBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Info, Some(size))(f)
  final def warnBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Warn, Some(size))(f)
  final def errorBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Error, Some(size))(f)

  // always bar
  final def logBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, logLevel, Some(fa.size))(f)
  final def debugBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Debug, Some(fa.size))(f)
  final def traceBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Trace, Some(fa.size))(f)
  final def infoBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Info, Some(fa.size))(f)
  final def warnBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Warn, Some(fa.size))(f)
  final def errorBarTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverse(fa, prefix, LogLevel.Error, Some(fa.size))(f)

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
  final def debugTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Debug, None)(f)
  final def traceTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Trace, None)(f)
  final def infoTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Info, None)(f)
  final def warnTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Warn, None)(f)
  final def errorTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Error, None)(f)

  // always bar
  final def logBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, logLevel, Some(size))(f)
  final def debugBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Debug, Some(size))(f)
  final def traceBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Trace, Some(size))(f)
  final def infoBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Info, Some(size))(f)
  final def warnBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Warn, Some(size))(f)
  final def errorBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, size: Long)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Error, Some(size))(f)

  // always bar
  final def logBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, logLevel, Some(fa.size))(f)
  final def debugBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Debug, Some(fa.size))(f)
  final def traceBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Trace, Some(fa.size))(f)
  final def infoBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Info, Some(fa.size))(f)
  final def warnBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Warn, Some(fa.size))(f)
  final def errorBarTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg)(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], ap: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = logTraverseWithIndexM(fa, prefix, LogLevel.Error, Some(fa.size))(f)

}
