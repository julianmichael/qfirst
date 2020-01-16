package freelog
import freelog.implicits._

import cats.Applicative
import cats.Foldable
import cats.Monad
import cats.Monoid
import cats.Traverse
import cats.implicits._

trait EphemeralLogger[F[_], Msg] extends Logger[F, Msg] {
  /** Create a rewind block, wherein calls to `rewind` will rewind to the current state */
  def block[A](fa: F[A]): F[A]
  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: F[Unit]
  /** Flush the buffer to effect the last call to `rewind` */
  def flush: F[Unit]

  // utility functions for logging traversals and folds

  def wrapProgressOuter[A](
    prefix: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = {
    block(body)
  }
  def wrapProgressInner[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], index: Long)(
    body: F[A])(
    implicit F: Monad[F], progress: ProgressSpec[Msg], ambientLevel: LogLevel
  ): F[A] = {
    val renderProgress = progress.renderProgress(Some(prefix), sizeHint)
    log(renderProgress(index), logLevel) >> body <* rewind
  }
  def progressEnd[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], total: Long)(
    implicit F: Monad[F], progress: ProgressSpec[Msg], ambientLevel: LogLevel
  ): F[Unit] = {
    val renderProgress = progress.renderProgress(Some(prefix), sizeHint)
    log(renderProgress(total), logLevel)
  }
}
