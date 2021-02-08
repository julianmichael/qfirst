package freelog
import freelog.implicits._

import cats.Applicative
import cats.Monad
import cats.implicits._

trait EphemeralLogger[F[_], Msg] extends Logger[F, Msg] {
  def emitProgress(
    prefix: Option[Msg],
    sizeHint: Option[Long],
    level: LogLevel,
    current: Long): F[Unit]

  def logProgress(
    prefix: Option[Msg],
    sizeHint: Option[Long],
    level: LogLevel,
    current: Long)(
    implicit ambientLevel: LogLevel, F: Applicative[F]
  ): F[Unit] = {
    if(level >= ambientLevel) emitProgress(prefix, sizeHint, level, current)
    else F.unit
  }

  /** Create a rewind block, wherein calls to `rewind` will rewind to the current state */
  def block[A](fa: F[A]): F[A]
  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: F[Unit]
  /** Flush the buffer to effect the last call to `rewind` */
  def flush: F[Unit]

  // utility functions for logging traversals and folds

  def getLoggableLineLength(implicit F: Applicative[F]): F[Option[Int]] = none[Int].pure[F]

  def wrapProgressOuter[A](
    prefix: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = {
    block(body)
  }
  def wrapProgressInnerUsesPrefix: Boolean = true
  def wrapProgressInner[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], index: Long)(
    body: F[A])(
    implicit F: Monad[F], ambientLevel: LogLevel
  ): F[A] = {
    val prefixOpt = if(wrapProgressInnerUsesPrefix) Some(prefix) else None
    logProgress(prefixOpt, sizeHint, logLevel, index) >> body <* rewind
  }
  def progressEnd[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], total: Long)(
    implicit F: Monad[F], ambientLevel: LogLevel
  ): F[Unit] = {
    val prefixOpt = if(wrapProgressInnerUsesPrefix) Some(prefix) else None
    logProgress(prefixOpt, sizeHint, logLevel, total)
  }
}
