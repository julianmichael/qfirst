package freelog
import freelog.implicits._

import cats.Applicative
import cats.Monad
import cats.Traverse
import cats.implicits._

// contract might be subject to change later...?
/** Contract: branch must block as well, even if not emitted. */
trait EphemeralTreeLogger[F[_], Msg] extends EphemeralLogger[F, Msg] with TreeLogger[F, Msg] {

  override def branch[A](
    msg: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = {
    if(logLevel >= ambientLevel) emitBranch(msg, logLevel)(block(body))
    else block(body)
  }

  // utility functions for traversal / folding

  override def wrapProgressOuter[A](
    prefix: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = {
    branch(prefix, logLevel)(body)
  }
  override def wrapProgressInner[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], index: Long)(
    body: F[A])(
    implicit F: Monad[F], progress: ProgressSpec[Msg], ambientLevel: LogLevel
  ): F[A] = {
    val renderProgress = progress.renderProgress(None, sizeHint)
    branch(renderProgress(index), logLevel)(body) <* rewind
  }
  override def progressEnd[A](
    prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long], total: Long)(
    implicit F: Monad[F], progress: ProgressSpec[Msg], ambientLevel: LogLevel
  ): F[Unit] = {
    val renderProgress = progress.renderProgress(Some(prefix), sizeHint)
    log(renderProgress(total), logLevel)
  }
}
