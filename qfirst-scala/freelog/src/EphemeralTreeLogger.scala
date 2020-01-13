package freelog
import freelog.implicits._

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

  // bar if arg present
  override def logTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], m: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(None, sizeHint)
    branch(prefix, logLevel)(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i), logLevel)(f(a)) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i), logLevel).as(b)
      }
    )
  }

  // sometimes bar
  override def logTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], m: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(None, sizeHint)
    branch(prefix, logLevel)(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i), logLevel)(f(a, i)) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i), logLevel).as(b)
      }
    )
  }
}
