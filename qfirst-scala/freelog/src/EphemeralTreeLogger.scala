package freelog

import cats.Monad
import cats.Traverse
import cats.implicits._

/** Contract: branch must block as well. */
trait EphemeralTreeLogger[F[_], Msg] extends EphemeralLogger[F, Msg] with TreeLogger[F, Msg] {

  // bar if arg present
  override def logTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], m: Monad[F], ambientLevel: LogLevel
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(None, sizeHint)
    branch(prefix)(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i))(f(a)) <* rewind
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
    branch(prefix)(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i))(f(a, i)) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i), logLevel).as(b)
      }
    )
  }
}
