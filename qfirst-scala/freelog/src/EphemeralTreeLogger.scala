package freelog

import cats.Monad
import cats.Traverse
import cats.implicits._

/** Contract: branch must block as well. */
trait EphemeralTreeLogger[F[_], Msg] extends EphemeralLogger[F, Msg] with TreeLogger[F, Msg] {

  // bar if arg present
  override def progTraverse[G[_]: Traverse, A, B](fa: G[A], prefix: String, sizeHint: Option[Long])(f: A => F[B])(
    implicit progress: ProgressSpec[Msg], m: Monad[F]
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(prefix, sizeHint)
    block(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i))(f(a)) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i)).as(b)
      }
    )
  }

  // sometimes bar
  override def progTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A], prefix: String, sizeHint: Option[Long])(f: (A, Int) => F[B])(
    implicit progress: ProgressSpec[Msg], m: Monad[F]
  ): F[G[B]] = {
    val renderProgress = progress.renderProgress(prefix, sizeHint)
    block(
      fa.traverseWithIndexAndSizeM((a, i) =>
        branch(renderProgress(i))(f(a, i)) <* rewind
      ).flatMap { case (b, i) =>
          log(renderProgress(i)).as(b)
      }
    )
  }
}
