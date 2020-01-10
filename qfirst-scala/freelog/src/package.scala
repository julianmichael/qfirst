import cats.Monad
import cats.Traverse
import cats.data.StateT
import cats.implicits._

package object freelog {

  implicit class RichTraverse[F[_]: Traverse, A](val fa: F[A]) {
    def traverseWithIndexAndSizeM[G[_], B](f: (A, Int) => G[B])(implicit G: Monad[G]): G[(F[B], Int)] =
      fa.traverse(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b))))
        .run(0).map(_.swap)
  }
}

