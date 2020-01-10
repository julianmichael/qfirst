package freelog

// import cats.Applicative
// import cats.Traverse
// import cats.implicits._

trait TreeLogger[F[_], Msg] extends Logger[F, Msg] {
  def branch[A](msg: Msg)(body: F[A]): F[A]

  // def branchTraverse[G[_]: Traverse, A, B](fa: G[A], msg: Msg)(f: A => F[B])(
  //   implicit ap: Applicative[F]
  // ): F[G[B]] =
  //   branch(msg)(fa.traverse(f))
}
