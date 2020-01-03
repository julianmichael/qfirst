package freelog

trait TreeLogger[Msg, F[_]] extends Logger[Msg, F] {
  def branch[A](msg: Msg)(body: F[A]): F[A]
}
