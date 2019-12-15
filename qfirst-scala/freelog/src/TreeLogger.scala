package freelog

trait TreeLogger[Msg, F[_]] extends Logger[Msg, F] {
  def log(msg: Msg): F[Unit]
  def log[A](msg: Msg, body: F[A]): F[A]
  // TODO: make sure Traverse can work with the normal treelog thing if F is a monad.
  // TODO: can do progress bar type thing?
}
