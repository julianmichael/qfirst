package freelog

trait Logger[Msg, F[_]] {
  def log(msg: Msg): F[Unit]
}

