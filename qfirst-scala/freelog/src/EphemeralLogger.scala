package freelog

trait EphemeralLogger[Msg, F[_]] extends Logger[Msg, F] {
  /** Create a rewind block, wherein calls to `rewind` will rewind to the current state */
  def block[A](fa: F[A]): F[A]
  /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  def rewind: F[Unit]
  /** Flush the buffer to effect the last call to `rewind` */
  def flush: F[Unit]
  /** Rewind and log */
  def replace(msg: Msg): F[Unit]
}
