package freelog

// import cats.Applicative
// import cats.Traverse
import cats.implicits._

trait TreeLogger[F[_], Msg] extends Logger[F, Msg] {
  def emitBranch[A](
    msg: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A]

  def branch[A](
    msg: Msg, logLevel: LogLevel)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = {
    if(logLevel >= ambientLevel) emitBranch(msg, logLevel)(body)
    else body
  }

  final def debugBranch[A](
    msg: Msg)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = branch(msg, LogLevel.Debug)(body)

  final def traceBranch[A](
    msg: Msg)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = branch(msg, LogLevel.Trace)(body)

  final def infoBranch[A](
    msg: Msg)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = branch(msg, LogLevel.Info)(body)

  final def warnBranch[A](
    msg: Msg)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = branch(msg, LogLevel.Warn)(body)

  final def errorBranch[A](
    msg: Msg)(
    body: F[A])(
    implicit ambientLevel: LogLevel
  ): F[A] = branch(msg, LogLevel.Error)(body)
}
