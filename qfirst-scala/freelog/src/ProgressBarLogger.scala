package freelog
import freelog.implicits._

import cats.Applicative
import cats.Monad
import cats.implicits._

trait ProgressBarLogger[F[_], Msg] extends EphemeralLogger[F, Msg] {
  val F: Monad[F]

  def getLoggableLineLength: F[Option[Int]]

  def emitProgress(
    prefix: Option[Msg],
    sizeHint: Option[Long],
    logLevel: LogLevel,
    current: Long)(
    implicit progress: ProgressSpec[Msg]
  ): F[Unit] = {
    F.flatMap(getLoggableLineLength) { lineLength =>
      val progressInput = ProgressInput(prefix, sizeHint, lineLength)
      val renderProgress = progress.renderProgress(progressInput)
      emit(renderProgress(current), logLevel)
    }
  }
}
