package freelog

import cats.effect.Sync

trait UtilPlatformExtensions {
  def getTerminalWidth[F[_]: Sync]: F[Option[Int]] = Sync[F].delay(None)
}
