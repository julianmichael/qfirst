package freelog

import cats.effect.Sync

trait UtilPlatformExtensions {
  lazy val terminal = org.jline.terminal.TerminalBuilder.terminal()
  def getTerminalWidth[F[_]: Sync]: F[Option[Int]] =
    Sync[F].delay(Option(terminal.getWidth()))
}
