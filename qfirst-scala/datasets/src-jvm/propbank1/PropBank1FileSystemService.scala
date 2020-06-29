package qfirst.datasets.propbank1

import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.ExecutionContext

import cats.effect.ContextShift
import cats.effect.Sync
import cats.implicits._

import fs2.Stream

class PropBank1FileSystemService(location: Path) {
  private[this] val propBankPropsPath =
    location.resolve(Paths.get("data/prop.txt"))

  def streamSentences[F[_]: Sync](
    implicit ec: ExecutionContext,
    cs: ContextShift[F]
  ): Stream[F, PropBank1Sentence] = {
    fs2.io.file.readAll[F](propBankPropsPath, ec, 4096)
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .through(PropBank1Parsing.streamSentencesFromLines[F])
  }
}
