package qfirst.conll08

import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Sync

import fs2.Stream

import scala.concurrent.ExecutionContext

class CoNLL08FileSystemService(path: Path) {
  // TODO: change to Blocker after update
  def streamSentences[F[_]: Sync](split: CoNLL08Split)(
    implicit ec: ExecutionContext, cs: ContextShift[F]): Stream[F, CoNLL08Sentence] = {
    val suffix = if(split.isTest) ".GOLD" else ""
    val splitPath = path.resolve(s"data/$split/$split.closed$suffix")
    fs2.io.file.readAll[F](path, ec, 4096)
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .through(CoNLL08Parsing.streamSentencesFromLines(split))
  }
}
