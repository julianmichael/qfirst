package qfirst.conll05

import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Sync
import cats.implicits._

import fs2.Stream

import scala.concurrent.ExecutionContext

class CoNLL05FileSystemService(rootPath: Path) {
  // TODO: change to Blocker after update
  def streamSentences[F[_]: Sync](split: CoNLL05Split)(
    implicit ec: ExecutionContext, cs: ContextShift[F]): Stream[F, CoNLL05Sentence] = {
    def getFilePathsForFeature(feature: String) = {
      val prefix = rootPath.resolve(s"$split/$feature")
      split match {
        case CoNLL05Split.Dev =>
          Stream.emit[F, Path](prefix.resolve(s"devel.24.$feature.gz"))
        case CoNLL05Split.TestWSJ =>
          Stream.emit[F, Path](prefix.resolve(s"test.wsj.$feature.gz"))
        case CoNLL05Split.TestBrown =>
          Stream.emit[F, Path](prefix.resolve(s"test.brown.$feature.gz"))
        case CoNLL05Split.Train =>
          Stream.emits[F, Path](
            (2 to 21).toList.map(sec => // section number in the wsj
              prefix.resolve(f"train.$sec%02d.$feature.gz")
            )
          )
      }
    }
    val predArgSetsBySid = getFilePathsForFeature("props") >>= (filePath =>
      fs2.io.file.readAll[F](filePath, ec, 4096)
        .through(fs2.text.utf8Decode)
        .through(fs2.text.lines)
        .through(PropsParsing.streamPropsFromLines(split))
    )

    // val sensesBySid = getFilePathsForFeature("senses") >>= (filePath =>
    //   fs2.io.file.readAll[F](filePath, ec, 4096)
    //     .through(fs2.text.utf8Decode)
    //     .through(fs2.text.lines)
    //     .through(CoNLL05Parsing.streamSensesFromLines(split))
    // )

    predArgSetsBySid.map { case (sid, pas) =>
      CoNLL05Sentence(sid, pas)
    }
  }
}
