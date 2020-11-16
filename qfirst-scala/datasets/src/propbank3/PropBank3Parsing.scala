package qfirst.datasets.propbank3

import scala.util.Try

import cats.implicits._

import qfirst.datasets.PredArgStructure
import qfirst.datasets.PropBankPredicate
import qfirst.datasets.propbank1.PropBankArgument
import qfirst.datasets.propbank1.PropBank1Parsing

object PropBank3Parsing {

  import PropBank1Parsing._

  def readPredArgStructure(line: String): PredArgStructure[PropBankPredicate, PropBankArgument] = {
    val fields = line.split(" ").toVector
    val predicateIndex = fields(2).toInt
    // require(fields(3) == "gold") // not always gold. sometimes is some version marker. idk
    // val predLemma = fields(4) // not using for now.
    val predFields = fields(5).split("\\.")
    val predicate = PropBankPredicate(
      lemma = predFields(0),
      sense = predFields(1))
    // fields(6) is "aspects" it seems, doc'd as "no longer used" in latest release
    // I wonder why. guess aspect is hard
    val arguments = fields.drop(7).map(readPropBankArgument(line, _))

    PredArgStructure(predicateIndex, predicate, arguments.toList)
  }

  import fs2.Pipe

  def streamPredArgStructuresFromLines[F[_]]: Pipe[
    F, String, (PropBank3SentenceId, PredArgStructure[PropBankPredicate, PropBankArgument])
  ] = lines => {
    lines.filter(_.trim.nonEmpty).map { line =>
      val pathStr :: sentenceNumStr :: Nil = line.split(" ").toList.take(2)
      val sentenceId = PropBank3SentenceId.parse(pathStr, sentenceNumStr.toInt)
      sentenceId -> readPredArgStructure(line)
    }
  }
}
