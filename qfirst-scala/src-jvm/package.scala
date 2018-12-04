import qasrl.data.AnswerSpan
import qasrl.data.VerbEntry

import cats.implicits._

package object qfirst extends Implicits {

  def filterGold(minNumAnswers: Int, maxNumInvalid: Int) = (verb: VerbEntry) => {
    val (invalids, valids) = verb.questionLabels.toList.flatMap {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        val numAnswers = judgments.size
        if(numAnswers >= minNumAnswers) {
          if(numInvalid <= maxNumInvalid) Some(Right(questionString -> qLabel))
          else Some(Left(questionString -> qLabel))
        } else None
    }.separate
    invalids.toMap -> valids.toMap
  }

  val filterGoldNonDense = filterGold(3, 0)
  val filterGoldDense = filterGold(6, 1)

  def overlaps(x: AnswerSpan)(y: AnswerSpan): Boolean = {
    x.begin <= y.end && y.begin <= x.end
  }

  import cats.Foldable
  import cats.implicits._

  def counts[F[_]: Foldable, A](fa: F[A]): Map[A, Int] = fa.foldMap(a => Map(a -> 1))
}
