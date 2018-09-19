package qfirst

import cats.Show

import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

case class BeamFilter(
  questionThreshold: Double,
  spanThreshold: Double,
  invalidThreshold: Double,
  shouldRemoveSpansBelowInvalidProb: Boolean
) extends (VerbPrediction => Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]) {
  def apply(prediction: VerbPrediction) = {
    def hasOverlap(acc: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])], span: AnswerSpan) = {
      val allAccSpans = acc.toList.flatMap(_._2._2).toSet
      allAccSpans.exists(overlaps(_, span))
    }
    val initAcc = Map.empty[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
    val predQAs = prediction.questions.toList.sortBy(-_.questionProb).foldLeft(initAcc) {
      case (acc, qPrediction) =>
        if(qPrediction.questionProb < questionThreshold) acc
        else if(qPrediction.invalidProb > invalidThreshold) acc
        else {
          val goodSpans = qPrediction.answerSpans.collect {
            case (span, prob) if prob >= spanThreshold &&
                !hasOverlap(acc, span) &&
                (!shouldRemoveSpansBelowInvalidProb || prob >= qPrediction.invalidProb) => span
          }.toSet
          if(goodSpans.isEmpty) acc
          else {
            val qString = qPrediction.questionSlots.renderQuestionString(prediction.verbInflectedForms)
            acc + (qString -> (qPrediction.questionSlots, goodSpans))
          }
        }
    }
    predQAs
  }
}
object BeamFilter {
  implicit def beamFilterShow: Show[BeamFilter] = Show.show { beamFilter =>
    import beamFilter._
    val iSlash = if(shouldRemoveSpansBelowInvalidProb) "/i" else ""
    f"q = $questionThreshold%.2f, s = $spanThreshold%.2f$iSlash%s, i = $invalidThreshold%.2f"
  }
}
