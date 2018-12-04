package qfirst

import qfirst.metrics.HasMetrics
import qfirst.metrics.Metric

import cats.Order
import cats.Show
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

sealed trait BeamFilter {
  def apply(
    verb: VerbPrediction,
    extraFilter: BeamFilter.QAPrediction => Boolean = (_ => true)
  ): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
}
object BeamFilter {
  // TODO
  implicit val beamFilterShow: Show[BeamFilter] = Show.show {
    case OneThreshold(thresh) => f"q*s*v ≥ $thresh%.3f"
    case TwoThreshold(qaThresh, iThresh) => f"q*s ≥ $qaThresh%.3f ∧ i ≤ $iThresh%.3f"
    case ThreeThreshold(qThresh, sThresh, iThresh, sGtI) =>
      f"q ≥ $qThresh%.3f ∧ s ≥ $sThresh%.3f ∧ i ≤ $iThresh%.3f" + (if(sGtI) " ∧ s ≥ i" else "")
  }
  implicit val beamFilterHasMetrics: HasMetrics[BeamFilter] = new HasMetrics[BeamFilter] {
    def getMetrics(filter: BeamFilter): MapTree[String, Metric] = {
      MapTree.leaf[String](Metric.metadata(beamFilterShow.show(filter)))
    }
  }
  // MapTree.fromPairs(
  //   "q" -> Metric.double(filter.questionThreshold),
  //   "s" -> Metric.double(filter.spanThreshold),
  //   "i" -> Metric.double(filter.invalidThreshold),
  //   "□(i <= s)" -> Metric.bool(filter.shouldRemoveSpansBelowInvalidProb)
  // )

  // private[this] def hasOverlap(acc: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])], span: AnswerSpan) = {
  //   val allAccSpans = acc.toList.flatMap(_._2._2).toSet
  //   allAccSpans.exists(overlaps(span))
  // }

  private[this] def hasOverlap(acc: List[QAPrediction], span: AnswerSpan) = {
    acc.map(_.answerSpan).toSet.exists(overlaps(span))
  }

  case class QAPrediction(
    questionSlots: SlotBasedLabel[VerbForm],
    answerSpan: AnswerSpan,
    questionProb: Double,
    invalidProb: Double,
    spanProb: Double
  ) {
    def qaProb = questionProb * spanProb
    def validQAProb = (1.0 - invalidProb) * questionProb * qaProb
  }
  private[this] def splitPredictionsBySpan(pred: QuestionPrediction): List[QAPrediction] = {
    pred.answerSpans.map { case (span, spanProb) =>
      QAPrediction(
        pred.questionSlots,
        span,
        pred.questionProb,
        pred.invalidProb,
        spanProb
      )
    }
  }
  private[this] def consolidateQAs(verbInflectedForms: InflectedForms)(qas: List[QAPrediction]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    qas.groupBy(_.questionSlots.renderQuestionString(verbInflectedForms)).map {
      case (qString, qaPreds) => qString -> (qaPreds.head.questionSlots -> qaPreds.map(_.answerSpan).toSet)
    }
  }

  private[this] def filterBeam(
    verb: VerbPrediction,
    order: Order[QAPrediction],
    filterPred: QAPrediction => Boolean
  ): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    verb.questions
      .flatMap(splitPredictionsBySpan)
      .sorted(order.toOrdering)
      .filter(filterPred)
      .foldLeft(List.empty[QAPrediction]) { case (acc, qaPred) =>
        if(!hasOverlap(acc, qaPred.answerSpan)) qaPred :: acc
        else acc
    } <| consolidateQAs(verb.verbInflectedForms)
  }

  private[this] val questionProbOrder = {
    Order.whenEqual(
      Order.by[QAPrediction, Double](-_.questionProb),
      Order.by[QAPrediction, Double](-_.spanProb)
    )
  }

  case class OneThreshold(
    threshold: Double
  ) extends BeamFilter {
    override def apply(
      verb: VerbPrediction,
      extraFilter: QAPrediction => Boolean = _ => true
    ): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      filterBeam(verb, Order.by[QAPrediction, Double](_.validQAProb), (qa => qa.validQAProb >= threshold && extraFilter(qa)))
    }
  }

  case class TwoThreshold(
    qaThreshold: Double,
    invalidThreshold: Double
  ) extends BeamFilter {
    override def apply(
      verb: VerbPrediction,
      extraFilter: QAPrediction => Boolean = _ => true
    ): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      filterBeam(verb, Order.by[QAPrediction, Double](_.qaProb),
                 (p => p.qaProb >= qaThreshold && p.invalidProb < invalidThreshold && extraFilter(p)))
    }
  }

  case class ThreeThreshold(
    questionThreshold: Double,
    spanThreshold: Double,
    invalidThreshold: Double,
    shouldRemoveSpansBelowInvalidProb: Boolean
  ) extends BeamFilter {
    def apply(
      verb: VerbPrediction,
      extraFilter: QAPrediction => Boolean = _ => true
    ) = {
      filterBeam(
        verb, questionProbOrder,
        (p => extraFilter(p) &&
           p.questionProb >= questionThreshold &&
           p.spanProb >= spanThreshold &&
           p.invalidProb < invalidThreshold &&
           (!shouldRemoveSpansBelowInvalidProb || p.spanProb > p.invalidProb))
      )
    }
  }

  def oneThreshold(threshold: Double): BeamFilter = OneThreshold(threshold)
  def twoThreshold(qaThreshold: Double, invalidThreshold: Double): BeamFilter = TwoThreshold(qaThreshold, invalidThreshold)
  def threeThreshold(questionThreshold: Double, spanThreshold: Double, invalidThreshold: Double, shouldRemoveSpansBelowInvalidProb: Boolean): BeamFilter = ThreeThreshold(questionThreshold, spanThreshold, invalidThreshold, shouldRemoveSpansBelowInvalidProb)
}

