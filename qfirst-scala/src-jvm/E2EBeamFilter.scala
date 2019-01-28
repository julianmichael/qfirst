package qfirst

import qfirst.frames.Frame

import qfirst.metrics.HasMetrics
import qfirst.metrics.Metric

import cats.Order
import cats.Show
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

import io.circe.{Encoder, Decoder}
import io.circe.Json
import io.circe.generic.JsonCodec

case class E2EBeamFilterSpace(
  qaOrders: List[E2EBeamFilter.BeamOrder],
  scoreTransforms: List[E2EBeamFilter.ScoreTransform],
  predicates: List[E2EBeamFilter.Predicate],
  best: Option[E2EBeamFilter]
) {
  def withBest(filter: E2EBeamFilter): E2EBeamFilterSpace = this.copy(best = Some(filter))
  def allFilters = best.fold(
    for {
      order <- qaOrders
      transform <- scoreTransforms
      predicate <- predicates
    } yield E2EBeamFilter(order, transform, predicate)
  )(List(_))
}

@JsonCodec case class E2EBeamFilter(
  qaOrder: E2EBeamFilter.BeamOrder,
  scoreTransform: E2EBeamFilter.ScoreTransform,
  predicate: E2EBeamFilter.Predicate
) {
  def apply(verb: E2EVerbPrediction): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    E2EBeamFilter.filterBeam(verb, qaOrder.get, predicate(scoreTransform.apply))
  }
}
object E2EBeamFilter {

  private[this] def hasOverlap(acc: List[E2EQAPrediction], span: AnswerSpan) = {
    acc.map(_.span).toSet.exists(overlaps(span))
  }

  def qaToString(
    verb: E2EVerbPrediction,
    qa: E2EQAPrediction
  ): String = {
    val tan = verb.tans.maxBy(_._2)._1
    val frame = Frame(qa.clause, verb.verbInflectedForms, tan)
    frame.questionsForSlot(qa.answerSlot).head
  }

  private def filterBeam(
    verb: E2EVerbPrediction,
    order: Order[E2EQAPrediction],
    filterPred: E2EQAPrediction => Boolean
  ): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    val allQAs = verb.beam
      .sorted(order.toOrdering)
      .filter(filterPred)
      .foldLeft(List.empty[E2EQAPrediction]) { case (acc, qaPred) =>
        if(!hasOverlap(acc, qaPred.span)) qaPred :: acc
        else acc
      }
    def getQuestionSlots(question: String) =
      SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
        Vector(), verb.verbInflectedForms, List(question)
      ).head.get
    allQAs.map(qa => qaToString(verb, qa) -> qa)
      .groupBy(_._1)
      .map { case (k, vs) => k -> (getQuestionSlots(k) -> vs.map(_._2.span).toSet) }
  }

  @JsonCodec sealed trait Predicate {
    def apply(scoreTransform: Double => Double)(qa: E2EQAPrediction): Boolean
  }
  object Predicate {
    case class ThreeThreshold(
      clauseThreshold: Double,
      spanThreshold: Double,
      answerSlotThreshold: Double
    ) extends Predicate {
      def apply(scoreTransform: Double => Double)(qa: E2EQAPrediction): Boolean = {
        scoreTransform(qa.clauseScore) >= clauseThreshold &&
          scoreTransform(qa.spanScore) >= spanThreshold &&
          scoreTransform(qa.answerSlotScore) >= answerSlotThreshold
      }
    }
    case class OneThreshold(
      threshold: Double
    ) extends Predicate {
      def apply(scoreTransform: Double => Double)(qa: E2EQAPrediction): Boolean = {
        scoreTransform(qa.clauseScore + qa.spanScore + qa.answerSlotScore) >= threshold
      }
    }

    implicit val predicateShow = Show.show[Predicate] {
      case ThreeThreshold(c, s, as) =>
        f"{ clause: $c%.2f; span: $s%.2f; answerSlot: $as%.2f }"
      case OneThreshold(t) =>
        f"{ total: $t%.2f }"
    }

    // implicit val predicateEncoder: Encoder[Predicate] =
    //   Encoder.instance[Predicate] {
    //     case ThreeThreshold(c, s, as) => Json.obj(
    //       "clause" -> c.asJson,
    //       "span" -> s.asJson,
    //       "answerSlot" -> as.asJson
    //     )
    //     case OneThreshold(t) => Json.obj(
    //       "threshold" -> t.asJson
    //     )
    //   }
  }

  sealed trait BeamOrder {
    def get: Order[E2EQAPrediction]
  }
  object BeamOrder {
    sealed trait BasicBeamOrder extends BeamOrder
    case object Clause extends BasicBeamOrder {
      def get = Order.by[E2EQAPrediction, Double](-_.clauseScore)
    }
    case object Span extends BasicBeamOrder {
      def get = Order.by[E2EQAPrediction, Double](-_.spanScore)
    }
    case object AnswerSlot extends BasicBeamOrder {
      def get = Order.by[E2EQAPrediction, Double](-_.answerSlotScore)
    }
    case object Total extends BasicBeamOrder {
      def get = Order.by[E2EQAPrediction, Double](qa => -(qa.clauseScore + qa.spanScore + qa.answerSlotScore))
    }
    case class Composite(head: BasicBeamOrder, tail: BeamOrder) extends BeamOrder {
      def get = Order.whenEqual(head.get, tail.get)
    }

    def beamOrderToString(bo: BeamOrder): String = bo match {
      case Clause => "clause"
      case Span => "span"
      case AnswerSlot => "answerSlot"
      case Total => "total"
      case Composite(h, t) => s"${beamOrderToString(h)},${beamOrderToString(t)}"
    }

    private[this] object OrderMatch {
      val OrderMatchRegex = "([^,]*),(.*)".r
      def unapply(s: String): Option[(String, String)] = s match {
        case OrderMatchRegex(first, rest) => Some(first, rest)
        case _                            => None
      }
    }

    def basicBeamOrderFromString(s: String): Option[BasicBeamOrder] = s match {
      case "clause"     => Some(Clause)
      case "span"       => Some(Span)
      case "answerSlot" => Some(AnswerSlot)
      case "total"      => Some(Total)
      case _ => None
    }

    def beamOrderFromString(s: String): Option[BeamOrder] = s match {
      case OrderMatch(first, rest) => (basicBeamOrderFromString(first), beamOrderFromString(rest)).mapN(Composite(_, _))
      case _ => basicBeamOrderFromString(s)
    }

    implicit val beamOrderShow = Show.show(beamOrderToString)

    implicit val beamOrderEncoder = Encoder.encodeString.contramap(beamOrderToString)
    implicit val beamOrderDecoder = Decoder.decodeString.map(beamOrderFromString).map(_.get)
  }

  sealed trait ScoreTransform {
    def apply(score: Double): Double
  }
  object ScoreTransform {
    case object Exp extends ScoreTransform {
      def apply(score: Double) = math.exp(score)
    }
    case object Sigmoid extends ScoreTransform {
      def apply(score: Double) = 1.0 / (1.0 + math.exp(-score))
    }

    def scoreTransformToString(st: ScoreTransform) = st match {
      case Exp => "exp"
      case Sigmoid => "sigmoid"
    }

    def scoreTransformFromString(s: String): Option[ScoreTransform] = s match {
      case "exp" => Some(Exp) // for product-of-probs
      case "sigmoid" => Some(Sigmoid) // for sum-of-scores
      case _ => None
    }

    implicit val scoreTransformShow = Show.show(scoreTransformToString)

    implicit val scoreTransformEncoder = Encoder.encodeString.contramap(scoreTransformToString)
    implicit val scoreTransformDecoder = Decoder.decodeString.map(scoreTransformFromString).map(_.get)
  }

  implicit val e2eBeamFilterShow: Show[E2EBeamFilter] = Show.show {
    case E2EBeamFilter(qaOrder, scoreTransform, predicate) =>
      s"{ order: ${qaOrder.show}, transform: ${scoreTransform.show}, predicate: ${predicate.show} }"
  }
  implicit val e2eBeamFilterHasMetrics: HasMetrics[E2EBeamFilter] = new HasMetrics[E2EBeamFilter] {
    def getMetrics(filter: E2EBeamFilter): MapTree[String, Metric] = {
      MapTree.leaf[String](Metric.metadata(e2eBeamFilterShow.show(filter)))
    }
  }

}
