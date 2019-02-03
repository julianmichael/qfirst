package qfirst.protocols
import qfirst.BeamProtocol

import cats.Show

import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

object SimpleQAProtocol extends BeamProtocol {
  // TODO switch naming convention
  case class BeamItem(
    question_slots: SlotBasedLabel[VerbForm],
    question_prob: Double,
    span: AnswerSpan,
    span_prob: Double)
  type Beam = List[BeamItem]

  case class Filter(
    questionThreshold: Double,
    spanThreshold: Double)

  case class FilterSpace(
    questionThresholds: List[Double],
    spanThresholds: List[Double],
    best: Option[Filter])

  def getAllFilters(fs: FilterSpace): List[Filter] = {
    fs.best.fold(
      for {
        q <- fs.questionThresholds
        s <- fs.spanThresholds
      } yield Filter(q, s)
    )(List(_))
  }
  def withBestFilter(fs: FilterSpace, f: Option[Filter]): FilterSpace = {
    fs.copy(best = f)
  }
  def filterBeam(filter: Filter, beam: Beam): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    val validItems = beam.filter(item =>
      item.question_prob >= filter.questionThreshold &&
        item.span_prob >= filter.spanThreshold
    )
    ???
  }
  val codecs = new BeamProtocol.Codecs[
    Beam, Filter, FilterSpace] {
    import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
    import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}
    import io.circe.generic.semiauto._
    implicit val beamItemDecoder = deriveDecoder[BeamItem]
    implicit val beamItemEncoder = deriveEncoder[BeamItem]
    implicit val filterDecoder = deriveDecoder[Filter]
    implicit val filterEncoder = deriveEncoder[Filter]

    implicit val filterShow = Show.show[Filter](f =>
      f"{ q ≥ ${f.questionThreshold}%.2f ∧ s ≥ ${f.spanThreshold}%.2f }"
    )
    import io.circe.{Encoder, Decoder}
    implicit val beamDecoder: Decoder[Beam] = deriveDecoder[List[BeamItem]]
    implicit val beamEncoder: Encoder[Beam] = deriveEncoder[List[BeamItem]]
    implicit val filterSpaceDecoder: Decoder[FilterSpace] = deriveDecoder[FilterSpace]
    implicit val filterSpaceEncoder: Encoder[FilterSpace] = deriveEncoder[FilterSpace]
  }
}
