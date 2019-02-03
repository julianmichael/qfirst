package qfirst

import cats.Show

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

case class VerbPrediction[A](
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  beam: A
)
object VerbPrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit def verbPredictionEncoder[A: Encoder]: Encoder[VerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[VerbPrediction[A]]
  }
  implicit def verbPredictionDecoder[A: Decoder]: Decoder[VerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[VerbPrediction[A]]
  }
}

case class SentencePrediction[A](
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbs: List[VerbPrediction[A]]
)
object SentencePrediction {
  implicit def sentencePredictionEncoder[A: Encoder]: Encoder[SentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[SentencePrediction[A]]
  }
  implicit def sentencePredictionDecoder[A: Decoder]: Decoder[SentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[SentencePrediction[A]]
  }
}

// case class QABeamItem(
//   questionSlots: SlotBasedLabel[VerbForm],
//   questionProb: Double,
//   span: AnswerSpan,
//   spanProb: Double
// )

// law: bp.setBest(fs, Some(f)).getAllFilters == List(f)
trait BeamProtocol {
  type Beam
  type Filter
  type FilterSpace
  def getAllFilters(fs: FilterSpace): List[Filter]
  def withBestFilter(fs: FilterSpace, f: Option[Filter]): FilterSpace
  def filterBeam(filter: Filter, beam: Beam): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
  val codecs: BeamProtocol.Codecs[Beam, Filter, FilterSpace]
}
object BeamProtocol {
  type Aux[B, F, FS] = BeamProtocol { type Beam = B; type Filter = F; type FilterSpace = FS }
  trait Codecs[Beam, Filter, FilterSpace] {
    implicit val filterShow: Show[Filter]
    implicit val beamDecoder: Decoder[Beam]
    implicit val beamEncoder: Encoder[Beam]
    implicit val filterSpaceDecoder: Decoder[FilterSpace]
    implicit val filterSpaceEncoder: Encoder[FilterSpace]
  }
}
