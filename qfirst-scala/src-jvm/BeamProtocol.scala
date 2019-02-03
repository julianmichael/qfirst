package qfirst

import cats.Show

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

// law: bp.withBestFilter(fs, Some(f)).getAllFilters == List(f)
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
