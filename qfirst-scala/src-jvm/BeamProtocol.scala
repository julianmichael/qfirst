package qfirst

import cats.Show

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

// law: bp.withBestFilter(fs, Some(f)).getAllFilters == List(f)
trait BeamProtocol[Beam, Filter, FilterSpace] {
  def getAllFilters(fs: FilterSpace): List[Filter]
  def withBestFilter(fs: FilterSpace, f: Option[Filter]): FilterSpace
  def filterBeam(filter: Filter, verb: VerbPrediction[Beam]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
}
