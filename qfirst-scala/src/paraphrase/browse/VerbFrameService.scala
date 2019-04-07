package qfirst.paraphrase.browse
import qfirst.paraphrase._

import qasrl.labeling.SlotBasedLabel
import qasrl.data.AnswerSpan

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import io.circe.generic.JsonCodec
import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}

@JsonCodec case class ParaphrasingInfo(
  sentenceId: String,
  verbIndex: Int,
  verbFrameset: VerbFrameset,
  frameDistribution: Vector[Double],
  predictions: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])],
  goldParaphrases: VerbParaphraseLabels
)

trait VerbFrameService[F[_]] { self =>
  def getVerbs: F[Map[InflectedForms, Int]]
  def getFrameset(verb: InflectedForms): F[VerbFrameset]
  def getParaphrasingInfo(i: Int): F[ParaphrasingInfo]
  def saveParaphraseAnnotations(
    sentenceId: String, verbIndex: Int, paraphrases: VerbParaphraseLabels
  ): F[VerbParaphraseLabels]
}
