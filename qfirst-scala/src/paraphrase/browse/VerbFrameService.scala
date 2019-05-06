package qfirst.paraphrase.browse
import qfirst.paraphrase._

import qasrl.labeling.SlotBasedLabel
import qasrl.data.AnswerSpan
import qasrl.data.VerbEntry

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import io.circe.generic.JsonCodec
import qasrl.data.JsonCodecs.{qasrlVerbEntryEncoder, qasrlVerbEntryDecoder}
import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}

@JsonCodec case class ParaphrasingInfo(
  sentenceId: String,
  verbIndex: Int,
  verbEntry: VerbEntry,
  verbFrameset: VerbFrameset,
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
