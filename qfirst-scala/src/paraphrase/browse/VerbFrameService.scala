package qfirst.paraphrase.browse
import qfirst.paraphrase._

import nlpdata.datasets.wiktionary.InflectedForms

import io.circe.generic.JsonCodec

@JsonCodec case class ParaphrasingInfo(
  sentenceId: String,
  verbIndex: Int,
  verbFrameset: VerbFrameset,
  frameDistribution: Vector[Double],
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
