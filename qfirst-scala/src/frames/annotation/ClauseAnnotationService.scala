package qfirst.frames.annotation

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import qfirst.frames.ArgumentSlot
import qfirst.frames.Frame

import io.circe.generic.JsonCodec

@JsonCodec case class ClauseChoice(
  frame: Frame,
  argumentSlot: ArgumentSlot)
object ClauseChoice {
  def make(p: (Frame, ArgumentSlot)) = ClauseChoice(p._1, p._2)
}

@JsonCodec case class ClauseAmbiguity(
  sentenceId: SentenceId,
  verbIndex: Int,
  questionString: String,
  structures: Set[ClauseChoice])

@JsonCodec case class ClauseResolution(
  ambiguity: ClauseAmbiguity,
  choiceOpt: Option[ClauseChoice]
)

trait ClauseAnnotationService[F[_]] {
  def getResolution(isFull: Boolean, index: Int): F[ClauseResolution]
  def saveResolution(isFull: Boolean, index: Int, choice: ClauseChoice): F[Option[ClauseChoice]]
}
