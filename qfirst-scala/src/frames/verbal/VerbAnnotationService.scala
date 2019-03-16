package qfirst.frames.verbal

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import qfirst.frames.ArgumentSlot
import qfirst.frames.ArgStructure
import qfirst.frames.Frame
import qfirst.frames.implicits._

import io.circe.generic.JsonCodec

import nlpdata.datasets.wiktionary.InflectedForms

import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}

import monocle.macros._

@Lenses @JsonCodec case class QuestionId(
  sentenceId: SentenceId,
  verbIndex: Int,
  questionString: String)
object QuestionId {
  implicit val questionIdOrder =
    Order.whenEqual(
      Order.by[QuestionId, SentenceId](_.sentenceId),
      Order.whenEqual(
        Order.by[QuestionId, Int](_.verbIndex),
        Order.by[QuestionId, String](_.questionString)
      )
    )
}

@Lenses @JsonCodec case class FrameClause(
  args: ArgStructure,
  argMapping: Map[ArgumentSlot, String],
  instances: Map[ArgumentSlot, NonEmptySet[QuestionId]])
object FrameClause


@Lenses @JsonCodec case class VerbFrame(
  inflectedForms: InflectedForms,
  clauseSets: List[List[FrameClause]]
)
object VerbFrame

trait VerbAnnotationService[F[_]] { self =>
  def getVerbs: F[Map[InflectedForms, Int]]
  def getFrame(verb: InflectedForms): F[VerbFrame]
  def saveFrame(frame: VerbFrame): F[VerbFrame]
}
