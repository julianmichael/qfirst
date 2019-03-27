package qfirst.browse

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import qasrl._
import qfirst.ClauseResolution.ArgStructure

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
  probability: Double)
object FrameClause

@Lenses @JsonCodec case class VerbFrame(
  clauseTemplates: List[FrameClause],
  instances: List[SentenceId], // TODO pair sentence ID and verb index
  probability: Double)
object VerbFrame

@Lenses @JsonCodec case class VerbFrameset(
  inflectedForms: InflectedForms,
  frames: List[VerbFrame]
)
object VerbFrameset

trait VerbFrameService[F[_]] { self =>
  def getVerbs: F[Map[InflectedForms, Int]]
  def getFrame(verb: InflectedForms): F[VerbFrameset]
}
