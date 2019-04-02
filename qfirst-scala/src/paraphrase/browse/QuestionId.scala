package qfirst.paraphrase.browse

import cats.Order
import cats.implicits._

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import io.circe.generic.JsonCodec

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
