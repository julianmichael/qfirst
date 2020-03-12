package qfirst.frame

import cats.Order

import io.circe.generic.JsonCodec

import monocle.macros.Lenses

@Lenses @JsonCodec case class QuestionId(
  verbId: VerbId, question: ClausalQuestion
) {
  def frame = question.frame
  def slot = question.slot
}
object QuestionId {
  implicit val questionIdOrder =
    Order.whenEqual(
      Order.by[QuestionId, VerbId](_.verbId),
      Order.by[QuestionId, ClausalQuestion](_.question)
    )
}
