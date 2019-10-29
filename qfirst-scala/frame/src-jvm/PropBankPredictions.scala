package qfirst.frame

import qfirst.model.eval.VerbPrediction

import io.circe.{Encoder, Decoder}

import jjm.ling.en.InflectedForms
import jjm.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class PropBankVerbPrediction[A](
  verbIndex: Int,
  verbLemma: String,
  verbSense: String,
  beam: A
) {
  def toGenericVerbPrediction = VerbPrediction(
    verbIndex, InflectedForms.generic, beam
  )
}

case class PropBankSentencePrediction[A](
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbs: List[PropBankVerbPrediction[A]]
)
object PropBankSentencePrediction {
  implicit def propBankSentencePredictionEncoder[A: Encoder]: Encoder[PropBankSentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[PropBankSentencePrediction[A]]
  }
  implicit def propBankSentencePredictionDecoder[A: Decoder]: Decoder[PropBankSentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[PropBankSentencePrediction[A]]
  }
}
