package qfirst

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

case class PropBankVerbPrediction[A](
  verbIndex: Int,
  verbLemma: String,
  verbSense: String,
  beam: A
) {
  def toGenericVerbPrediction = VerbPrediction(
    verbIndex, PropBankVerbPrediction.genericInflectedForms, beam
  )
}
object PropBankVerbPrediction {
  val genericInflectedForms = InflectedForms(
    stem = "stem".lowerCase,
    present = "present".lowerCase,
    presentParticiple = "presentParticiple".lowerCase,
    past = "past".lowerCase,
    pastParticiple = "pastParticiple".lowerCase
  )

  implicit def propBankVerbPredictionEncoder[A: Encoder]: Encoder[PropBankVerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[PropBankVerbPrediction[A]]
  }
  implicit def propBankVerbPredictionDecoder[A: Decoder]: Decoder[PropBankVerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[PropBankVerbPrediction[A]]
  }
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
