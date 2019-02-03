package qfirst

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

case class VerbPrediction[A](
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  beam: A
)
object VerbPrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit def verbPredictionEncoder[A: Encoder]: Encoder[VerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[VerbPrediction[A]]
  }
  implicit def verbPredictionDecoder[A: Decoder]: Decoder[VerbPrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[VerbPrediction[A]]
  }
}

case class SentencePrediction[A](
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbs: List[VerbPrediction[A]]
)
object SentencePrediction {
  implicit def sentencePredictionEncoder[A: Encoder]: Encoder[SentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveEncoder[SentencePrediction[A]]
  }
  implicit def sentencePredictionDecoder[A: Decoder]: Decoder[SentencePrediction[A]] = {
    import io.circe.generic.semiauto._
    deriveDecoder[SentencePrediction[A]]
  }
}
