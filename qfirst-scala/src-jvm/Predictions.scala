package qfirst

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

case class SentencePrediction(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbs: List[VerbPrediction]
)
object SentencePrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit val sentencePredictionEncoder: Encoder[SentencePrediction] = {
    import io.circe.generic.semiauto._
    deriveEncoder[SentencePrediction]
  }
  implicit val sentencePredictionDecoder: Decoder[SentencePrediction] = {
    import io.circe.generic.semiauto._
    deriveDecoder[SentencePrediction]
  }
}

case class VerbPrediction(
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  questions: List[QuestionPrediction]
)
object VerbPrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit val verbPredictionEncoder: Encoder[VerbPrediction] = {
    import io.circe.generic.semiauto._
    deriveEncoder[VerbPrediction]
  }
  implicit val verbPredictionDecoder: Decoder[VerbPrediction] = {
    import io.circe.generic.semiauto._
    deriveDecoder[VerbPrediction]
  }
}

case class QuestionPrediction(
  questionSlots: SlotBasedLabel[VerbForm],
  questionProb: Double,
  invalidProb: Double,
  answerSpans: List[(AnswerSpan, Double)]
)
object QuestionPrediction {
  import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}
  import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
  implicit val questionPredictionEncoder: Encoder[QuestionPrediction] = {
    import io.circe.generic.semiauto._
    deriveEncoder[QuestionPrediction]
  }
  implicit val questionPredictionDecoder: Decoder[QuestionPrediction] = {
    import io.circe.generic.semiauto._
    deriveDecoder[QuestionPrediction]
  }
}
