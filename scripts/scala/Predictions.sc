import $ivy.`org.julianmichael::nlpdata:0.2.0`
import $ivy.`org.julianmichael::qasrl:0.1.0`
import $ivy.`io.circe::circe-core:0.9.3`
import $ivy.`io.circe::circe-generic:0.9.3`

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

case class QuestionPrediction(
  questionSlots: SlotBasedLabel[VerbForm],
  questionProb: Double,
  invalidProb: Double,
  answerSpans: List[(AnswerSpan, Double)]
)
object QuestionPrediction {
  import io.circe.{Encoder, Decoder}
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

case class Prediction(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  questions: List[QuestionPrediction]
)

object Prediction {
  import io.circe.{Encoder, Decoder}
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit val PredictionEncoder: Encoder[Prediction] = {
    import io.circe.generic.semiauto._
    deriveEncoder[Prediction]
  }
  implicit val PredictionDecoder: Decoder[Prediction] = {
    import io.circe.generic.semiauto._
    deriveDecoder[Prediction]
  }
}
