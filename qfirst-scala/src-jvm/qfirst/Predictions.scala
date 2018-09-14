package qfirst

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

object Predictions {
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

  case class Prediction(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    questions: List[QuestionPrediction]
  )

  object Prediction {
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
}
