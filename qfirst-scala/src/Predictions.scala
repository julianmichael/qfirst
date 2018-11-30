package qfirst

import io.circe.{Encoder, Decoder}

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.data.QuestionLabel
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
) {
  def withOracleAnswers(goldLabels: Map[String, QuestionLabel], minThreshold: Double) = {
    val goldLabelsBySlots = goldLabels.values.toList.groupBy(_.questionSlots).map {
      case (slots, qLabels) => slots -> qLabels.head
    }
    val newQuestions = questions.map(qPred =>
      if(qPred.questionSlots.wh.toString match { case "who" | "what" => true; case _ => false}) {
        goldLabelsBySlots.get(qPred.questionSlots).fold(qPred)(qPred.withOracleAnswers(_, minThreshold))
      } else qPred
    )
    this.copy(questions = newQuestions)
  }
  def withOracleQuestions(
    goldQuestions: Set[SlotBasedLabel[VerbForm]],
    invalidQuestions: Set[SlotBasedLabel[VerbForm]],
    minThreshold: Double
  ) = {
    val newQuestions = questions.filter(_.questionProb >= minThreshold).map(qPred =>
      if(qPred.questionSlots.wh.toString match { case "who" | "what" => true; case _ => false}) {
        if(goldQuestions.contains(qPred.questionSlots)) { qPred.copy(questionProb = (qPred.questionProb + 99.0) / 100.0) }
        else if(invalidQuestions.contains(qPred.questionSlots)) { qPred.copy(questionProb = qPred.questionProb / 1000.0) }
        else qPred.copy(questionProb = qPred.questionProb / 100.0)
      } else qPred
    )
    this.copy(questions = newQuestions)
  }
}
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
) {
  def withOracleAnswers(qLabel: QuestionLabel, minThreshold: Double) = {
    val goldSpans = qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans)
    this.copy(
      answerSpans = answerSpans.collect { case (span, prob) if prob >= minThreshold =>
        if(goldSpans.contains(span)) { (span, (prob + 99.0) / 100.0) } else (span, prob / 100.0)
      }
    )
  }
}
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
