package qfirst

import qfirst.frames.ArgumentSlot
import qfirst.frames.TAN
import qfirst.frames.ArgStructure
import qfirst.frames.ClausalQuestion

import io.circe.{Encoder, Decoder}
import io.circe.HCursor

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.LowerCaseStrings._

import qasrl.data.AnswerSpan
import qasrl.data.QuestionLabel
import qasrl.labeling.SlotBasedLabel

case class E2ESentencePrediction(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbs: List[E2EVerbPrediction]
)
object E2ESentencePrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  // implicit val e2eSentencePredictionEncoder: Encoder[E2ESentencePrediction] = {
  //   import io.circe.generic.semiauto._
  //   deriveEncoder[E2ESentencePrediction]
  // }
  def decoder(getArgStructureFromString: Map[String, ArgStructure]): Decoder[E2ESentencePrediction] = {
    implicit val innerDecoder: Decoder[E2EVerbPrediction] = E2EVerbPrediction.decoder(getArgStructureFromString)
    import io.circe.generic.semiauto._
    deriveDecoder[E2ESentencePrediction]
  }
}

case class E2EVerbPrediction(
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  animacies: List[(AnswerSpan, Double)],
  tans: List[(TAN, Double)],
  beam: List[E2EQAPrediction]
)

object E2EVerbPrediction {
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  // import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}
  // implicit val verbPredictionEncoder: Encoder[E2EVerbPrediction] = {
  //   import io.circe.generic.semiauto._
  //   deriveEncoder[E2EVerbPrediction]
  // }
  def getTenseFromString(s: String) = {
    import qasrl.{Tense, PastTense, PresentTense, Modal}
    s match {
      case "past"    => PastTense
      case "present" => PresentTense
      case m         => Modal(m.lowerCase)
    }
  }

  def getTANFromString(s: String): TAN = TAN(
    tense = getTenseFromString(s.takeWhile(_ != ' ')),
    isPerfect = s.contains("+pf"),
    isProgressive = s.contains("+prog"),
    isNegated = s.contains("+neg")
  )
  implicit val spanDecoder: Decoder[AnswerSpan] = new Decoder[AnswerSpan] {
    final def apply(c: HCursor): Decoder.Result[AnswerSpan] =
      for {
        begin <- c.downN(0).as[Int].right
        end   <- c.downN(1).as[Int].right
      } yield AnswerSpan(begin, end + 1)
  }

  implicit val tanDecoder: Decoder[TAN] = Decoder.decodeString.map(getTANFromString)

  def decoder(getArgStructureFromString: Map[String, ArgStructure]): Decoder[E2EVerbPrediction] = {
    implicit val innerDecoder: Decoder[E2EQAPrediction] = E2EQAPrediction.decoder(getArgStructureFromString)
    import io.circe.generic.semiauto._
    deriveDecoder[E2EVerbPrediction]
  }
}

case class E2EQAPrediction(
  clause: ArgStructure,
  clauseScore: Double,
  span: AnswerSpan,
  spanScore: Double,
  answerSlotScores: Map[ArgumentSlot, Double],
) {
  val (answerSlot, answerSlotScore) = answerSlotScores.maxBy(_._2)
  def totalScore = clauseScore + spanScore + answerSlotScore
}
object E2EQAPrediction {
  def decoder(getArgStructureFromString: Map[String, ArgStructure]): Decoder[E2EQAPrediction] = {
    RawE2EQAPrediction.rawE2EQAPredictionDecoder.map { raw =>
      val argStructure = getArgStructureFromString(raw.clause)
      val answerSlotScores = raw.all_qargs.map { case (qargString, qargScore) =>
        (ClausalQuestion.getAnswerSlotFromString(qargString.lowerCase): ArgumentSlot) -> qargScore
      }.toMap

      E2EQAPrediction(
        argStructure, raw.clause_score,
        AnswerSpan(raw.span._1, raw.span._2 + 1), raw.span_score,
        answerSlotScores
      )
    }
  }
}

case class RawE2EQAPrediction(
  clause: String,
  clause_score: Double,
  span: (Int, Int),
  span_score: Double,
  qarg: String,
  qarg_score: Double,
  total_prob: Double,
  all_qargs: List[(String, Double)]
)
object RawE2EQAPrediction {
  implicit val rawE2EQAPredictionEncoder: Encoder[RawE2EQAPrediction] = {
    import io.circe.generic.semiauto._
    deriveEncoder[RawE2EQAPrediction]
  }
  implicit val rawE2EQAPredictionDecoder: Decoder[RawE2EQAPrediction] = {
    import io.circe.generic.semiauto._
    deriveDecoder[RawE2EQAPrediction]
  }
}
