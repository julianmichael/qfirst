package qfirst.frames

import cats.Id
import cats.effect.IO

import io.circe.Json
import io.circe.Encoder

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

import qasrl.data.Dataset
import qasrl.util.DependentMap

object FrameDataWriter {
  val genericInflectedForms = InflectedForms(
    stem = "stem".lowerCase,
    present = "presentSingular3rd".lowerCase,
    presentParticiple = "presentParticiple".lowerCase,
    past = "past".lowerCase,
    pastParticiple = "pastParticiple".lowerCase
  )
  def recapitalizeInflection(s: String): String = s match {
    case "presentsingular3rd" => "presentSingular3rd"
    case "presentparticiple" => "presentParticiple"
    case "pastparticiple" => "pastParticiple"
    case x => x
  }

  case class FrameInfo(
    sentenceId: String,
    verbIndex: Int,
    question: String,
    frame: Frame, // DependentMap[ArgumentSlot.Aux, Id],
    answerSlot: ArgumentSlot)
  object FrameInfo {
    def getSlotLabel(slot: ArgumentSlot): String = slot match {
      case Subj => "subj"
      case Obj => "obj"
      case Prep1 => "prep1"
      case Prep2 => "prep2"
      case Misc => "misc"
      case Adv(wh) => wh.toString
    }
    def getAnswerSlotLabel(slot: ArgumentSlot): String = slot match {
      case Prep1 => "prep1-obj"
      case Prep2 => "prep2-obj"
      case x => getSlotLabel(x)
    }

    def getFrameObj(frame: Frame) = {
      val verbTokens = frame.copy(verbInflectedForms = genericInflectedForms).getVerbStack.map(recapitalizeInflection)
      val (auxSlotValue, verbSlotValue) = verbTokens.tail match {
        case Nil => ("_", verbTokens.toList.mkString(" "))
        case toks => (verbTokens.head, toks.mkString(" "))
      }
      Json.obj(
        (List(
           getSlotLabel(Subj) -> Json.fromString(frame.args.get(Subj).fold("_")(_.placeholder.mkString(" "))),
           getSlotLabel(Obj)  -> Json.fromString(frame.args.get(Obj).fold("_")(_.placeholder.mkString(" "))),
           getSlotLabel(Misc)  -> Json.fromString(frame.args.get(Misc).fold("_")(_.placeholder.mkString(" "))),
           "aux" -> Json.fromString(auxSlotValue),
           "verb" -> Json.fromString(verbSlotValue),
           ) ++ List(Prep1, Prep2).flatMap(prepSlot =>
           List(
             getSlotLabel(prepSlot) -> Json.fromString(
               frame.args.get(prepSlot).fold("_")(_.preposition.toString)
             ),
             (getAnswerSlotLabel(prepSlot)) -> Json.fromString(
               frame.args.get(prepSlot).flatMap(_.objOpt).fold("_")(_.placeholder.mkString(" "))
             )
           )
         )): _*
      )
    }

    implicit val frameInfoEncoder: Encoder[FrameInfo] = new Encoder[FrameInfo] {
      final def apply(info: FrameInfo): Json = Json.obj(
        "sentenceId" -> Json.fromString(info.sentenceId),
        "verbIndex" -> Json.fromInt(info.verbIndex),
        "question" -> Json.fromString(info.question),
        "slots" -> getFrameObj(info.frame),
        "answerSlot" -> Json.fromString(getAnswerSlotLabel(info.answerSlot))
      )
    }
  }
}
