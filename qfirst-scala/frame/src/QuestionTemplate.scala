package qfirst.frame

import qasrl.labeling.SlotBasedLabel

import jjm.LowerCaseString
import jjm.ling.en.VerbForm
import jjm.ling.en.VerbForm.PastParticiple
import jjm.implicits._

import cats.implicits._

import io.circe.Json

case class QuestionTemplate(
  wh: LowerCaseString,
  hasSubj: Boolean,
  isPassive: Boolean,
  hasObj: Boolean,
  prep: Option[LowerCaseString],
  obj2: Option[LowerCaseString]
) {
  def toTemplateString = List(
    Some(wh),
    Option("something".lowerCase).filter(_ => hasSubj),
    Some(if(isPassive) "verb[pss]" else "verb").map(_.lowerCase),
    Option("something".lowerCase).filter(_ => hasObj),
    prep,
    obj2
  ).flatten.mkString(" ")
}
object QuestionTemplate {
  // extremely unnecessary amounts of computation here, lol
  def fromClausalQuestion(clausalQ: ClausalQuestion) = {
    fromQuestionSlots(
      SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
        Vector(), clausalQ.frame.verbInflectedForms,
        clausalQ.frame.questionsForSlot(clausalQ.slot)
      ).head.get
    )
  }

  def fromQuestionSlots(slots: SlotBasedLabel[VerbForm]) = QuestionTemplate(
    wh = if(slots.wh.toString == "who") "what".lowerCase else slots.wh,
    hasSubj = slots.subj.nonEmpty,
    isPassive = slots.verb == PastParticiple &&
      (slots.aux.toList ++ slots.verbPrefix).map(_.toString).toSet.intersect(
        Set("be", "been", "is", "isn't", "was", "wasn't")
      ).nonEmpty,
    hasObj = slots.obj.nonEmpty,
    prep = slots.prep,
    obj2 = slots.obj2.map(_.toString.replaceAll("someone", "something").lowerCase)
  )

  import io.circe.{Encoder, Decoder}

  implicit val questionTemplateEncoder = implicitly[Encoder[Map[String, String]]]
    .contramap[QuestionTemplate] { questionTemplate =>
      import questionTemplate._
      Map(
        "abst-wh"    -> wh.toString,
        "abst-subj"  -> (if(hasSubj) "something" else "_"),
        "abst-verb"  -> (if(isPassive) "verb[pss]" else "verb"),
        "abst-obj"   -> (if(hasObj) "something" else "_"),
        "prep"       -> prep.fold("_")(_.toString),
        "abst-obj2"  -> obj2.fold("_")(_.toString)
      )
    }

  implicit val questionTemplateDecoder = Decoder.instance[QuestionTemplate] { c =>
    for {
      wh <- c.downField("abst-wh").as[String]
      subj <- c.downField("abst-subj").as[String]
      verb <- c.downField("abst-verb").as[String]
      obj <- c.downField("abst-obj").as[String]
      prep <- c.downField("prep").as[String]
      obj2 <- c.downField("abst-obj2").as[String]
    } yield QuestionTemplate(
      wh = wh.lowerCase,
      hasSubj = subj != "_",
      isPassive = verb == "verb[pss]",
      hasObj = obj != "_",
      prep = prep.lowerCase.some.filter(_ != "_"),
      obj2 = obj2.lowerCase.some.filter(_ != "_")
    )
  }
}
