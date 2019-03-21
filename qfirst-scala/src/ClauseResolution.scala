package qfirst

import qfirst.frames.implicits._

import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl._
import qasrl.data._
import qasrl.labeling._

object ClauseResolution {

  def getFramesWithAnswerSlots(verbInflectedForms: InflectedForms, question: String) = {
    val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
    val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
    val qPrepBigrams = questionTokensIsh
      .sliding(2)
      .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
      .map(_.mkString(" ").lowerCase)
      .toSet
    val stateMachine =
      new TemplateStateMachine(Vector(), verbInflectedForms, Some(qPreps ++ qPrepBigrams))
    val template = new QuestionProcessor(stateMachine)
    val framesWithAnswerSlots = template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) =>
        println("Failed! " + question)
        ???
      case Right(goodStates) => goodStates.toList.collect {
        case QuestionProcessor.CompleteState(_, frame, answerSlot) =>
          frame -> answerSlot
      }.toSet
    }
    framesWithAnswerSlots
  }


  def locallyResolve(framePairSets: List[Set[(Frame, ArgumentSlot)]]) = {
    val framePseudoCounts = framePairSets.foldMap { framePairSet =>
      val frames = framePairSet.map(_._1).toList
      frames.map(_ -> (1.0 / frames.size)).toMap
    }
    framePairSets.map { framePairSet =>
      framePairSet.toList.maximaBy((p: (Frame, ArgumentSlot)) => framePseudoCounts(p._1)).toSet
    }
  }

  def fallbackResolve(slots: SlotBasedLabel[VerbForm], framePairSet: Set[(Frame, ArgumentSlot)]) = {
    classifyAmbiguity(slots, framePairSet) match {
      case "unambiguous" => framePairSet.head
      case "prepositional" => framePairSet.find(_._2 == Obj2).get
      case "where" => framePairSet.find(_._2 == Adv("where".lowerCase)).get
      case "ditransitive" => slots.wh.toString match {
        case "what" => framePairSet.find(_._2 == Obj2).get
        case "who" => framePairSet.find(_._2 == Obj).get
      }
      case "other" => framePairSet.head
    }
  }

  def classifyAmbiguity(slots: SlotBasedLabel[VerbForm], frameSet: Set[(Frame, ArgumentSlot)]) = {
    val isUnambiguous = frameSet.size == 1
    val isPrepositionalObj = (
      slots.obj.isEmpty && slots.prep.nonEmpty && slots.obj2.isEmpty &&
        frameSet.map(_._2) == Set(Obj, Obj2)
    )
    val isWhereAmb = (
      slots.wh == "where".lowerCase && frameSet.map(_._2) == Set(Adv("where".lowerCase), Obj2)
    )
    val isDitransitiveAmb = (
      Set("who", "what").contains(slots.wh.toString) &&
        ((slots.obj.nonEmpty && slots.obj2.isEmpty) || (slots.obj.isEmpty && slots.obj2.nonEmpty)) &&
        frameSet.map(_._2) == Set(Obj, Obj2)
    )
    if(isUnambiguous) "unambiguous"
    else if(isPrepositionalObj) "prepositional"
    else if(isWhereAmb) "where"
    else if(isDitransitiveAmb) "ditransitive"
    else "other"
  }
}
