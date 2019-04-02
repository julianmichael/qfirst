package qfirst.paraphrase.browse
import qfirst.ClauseResolution

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import qasrl._
import qasrl.labeling.SlotBasedLabel
import qfirst.ClauseResolution.ArgStructure

import io.circe.generic.JsonCodec

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}

import monocle.macros._

@Lenses @JsonCodec case class FrameClause(
  args: ArgStructure,
  argMapping: Map[ArgumentSlot, String],
  probability: Double)
object FrameClause

@Lenses @JsonCodec case class VerbFrame(
  clauseTemplates: List[FrameClause],
  probability: Double) {
  // TODO take probabilities as arguments and return them as results
  def getParaphrases(structure: ArgStructure, slot: ArgumentSlot): Set[(ArgStructure, ArgumentSlot)] = {
    clauseTemplates
      .filter(_.args == structure)
      .flatMap(_.argMapping.get(slot))
      .toList.flatMap { sigil =>
        clauseTemplates.flatMap { fc =>
          fc.argMapping.toList.filter(_._2 == sigil).map(_._1).map(slot =>
            fc.args -> slot
          )
        }
      }
      .filter(_ != (structure -> slot))
      .toSet
  }
}
object VerbFrame

@Lenses @JsonCodec case class VerbFrameset(
  inflectedForms: InflectedForms,
  frames: List[VerbFrame]
) {
  // TODO: properly marginalize instead of just taking the max; add probability threshold args
  def getParaphrasingClauses(frameProbabilities: Vector[Double]) = {
    val chosenFrame = frames(frameProbabilities.zipWithIndex.maxBy(_._1)._2)
    chosenFrame.clauseTemplates.map(_.args).toSet
  }
  // TODO: properly marginalize instead of just taking the max; add probability threshold arg
  def getParaphrases(frameProbabilities: Vector[Double], questions: Set[SlotBasedLabel[VerbForm]]) = {
    val questionSlots = questions.toList
    val questionFrameSlotPairs = ClauseResolution.getResolvedFramePairs(inflectedForms, questionSlots)
    val chosenFrame = frames(frameProbabilities.zipWithIndex.maxBy(_._1)._2)
    questionSlots.zip(questionFrameSlotPairs).map { case (qSlots, (frame, slot)) =>
      // TODO add adverbial case
      // TODO fix up animacy
      qSlots -> {
        val paraphrasePairs = chosenFrame.getParaphrases(ClauseResolution.getClauseTemplate(frame), slot)
        val paraphraseQs = paraphrasePairs.map { case (structure, slot) =>
          frame.copy(args = structure.args, isPassive = structure.isPassive).questionsForSlot(slot).head
        }.toSet
        paraphraseQs
      }
    }.toMap
  }
}
object VerbFrameset
