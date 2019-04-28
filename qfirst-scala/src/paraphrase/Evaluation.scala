package qfirst.paraphrase

import cats.implicits._

import qasrl.ArgumentSlot
import qasrl.data.AnswerSpan
import qasrl.data.VerbEntry
import qasrl.labeling.SlotBasedLabel

import nlpdata.datasets.wiktionary.VerbForm

import qfirst.filterGoldDense
// import qfirst.{Instances => I}
// import qfirst.metrics.{Transformers => M}
import qfirst.metrics._
import shapeless._
import shapeless.syntax.singleton._
import shapeless.record._
import monocle.function.{all => Optics}
import qfirst.ClauseResolution
import qfirst.ClauseResolution.ArgStructure

object Evaluation {
  // def getParaphrasingClauses(
  //   frameset: VerbFrameset,
  //   frameProbabilities: Vector[Double], threshold: Double, marginalize: Boolean
  // ) = {
  //   if(marginalize) {
  //     frameset.frames.zip(frameProbabilities).foldMap { case (frame, prob) =>
  //       frame.clauseTemplates.foldMap(clause =>
  //         Map(clause.args -> (clause.probability * prob))
  //       )
  //     }.filter(_._2 >= threshold).keySet
  //   } else {
  //     val chosenFrame = frameset.frames(frameProbabilities.zipWithIndex.maxBy(_._1)._2)
  //     chosenFrame.clauseTemplates.filter(_.probability >= threshold).map(_.args).toSet
  //   }
  // }

  def getClauseParaphrasingMetric(
    verbFrameset: VerbFrameset,
    predictedParaphrasingClauses: Set[ArgStructure],
    goldParaphrases: VerbParaphraseLabels
  ) = {
    predictedParaphrasingClauses.toList.foldMap(clauseTemplate =>
      if(goldParaphrases.correctClauses.contains(clauseTemplate)) BoundedAcc.correct(clauseTemplate)
      else if(goldParaphrases.incorrectClauses.contains(clauseTemplate)) BoundedAcc.incorrect(clauseTemplate)
      else BoundedAcc.uncertain(clauseTemplate)
    )
  }

  // TODO oracle clause paraphrasing

  def getVerbResults(
    gold: VerbEntry,
    // predictedQAs: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])],
    goldParaphrases: VerbParaphraseLabels,
    verbFrameset: VerbFrameset,
    frameProbabilities: Vector[Double],
    filter: ParaphrasingFilter
  ) = {
    val (goldInvalid, goldValid) = filterGoldDense(gold)
    val goldValidQLabels = goldValid.values.toList
    val goldValidStructures = ClauseResolution.getResolvedStructures(goldValidQLabels.map(_.questionSlots))
    val goldValidStructurePairs = goldValidStructures.zip(goldValidQLabels)
      .groupBy(_._1).map { case (struct, labels) =>
        struct -> labels.unorderedFoldMap(_._2.answerJudgments.flatMap(_.judgment.getAnswer).unorderedFoldMap(_.spans))
    }
    // val predictedQAPairs = predictedQAs.values.toList
    // val predictedStructures = ClauseResolution.getResolvedStructures(predictedQAPairs.map(_._1))
    // val predictedStructurePairs = predictedStructures.zip(predictedQAPairs)
    //   .groupBy(_._1).map { case (struct, pairs) =>
    //     struct -> pairs.foldMap(_._2._2)
    // }

    // val questionWithAnswerBoundedAcc = predictedQAs.values.toList.foldMap { case (predQuestion, predSpans) =>
    //   val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
    //   if(goldInvalid.contains(predQString)) BoundedAcc.incorrect(predQuestion)
    //   else goldValid.get(predQString).fold(BoundedAcc.uncertain(predQuestion)) { goldQLabel =>
    //     val goldSpans = goldQLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
    //     if(goldSpans.intersect(predSpans).nonEmpty) BoundedAcc.correct(predQuestion)
    //     else BoundedAcc.incorrect(predQuestion)
    //   }
    // }
    // val templatedQAAcc = predictedStructurePairs.toList.foldMap { case pred @ (predStruct, predSpans) =>
    //   if(goldValidStructurePairs.get(predStruct).exists(_.exists(predSpans.contains))) Accuracy.correct(pred)
    //   else Accuracy.incorrect(pred)
    // }
    // val templatedQuestionAcc = predictedStructurePairs.keys.toList.foldMap(struct =>
    //   if(goldValidStructurePairs.contains(struct)) Accuracy.correct(struct) else Accuracy.incorrect(struct)
    // )

    // TODO perhaps do something with paraphrasing of incorrect QA pairs as well?
    val predictedParaphrases = goldValidStructures.map(struct =>
      struct -> filter.getParaphrases(verbFrameset, frameProbabilities, struct)
    ).toMap
    val novelParaphrasingClauses = predictedParaphrases.unorderedFold.map(_._1) -- predictedParaphrases.keySet.map(_._1)
    val clauseParaphrasingBoundedAcc = getClauseParaphrasingMetric(
      verbFrameset, novelParaphrasingClauses, goldParaphrases
    )

    val paraphrasingBoundedAcc = goldValidStructures.foldMap { struct =>
      predictedParaphrases(struct).toList.foldMap(predParaphrase =>
        if(goldParaphrases.incorrectClauses.contains(predParaphrase._1)) BoundedAcc.incorrect(struct -> predParaphrase)
        else if(goldParaphrases.paraphrases.equal(struct, predParaphrase)) BoundedAcc.correct(struct -> predParaphrase)
        else if(goldParaphrases.paraphrases.apart(struct, predParaphrase)) BoundedAcc.incorrect(struct -> predParaphrase)
        else BoundedAcc.uncertain(struct -> predParaphrase)
      )
    }
    // "question+answer accuracy" ->> questionWithAnswerBoundedAcc ::
    // "templated qa accuracy" ->> templatedQAAcc ::
    // "templated question accuracy" ->> templatedQuestionAcc ::

    "number of verbs" ->> 1 ::
      "number of questions" ->> goldValidStructures.size ::
      "clause paraphrasing accuracy" ->> clauseParaphrasingBoundedAcc ::
      "question template paraphrasing accuracy (correct QAs)" ->> paraphrasingBoundedAcc ::
      HNil
  }
}
