package qfirst.paraphrase

import cats.implicits._

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

  def getVerbResults(
    gold: VerbEntry,
    predictedQAs: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])],
    goldParaphrases: VerbParaphraseLabels,
    verbFrameset: VerbFrameset,
    frameProbabilities: Vector[Double],
    threshold: Double,
    marginalize: Boolean
  ) = {
    val clauseParaphrasingBoundedAcc = getClauseParaphrasingMetric(
      verbFrameset,
      verbFrameset.getParaphrasingClauses(frameProbabilities, threshold, marginalize),
      goldParaphrases
    )
    val (goldInvalid, goldValid) = filterGoldDense(gold)
    val goldValidQLabels = goldValid.values.toList
    val goldValidStructures = ClauseResolution.getResolvedStructures(goldValidQLabels.map(_.questionSlots))
    val goldValidStructurePairs = goldValidStructures.zip(goldValidQLabels)
      .groupBy(_._1).map { case (struct, labels) =>
        struct -> labels.unorderedFoldMap(_._2.answerJudgments.flatMap(_.judgment.getAnswer).unorderedFoldMap(_.spans))
    }
    val predictedQAPairs = predictedQAs.values.toList
    val predictedStructures = ClauseResolution.getResolvedStructures(predictedQAPairs.map(_._1))
    val predictedStructurePairs = predictedStructures.zip(predictedQAPairs)
      .groupBy(_._1).map { case (struct, pairs) =>
        struct -> pairs.foldMap(_._2._2)
    }

    val questionWithAnswerBoundedAcc = predictedQAs.values.toList.foldMap { case (predQuestion, predSpans) =>
      val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
      if(goldInvalid.contains(predQString)) BoundedAcc.incorrect(predQuestion)
      else goldValid.get(predQString).fold(BoundedAcc.uncertain(predQuestion)) { goldQLabel =>
        val goldSpans = goldQLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
        if(goldSpans.intersect(predSpans).nonEmpty) BoundedAcc.correct(predQuestion)
        else BoundedAcc.incorrect(predQuestion)
      }
    }
    val templatedQAAcc = predictedStructurePairs.toList.foldMap { case pred @ (predStruct, predSpans) =>
      if(goldValidStructurePairs.get(predStruct).exists(_.exists(predSpans.contains))) Accuracy.correct(pred)
      else Accuracy.incorrect(pred)
    }
    val templatedQuestionAcc = predictedStructurePairs.keys.toList.foldMap(struct =>
      if(goldValidStructurePairs.contains(struct)) Accuracy.correct(struct) else Accuracy.incorrect(struct)
    )

    // TODO perhaps do something with paraphrasing of incorrect QA pairs as well?
    val predictedParaphrases = verbFrameset.getParaphrases(
      frameProbabilities, templatedQuestionAcc.correct.toSet
    )
    val paraphrasingBoundedAcc = templatedQAAcc.correct.foldMap { case (predStruct, _) =>
      predictedParaphrases(predStruct).toList.foldMap(predParaphrase =>
        if(goldParaphrases.paraphrases.equal(predStruct, predParaphrase)) BoundedAcc.correct(predStruct -> predParaphrase)
        else if(goldParaphrases.paraphrases.apart(predStruct, predParaphrase)) BoundedAcc.incorrect(predStruct -> predParaphrase)
        else BoundedAcc.uncertain(predStruct -> predParaphrase)
      )
    }
    "number of verbs" ->> 1 ::
      "question+answer accuracy" ->> questionWithAnswerBoundedAcc ::
      "templated qa accuracy" ->> templatedQAAcc ::
      "templated question accuracy" ->> templatedQuestionAcc ::
      "clause paraphrasing accuracy" ->> clauseParaphrasingBoundedAcc ::
      "question template paraphrasing accuracy (correct QAs)" ->> paraphrasingBoundedAcc ::
      HNil
  }
}
