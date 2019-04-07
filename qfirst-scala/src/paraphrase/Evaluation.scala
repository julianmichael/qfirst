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
    val questionWithAnswerBoundedAcc = predictedQAs.values.toList.foldMap { case (predQuestion, predSpans) =>
      val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
      if(goldInvalid.contains(predQString)) BoundedAcc.incorrect(predQuestion)
      else goldValid.get(predQString).fold(BoundedAcc.uncertain(predQuestion)) { goldQLabel =>
        val goldSpans = goldQLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
        if(goldSpans.intersect(predSpans).nonEmpty) BoundedAcc.correct(predQuestion)
        else BoundedAcc.incorrect(predQuestion)
      }
    }
    val predictedParaphrases = verbFrameset.getParaphrases(
      frameProbabilities, questionWithAnswerBoundedAcc.correct.toSet
    )
    val paraphrasingBoundedAcc = questionWithAnswerBoundedAcc.correct.foldMap { predQuestion =>
      val predQString = predQuestion.renderQuestionString(gold.verbInflectedForms)
      predictedParaphrases(predQuestion).toList.foldMap(predParaphrase =>
        goldParaphrases.questionParaphrases.get(predQString).fold(BoundedAcc.uncertain(predQuestion -> predParaphrase))(goldParaphraseLabels =>
          if(goldParaphraseLabels.correct.contains(predParaphrase)) BoundedAcc.correct(predQuestion -> predParaphrase)
          else if(goldParaphraseLabels.incorrect.contains(predParaphrase)) BoundedAcc.incorrect(predQuestion -> predParaphrase)
          else BoundedAcc.uncertain(predQuestion -> predParaphrase)
        )
      )
    }
    "number of verbs" ->> 1 ::
      "question+answer accuracy" ->> questionWithAnswerBoundedAcc ::
      "question paraphrasing accuracy (correct questions)" ->> paraphrasingBoundedAcc ::
      "clause paraphrasing accuracy" ->> clauseParaphrasingBoundedAcc ::
      HNil
  }
}
