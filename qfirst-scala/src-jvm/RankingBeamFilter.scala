package qfirst

import qfirst.frames._
import qfirst.metrics.HasMetrics
import qfirst.metrics.Metric
import BeamFilter.QAPrediction

import cats.Order
import cats.Show
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

sealed trait RankingBeamFilter extends ((VerbPrediction, ClauseInstance) => Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])])
object RankingBeamFilter {
  implicit val rankingBeamFilterShow: Show[RankingBeamFilter] = Show.show {
    case OneThreshold(base, threshold) => f"c ≥ $threshold%.2f ∧ " + base.show
  }
  implicit val rankingBeamFilterHasMetrics: HasMetrics[RankingBeamFilter] = new HasMetrics[RankingBeamFilter] {
    def getMetrics(filter: RankingBeamFilter): MapTree[String, Metric] = {
      MapTree.leaf[String](Metric.metadata("TODO"))
    }
  }

  private[this] def drawQAs(
    verbInflectedForms: InflectedForms,
    clauses: List[(ClauseStructure, Double)],
    threshold: Double,
    usedSpans: Set[AnswerSpan]
  ): List[(String, (SlotBasedLabel[VerbForm], AnswerSpan))] = {
    clauses.sortBy(-_._2) match {
      case (ClauseStructure(struct, tan, args), prob) :: rest if prob >= threshold =>
        val (qas, usedArgPairs) = args.toList.flatMap { case (slot, span) =>
          if(usedSpans.exists(overlaps(span))) None else {
            for {
              qSlots <- QuestionSlotsGetter.getSlots(ClausalQuestion(struct, tan, slot)).toOption
              frame = Frame(struct, verbInflectedForms, tan)
              qString <- frame.questionsForSlot(slot).headOption
            } yield (qString -> (qSlots -> span), slot -> span)
          }
        }.unzip
        if(usedArgPairs.nonEmpty) {
          val revisedRest = rest.map {
            // find any other possible structures that include the current one...
            case (cs @ ClauseStructure(`struct`, `tan`, args2), prob2) if usedArgPairs.forall(args2.toList.contains) =>
              // and adjust their probabilities to condition on the current one, assuming consistency
              cs -> (prob2 / prob)
            case x => x
          }
          val newSpans = qas.map(_._2._2).toSet
          qas ++ drawQAs(verbInflectedForms, revisedRest, threshold, usedSpans ++ newSpans)
        } else drawQAs(verbInflectedForms, rest, threshold, usedSpans)
      case _ => Nil
    }
  }

  case class OneThreshold(
    baseFilter: BeamFilter,
    clauseThreshold: Double
  ) extends RankingBeamFilter {
    def apply(verb: VerbPrediction, ranking: ClauseInstance): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      val coreQAs = drawQAs(ranking.verbInflectedForms, ranking.clauses, clauseThreshold, Set.empty[AnswerSpan])
      val coreSpans = coreQAs.map(_._2._2).toSet
      val extraFilter = (p: QAPrediction) => !coreSpans.exists(overlaps(p.answerSpan))
      val remainingQAs = baseFilter(verb, extraFilter)

      (coreQAs.map(t => t.copy(_2 = t._2.copy(_2 = Set(t._2._2)))) ++ remainingQAs).groupBy(_._1).map { case (qString, items) =>
        qString -> (items.head._2._1, items.foldMap(_._2._2))
      }
    }
  }

  def oneThreshold(base: BeamFilter, threshold: Double): RankingBeamFilter = OneThreshold(base, threshold)
}
