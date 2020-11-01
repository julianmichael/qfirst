package qfirst.frame.browse

import qfirst.frame.ArgumentId
import qfirst.frame.ClausalQuestion
import qfirst.frame.OldClusterSplittingCriterion
import qfirst.frame.MergeTree
// import qfirst.frame.VerbFrame

import qfirst.clause.ArgStructure

import qasrl.ArgumentSlot

import io.circe.generic.JsonCodec

import monocle.macros._

import cats.implicits._

@Lenses @JsonCodec case class ParaphrasingFilter(
  verbCriterion: OldClusterSplittingCriterion,
  questionCriterion: OldClusterSplittingCriterion,
  minClauseProb: Double,
  minParaphrasingProb: Double
) {

  // assume the question is in the tree
  // def getParaphrases(
  //   frame: VerbFrame,
  //   question: ArgumentId[ClausalQuestion],
  //   questionClusterTree: MergeTree[Set[ArgumentId[ClausalQuestion]]]
  // ): Set[(ArgStructure, ArgumentSlot)] = {
  //   val structureCounts = questionClusterTree.unorderedFoldMap(qids =>
  //     qids.unorderedFoldMap(qid =>
  //       Map(qid.argument.template -> 1)
  //     )
  //   )
  //   val total = structureCounts.values.sum
  //   structureCounts.collect {
  //     case (clausalQ, count) if (
  //       frame.clauseTemplates.find(_.args == clausalQ._1).exists(_.probability >= this.minClauseProb) &&
  //         (count.toDouble / total) >= this.minParaphrasingProb
  //     ) => clausalQ
  //   }.toSet
  // }

  // def getParaphrases(
  //   frame: VerbFrame,
  //   question: QuestionId
  // ): Set[(ArgStructure, ArgumentSlot)] = {
  //   val goodClauses = frame.clauseTemplates.collect {
  //     case FrameClause(clauseTemplate, prob) if prob >= minClauseProb => clauseTemplate
  //   }.toSet

  //   frame.coindexingTree.clustersForValue(structure)
  //     .flatMap(_.find(_.loss <= (1.0 - minCoindexingProb)))
  //     .foldMap(_.values.filter(p => goodClauses.contains(p._1)).toSet)
  // }
}
object ParaphrasingFilter {
}

// @JsonCodec sealed trait ParaphrasingFilter {
//   def getParaphrases(
//     frame: VerbFrame,
//     structure: (ArgStructure, ArgumentSlot)
//   ): Set[(ArgStructure, ArgumentSlot)] = {
//     val scoredParaphrases = frame.getScoredParaphrases(structure)
//     val adverbialParaphrases = structure._2 match {
//       case adv @ Adv(_) => filterScoredParaphrases(
//         scoredParaphrases.map { case ((clauseTemplate, _), (clauseProb, _)) => (clauseTemplate -> adv) -> (clauseProb -> 1.0) }
//       )
//       case _ => Set()
//     }
//     (filterScoredParaphrases(scoredParaphrases) ++ adverbialParaphrases).filter(_ != structure)
//   }
//   def filterScoredParaphrases(
//     scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
//   ): Set[(ArgStructure, ArgumentSlot)]
//   def ignoreCoindexing: ParaphrasingFilter
// }

// object ParaphrasingFilter {
//   @Lenses @JsonCodec case class TwoThreshold(
//     clauseThreshold: Double, coindexingThreshold: Double
//   ) extends ParaphrasingFilter {
//     def filterScoredParaphrases(
//       scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
//     ): Set[(ArgStructure, ArgumentSlot)] = {
//       scoredParaphrases.filter(p => p._2._1 >= clauseThreshold && p._2._2 >= coindexingThreshold).keySet
//     }
//     def ignoreCoindexing: TwoThreshold = this.copy(coindexingThreshold = 0.0)
//   }
//   object TwoThreshold

//   @Lenses @JsonCodec case class OneThreshold(
//     threshold: Double
//   ) extends ParaphrasingFilter {
//     def filterScoredParaphrases(
//       scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
//     ): Set[(ArgStructure, ArgumentSlot)] =
//       scoredParaphrases.filter(p => (p._2._1 * p._2._2) >= threshold).keySet
//     def ignoreCoindexing: OneThreshold = this
//   }
//   object OneThreshold

//   val oneThreshold = GenPrism[ParaphrasingFilter, OneThreshold]
//   val twoThreshold = GenPrism[ParaphrasingFilter, TwoThreshold]
// }
