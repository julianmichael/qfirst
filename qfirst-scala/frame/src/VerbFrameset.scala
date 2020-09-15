package qfirst.frame

import qfirst.clause.ClauseResolution
import qfirst.clause.ArgStructure

import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import qasrl.bank.SentenceId

import qasrl._
import qasrl.labeling.SlotBasedLabel

import io.circe.generic.JsonCodec

import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import monocle.macros._

// @JsonCodec case class PropBankVerbClusterModel(
//   verbLemma: String,
//   clusterTree: MergeTree[VerbId],
//   clauseSets: Map[VerbId, Set[ArgStructure]]
//   // TODO add coindexing scores after sentence indexing issue has been fixed
//   // coindexingScoresList: List[(((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double)]
// ) {
//   // val coindexingScores = coindexingScoresList.toMap
//   val numInstances = clusterTree.size
//   import io.circe.syntax._
//   def toJsonStringSafe = "{\"verbLemma\": " + verbLemma.asJson.noSpaces + ", \"clusterTree\": " + clusterTree.toJsonStringSafe + ", \"clauseSets\": " + clauseSets.asJson.noSpaces + "}"
// }
// object PropBankVerbClusterModel

@JsonCodec sealed trait ClusterSplittingCriterion {
  import ClusterSplittingCriterion._
  def getNumber: Option[Int] = this match { case Number(value) => Some(value); case _ => None }
  def getLoss: Option[Double] = this match { case Loss(value) => Some(value); case _ => None }
  def isNumber: Boolean = getNumber.nonEmpty
  def isLoss: Boolean = getLoss.nonEmpty

  def splitTree[A](tree: MergeTree[A]): Vector[MergeTree[A]] = this match {
    case Number(numClusters) => tree.splitToN(numClusters)
    case Loss(maxLoss) => tree.splitWhile(_.loss > maxLoss)
  }
}
object ClusterSplittingCriterion {
  @Lenses @JsonCodec case class Number(value: Int) extends ClusterSplittingCriterion
  @Lenses @JsonCodec case class Loss(value: Double) extends ClusterSplittingCriterion

  val number = GenPrism[ClusterSplittingCriterion, Number].composeIso(
    monocle.Iso[Number, Int](_.value)(Number(_))
  )
  val loss = GenPrism[ClusterSplittingCriterion, Loss].composeIso(
    monocle.Iso[Loss, Double](_.value)(Loss(_))
  )
}

class LazyFramesets(model: VerbClusterModel[InflectedForms, ClausalQuestion], initialCriterion: ClusterSplittingCriterion = ClusterSplittingCriterion.Number(5)) {
  case class FrameInputInfo(
    verbIds: Set[VerbId],
    clauseCounts: Map[ArgStructure, Int])
  object FrameInputInfo {
    implicit val frameInputInfoCommutativeMonoid: CommutativeMonoid[FrameInputInfo] = {
      new CommutativeMonoid[FrameInputInfo] {
        def empty: FrameInputInfo = FrameInputInfo(Set(), Map())
        def combine(x: FrameInputInfo, y: FrameInputInfo) = FrameInputInfo(
          x.verbIds ++ y.verbIds,
          x.clauseCounts |+| y.clauseCounts
        )
      }
    }
  }

  private[this] def getFrames(infos: Vector[FrameInputInfo]) = {
    val infosWithIndex = infos.zipWithIndex
    val questionClusterTrees = model.argumentClusterTree.groupBy(qid =>
      infosWithIndex.find(_._1.verbIds.contains(qid.verbId)).fold(-1)(_._2)
    )
    infos.indices.toList.map { index =>
      val info = infos(index)
      val numInstancesInFrame = info.verbIds.size
      val frameClauses = info.clauseCounts.iterator.map { case (clauseTemplate, count) =>
        FrameClause(clauseTemplate, count.toDouble / numInstancesInFrame)
      }.toList
      VerbFrame(
        verbIds = info.verbIds,
        clauseTemplates = frameClauses,
        questionClusterTree = questionClusterTrees(index),
        probability = numInstancesInFrame.toDouble / model.numVerbInstances
      )
    }
  }

  val clauseSets = model.argumentClusterTree
    .unorderedFoldMap(qid =>
      Map(qid.verbId -> Set(qid.argument.clauseTemplate))
    )

  private[this] def makeFramesets(criterion: ClusterSplittingCriterion) = {
    val aggFrameInfo = (childTree: MergeTree[Set[VerbId]]) => {
      val verbIds = childTree.unorderedFold
      FrameInputInfo(
        verbIds.toSet,
        verbIds.unorderedFoldMap(vid =>
          clauseSets(vid).iterator.map(_ -> 1).toMap
        )
      )
    }
    var aggTree = criterion match {
      case ClusterSplittingCriterion.Number(numClusters) => model.verbClusterTree.cutMapAtN(numClusters, aggFrameInfo)
      case ClusterSplittingCriterion.Loss(maxLoss) => model.verbClusterTree.cutMap(_.loss > maxLoss, aggFrameInfo)
    }
    var resList = List.empty[(Double, VerbFrameset)]
    var nextSize = aggTree.size.toInt - 1
    if(nextSize == model.numVerbInstances - 1) {
      allFramesetsLoaded = true
    }
    while(nextSize >= 0) {
      val (losses, frameInfos) = aggTree.valuesWithLosses.unzip
      val maxLoss = losses.max
      val frames = getFrames(frameInfos.toVector)
      val frameset = VerbFrameset(model.verbType, frames)
      resList = (maxLoss -> frameset) :: resList
      aggTree = aggTree.cutMapAtN(nextSize, _.unorderedFold) // should do exactly one merge
      nextSize = nextSize - 1
    }
    resList
  }

  private[this] def refreshClusters(criterion: ClusterSplittingCriterion) = {
    framesetResolutions = makeFramesets(criterion)
  }

  private[this] var allFramesetsLoaded: Boolean = false
  private[this] var framesetResolutions: List[(Double, VerbFrameset)] = makeFramesets(initialCriterion)

  def getFrameset(criterion: ClusterSplittingCriterion): (Double, VerbFrameset) = {
    criterion match {
      case ClusterSplittingCriterion.Number(numClusters) =>
        getFramesetWithNFrames(numClusters)
      case ClusterSplittingCriterion.Loss(maxLoss) =>
        getFramesetWithMaxLoss(maxLoss)
    }
  }

  def getFramesetWithNFrames(n: Int) = {
    val numFramesetsLoaded = framesetResolutions.size
    if(numFramesetsLoaded < n && !allFramesetsLoaded) {
      refreshClusters(ClusterSplittingCriterion.Number(scala.math.max(n, numFramesetsLoaded) * 2))
    }
    framesetResolutions.lift(n - 1).getOrElse(framesetResolutions.last)
  }
  def getFramesetWithMaxLoss(maxLoss: Double): (Double, VerbFrameset) = {
    framesetResolutions.find(_._1 <= maxLoss).getOrElse {
      refreshClusters(ClusterSplittingCriterion.Loss(maxLoss))
      getFramesetWithMaxLoss(maxLoss)
    }
  }
}


@Lenses @JsonCodec case class ParaphrasingFilter(
  verbCriterion: ClusterSplittingCriterion,
  questionCriterion: ClusterSplittingCriterion,
  minClauseProb: Double,
  minParaphrasingProb: Double
) {

  // assume the question is in the tree
  def getParaphrases(
    frame: VerbFrame,
    question: QuestionId,
    questionClusterTree: MergeTree[QuestionId]
  ): Set[(ArgStructure, ArgumentSlot)] = {
    val structureCounts = questionClusterTree.unorderedFoldMap(qid =>
      Map(qid.argument.template -> 1)
    )
    val total = structureCounts.values.sum
    structureCounts.collect {
      case (clausalQ, count) if (
        frame.clauseTemplates.find(_.args == clausalQ._1).exists(_.probability >= this.minClauseProb) &&
          (count.toDouble / total) >= this.minParaphrasingProb
      ) => clausalQ
    }.toSet
  }

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

@Lenses @JsonCodec case class FrameClause(
  args: ArgStructure,
  probability: Double)
object FrameClause

@Lenses @JsonCodec case class VerbFrame(
  verbIds: Set[VerbId],
  clauseTemplates: List[FrameClause],
  questionClusterTree: MergeTree[ArgumentId[ClausalQuestion]],
  probability: Double) {

  // // TODO shim for current functionality
  // def getParaphrasingScores: Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), (Double, Double)] = {
  //   val argStructureToProb = clauseTemplates.map(ct => ct.args -> ct.probability).toMap
  //   def getParaphrasingScoresForTree(
  //     tree: MergeTree[(ArgStructure, ArgumentSlot)]
  //   ): Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), (Double, Double)] =
  //     tree match {
  //       case MergeTree.Leaf(loss, value) => Map((value -> value) -> (argStructureToProb(value._1) -> 1.0)) // assume 1.0 for same arg
  //       case MergeTree.Merge(rank, loss, left, right) =>
  //         left.values.foldMap(l => right.values.foldMap(r => Map((l -> r) -> (argStructureToProb(r._1) -> loss)))) |+|
  //           getParaphrasingScoresForTree(left) |+| getParaphrasingScoresForTree(right)
  //     }
  //   getParaphrasingScoresForTree(coindexingTree)
  // }
  // def getScoredParaphrases(
  //   structure: (ArgStructure, ArgumentSlot)
  // ): Map[(ArgStructure, ArgumentSlot), (Double, Double)] = {
  //   getParaphrasingScores.collect { case ((`structure`, k2), v) => k2 -> v }
  //   // clauseTemplates.flatMap { frameClause =>
  //   //   val clauseScore = frameClause.probability
  //   //   frameClause.args.args.keys.toList.map { otherSlot =>
  //   //     import scala.language.existentials
  //   //     val otherStructure = frameClause.args -> otherSlot
  //   //     val coindexingScore = coindexingScores.getOrElse(structure -> otherStructure, 0.0)
  //   //     otherStructure -> (clauseScore -> coindexingScore)
  //   //   }
  //   // }.toMap
  // }
}
object VerbFrame

@Lenses @JsonCodec case class VerbFrameset(
  inflectedForms: InflectedForms,
  frames: List[VerbFrame]
)
object VerbFrameset
