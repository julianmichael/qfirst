// package qfirst.frame

// import qfirst.clause.ClauseResolution
// import qfirst.clause.ArgStructure

// import cats.Monoid
// import cats.kernel.CommutativeMonoid
// import cats.Order
// import cats.data.NonEmptySet
// import cats.implicits._

// import qasrl.bank.SentenceId

// import qasrl._
// import qasrl.labeling.SlotBasedLabel

// import io.circe.generic.JsonCodec

// import jjm.ling.en.InflectedForms
// import jjm.ling.en.VerbForm

// import monocle.macros._

// class LazyFramesets(model: VerbClusterModel[InflectedForms, ClausalQuestion], initialCriterion: OldClusterSplittingCriterion = OldClusterSplittingCriterion.Number(5)) {
//   case class FrameInputInfo(
//     verbIds: Set[VerbId],
//     clauseCounts: Map[ArgStructure, Int])
//   object FrameInputInfo {
//     implicit val frameInputInfoCommutativeMonoid: CommutativeMonoid[FrameInputInfo] = {
//       new CommutativeMonoid[FrameInputInfo] {
//         def empty: FrameInputInfo = FrameInputInfo(Set(), Map())
//         def combine(x: FrameInputInfo, y: FrameInputInfo) = FrameInputInfo(
//           x.verbIds ++ y.verbIds,
//           x.clauseCounts |+| y.clauseCounts
//         )
//       }
//     }
//   }

//   private[this] def getFrames(infos: Vector[FrameInputInfo]) = {
//     val infosWithIndex = infos.zipWithIndex
//     val questionClusterTrees = model.argumentClusterTreeOpt.map(
//       _.group(qids =>
//         qids.groupBy(qid =>
//           infosWithIndex.find(_._1.verbIds.contains(qid.verbId)).fold(-1)(_._2)
//         )
//       )
//     ).getOrElse(Map())
//     infos.indices.toList.map { index =>
//       val info = infos(index)
//       val numInstancesInFrame = info.verbIds.size
//       val frameClauses = info.clauseCounts.iterator.map { case (clauseTemplate, count) =>
//         FrameClause(clauseTemplate, count.toDouble / numInstancesInFrame)
//       }.toList
//       VerbFrame(
//         verbIds = info.verbIds,
//         clauseTemplates = frameClauses,
//         questionClusterTree = questionClusterTrees(index),
//         probability = numInstancesInFrame.toDouble / model.numVerbInstances
//       )
//     }
//   }

//   val clauseSets = model.argumentClusterTreeOpt.foldMap(
//     _.unorderedFoldMap(qids =>
//       qids.unorderedFoldMap(qid =>
//         Map(qid.verbId -> Set(qid.argument.clauseTemplate))
//       )
//     )
//   )

//   private[this] def makeFramesets(criterion: OldClusterSplittingCriterion) = {
//     val aggFrameInfo = (childTree: MergeTree[Set[VerbId]]) => {
//       val verbIds = childTree.unorderedFold
//       FrameInputInfo(
//         verbIds.toSet,
//         verbIds.unorderedFoldMap(vid =>
//           clauseSets(vid).iterator.map(_ -> 1).toMap
//         )
//       )
//     }
//     var aggTree = criterion match {
//       case OldClusterSplittingCriterion.Number(numClusters) => model.verbClusterTree.cutMapAtN(numClusters, aggFrameInfo)
//       case OldClusterSplittingCriterion.Loss(maxLoss) => model.verbClusterTree.cutMap(_.loss > maxLoss, aggFrameInfo)
//     }
//     var resList = List.empty[(Double, VerbFrameset)]
//     var nextSize = aggTree.size.toInt - 1
//     if(nextSize == model.numVerbInstances - 1) {
//       allFramesetsLoaded = true
//     }
//     while(nextSize >= 0) {
//       val (losses, frameInfos) = aggTree.valuesWithLosses.unzip
//       val maxLoss = losses.max
//       val frames = getFrames(frameInfos.toVector)
//       val frameset = VerbFrameset(model.verbType, frames)
//       resList = (maxLoss -> frameset) :: resList
//       aggTree = aggTree.cutMapAtN(nextSize, _.unorderedFold) // should do exactly one merge
//       nextSize = nextSize - 1
//     }
//     resList
//   }

//   private[this] def refreshClusters(criterion: OldClusterSplittingCriterion) = {
//     framesetResolutions = makeFramesets(criterion)
//   }

//   private[this] var allFramesetsLoaded: Boolean = false
//   private[this] var framesetResolutions: List[(Double, VerbFrameset)] = makeFramesets(initialCriterion)

//   def getFrameset(criterion: OldClusterSplittingCriterion): (Double, VerbFrameset) = {
//     criterion match {
//       case OldClusterSplittingCriterion.Number(numClusters) =>
//         getFramesetWithNFrames(numClusters)
//       case OldClusterSplittingCriterion.Loss(maxLoss) =>
//         getFramesetWithMaxLoss(maxLoss)
//     }
//   }

//   def getFramesetWithNFrames(n: Int) = {
//     val numFramesetsLoaded = framesetResolutions.size
//     if(numFramesetsLoaded < n && !allFramesetsLoaded) {
//       refreshClusters(OldClusterSplittingCriterion.Number(scala.math.max(n, numFramesetsLoaded) * 2))
//     }
//     framesetResolutions.lift(n - 1).getOrElse(framesetResolutions.last)
//   }
//   def getFramesetWithMaxLoss(maxLoss: Double): (Double, VerbFrameset) = {
//     framesetResolutions.find(_._1 <= maxLoss).getOrElse {
//       refreshClusters(OldClusterSplittingCriterion.Loss(maxLoss))
//       getFramesetWithMaxLoss(maxLoss)
//     }
//   }
// }


// @Lenses @JsonCodec case class FrameClause(
//   args: ArgStructure,
//   probability: Double)
// object FrameClause

// @Lenses @JsonCodec case class VerbFrame(
//   verbIds: Set[VerbId],
//   clauseTemplates: List[FrameClause],
//   questionClusterTree: MergeTree[Set[ArgumentId[ClausalQuestion]]],
//   probability: Double) {
// }
// object VerbFrame

// @Lenses @JsonCodec case class VerbFrameset(
//   inflectedForms: InflectedForms,
//   frames: List[VerbFrame]
// )
// object VerbFrameset
