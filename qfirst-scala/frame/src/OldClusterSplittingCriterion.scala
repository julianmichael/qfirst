package qfirst.frame

import io.circe.generic.JsonCodec

import monocle.macros._

@JsonCodec sealed trait OldClusterSplittingCriterion {
  import OldClusterSplittingCriterion._
  def getNumber: Option[Int] = this match { case Number(value) => Some(value); case _ => None }
  def getLoss: Option[Double] = this match { case Loss(value) => Some(value); case _ => None }
  def isNumber: Boolean = getNumber.nonEmpty
  def isLoss: Boolean = getLoss.nonEmpty

  def splitTree[A](tree: MergeTree[A]): Vector[MergeTree[A]] = this match {
    case Number(numClusters) => tree.splitToN(numClusters)
    case Loss(maxLoss) => tree.splitWhile(_.loss > maxLoss)
  }
}
object OldClusterSplittingCriterion {
  @Lenses @JsonCodec case class Number(value: Int) extends OldClusterSplittingCriterion
  @Lenses @JsonCodec case class Loss(value: Double) extends OldClusterSplittingCriterion

  val number = GenPrism[OldClusterSplittingCriterion, Number].composeIso(
    monocle.Iso[Number, Int](_.value)(Number(_))
  )
  val loss = GenPrism[OldClusterSplittingCriterion, Loss].composeIso(
    monocle.Iso[Loss, Double](_.value)(Loss(_))
  )
}
