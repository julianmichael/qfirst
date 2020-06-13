package qfirst.frame.eval

import qfirst.metrics.Proportion
import qfirst.metrics.WeightedNumbers

import cats.implicits._

trait ClusterPRMetric {
  def name: String
  def precisionName: String
  def recallName: String
  def apply[A](
    goldClusterSizes: Map[A, Int],
    clusters: Vector[Map[A, Int]]
  ): WeightedPR
}

object ClusterPRMetric {

  val purityCollocation: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "pur-coll"
    override def precisionName: String = "Purity"
    override def recallName: String = "Collocation"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ): WeightedPR = {
      val purity = clusters.filter(_.nonEmpty).foldMap { counts =>
        val primaryLabelCount = counts.values.max
        val total = counts.values.sum
        Proportion.Stats(
          included = primaryLabelCount,
          excluded = total - primaryLabelCount
        )
      }.proportion
      val collocation = goldClusterSizes.toList.foldMap { case (goldLabel, numLabels) =>
        val numInPrimaryCluster = clusters.map(_.getOrElse(goldLabel, 0)).max
        Proportion.Stats(
          included = numInPrimaryCluster,
          excluded = numLabels - numInPrimaryCluster
        )
      }.proportion
      val numItems = goldClusterSizes.unorderedFold
      WeightedPR(
        WeightedNumbers(purity, weight = numItems.toDouble),
        WeightedNumbers(collocation, weight = numItems.toDouble)
      )
    }
  }

  def bCubedPerInstanceByLabel[A](
    goldClusterSizes: Map[A, Int],
    clusters: Vector[Map[A, Int]]
  ): Map[A, WeightedPR] = {
    clusters.foldMap { counts =>
      val predictedClusterSize = counts.unorderedFold
      counts.toList.foldMap { case (goldLabel, countInCluster) =>
        val precision = countInCluster.toDouble / predictedClusterSize
        val recall = countInCluster.toDouble / goldClusterSizes(goldLabel)
        Map(
          goldLabel -> WeightedPR(
            precisions = WeightedNumbers(precision, weight = countInCluster.toDouble),
            recalls = WeightedNumbers(recall, weight = countInCluster.toDouble)
          )
        )
      }
    }
  }

  val bCubedPerInstance: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "b3-instance"
    override def precisionName: String = "B^3 Precision (per instance)"
    override def recallName: String = "B^3 Recall (per instance)"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ) = bCubedPerInstanceByLabel(goldClusterSizes, clusters).values.toList.combineAll
  }

  val bCubedMFS: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "b3-mfs"
    override def precisionName: String = "B^3 Precision (per instance, MFS)"
    override def recallName: String = "B^3 Recall (per instance, MFS)"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ) = {
      val mfs = goldClusterSizes.toList.maxBy(_._2)._1
      bCubedPerInstanceByLabel(goldClusterSizes, clusters)(mfs)
    }
  }

  val bCubedLFS: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "b3-lfs"
    override def precisionName: String = "B^3 Precision (per instance, LFS)"
    override def recallName: String = "B^3 Recall (per instance, LFS)"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ) = {
      val mfs = goldClusterSizes.toList.maxBy(_._2)._1
        (bCubedPerInstanceByLabel(goldClusterSizes, clusters) - mfs).values.toList.combineAll
    }
  }

  val bCubedPerLabel: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "b3-label"
    override def precisionName: String = "B^3 Precision (per label)"
    override def recallName: String = "B^3 Recall (per label)"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ) = bCubedPerInstanceByLabel(goldClusterSizes, clusters).values.toList.foldMap(_.normalize)
  }

  val bCubedPerVerbType: ClusterPRMetric = new ClusterPRMetric {
    override def name: String = "b3-verb"
    override def precisionName: String = "B^3 Precision (per verb type)"
    override def recallName: String = "B^3 Recall (per verb type)"
    override def apply[A](
      goldClusterSizes: Map[A, Int],
      clusters: Vector[Map[A, Int]]
    ) = bCubedPerInstanceByLabel(goldClusterSizes, clusters).values.toList.combineAll.normalize
  }
}
