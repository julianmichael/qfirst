package qfirst

import simulacrum._
import scala.language.implicitConversions

@typeclass trait HasMetrics[A] {
  def getMetrics(a: A): MapTree[String, Metric]
}
object HasMetrics {
  implicit val intHasMetrics: HasMetrics[Int] =
    new HasMetrics[Int] {
      def getMetrics(i: Int): MapTree[String, Metric] =
        MapTree.leaf[String](Metric.int(i))
    }

  implicit val doubleHasMetrics: HasMetrics[Double] =
    new HasMetrics[Double] {
      def getMetrics(i: Double): MapTree[String, Metric] =
        MapTree.leaf[String](Metric.double(i))
    }
}
