package qfirst

import simulacrum._
import scala.language.implicitConversions

@typeclass trait HasMetrics[A] {
  def getMetrics(a: A): MapTree[String, MetricValue]
}
