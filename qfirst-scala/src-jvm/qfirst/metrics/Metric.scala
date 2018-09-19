package qfirst.metrics
import qfirst._

sealed trait Metric {
  import Metric._
  def render: String = this match {
    case MetricDouble(x) => f"$x%.3f"
    case MetricInt(x) => f"$x%d"
    case MetricIntOfTotal(value, total) => f"$value%d (${value.toDouble / total * 100}%.2f%%)"
  }
}
object Metric {
  case class MetricDouble(value: Double) extends Metric
  case class MetricInt(value: Int) extends Metric
  case class MetricIntOfTotal(value: Int, total: Int) extends Metric

  def int(value: Int): Metric = MetricInt(value)
  def double(value: Double): Metric = MetricDouble(value)
  def intOfTotal(value: Int, total: Int): Metric = MetricIntOfTotal(value, total)
}
