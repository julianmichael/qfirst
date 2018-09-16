package qfirst

sealed trait MetricValue {
  import MetricValue._
  def render: String = this match {
    case MetricDouble(x) => f"$x%.3f"
    case MetricInt(x) => f"$x%d"
    case MetricIntOfTotal(value, total) => f"$value%d (${value.toDouble / total * 100}%.2f%%)"
  }
}
object MetricValue {
  case class MetricDouble(value: Double) extends MetricValue
  case class MetricInt(value: Int) extends MetricValue
  case class MetricIntOfTotal(value: Int, total: Int) extends MetricValue

  def apply(value: Int): MetricValue = MetricInt(value)
  def apply(value: Double): MetricValue = MetricDouble(value)
  def apply(value: Int, total: Int): MetricValue = MetricIntOfTotal(value, total)

  implicit val intHasMetricValueMapTree: HasMapTree[MetricValue, Int] =
    new HasMapTree[MetricValue, Int] {
      def getMapTree(i: Int): MapTree[String, MetricValue] =
        MapTree.Leaf[String](MetricInt(i))
    }

  implicit val intHasMetricValueMapTree: HasMapTree[MetricValue, Double] =
    new HasMapTree[MetricValue, Double] {
      def getMapTree(i: Double): MapTree[String, MetricValue] =
        MapTree.Leaf[String](MetricDouble(i))
    }
}
