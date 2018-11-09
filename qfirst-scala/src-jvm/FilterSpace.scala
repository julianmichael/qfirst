package qfirst

case class FilterSpace(
  oneThreshold: Option[OneThresholdFilterSpace] = None,
  twoThreshold: Option[TwoThresholdFilterSpace] = None,
  threeThreshold: Option[ThreeThresholdFilterSpace] = None,
  best: Option[BeamFilter]
) {
  def withBest(filter: BeamFilter): FilterSpace = this.copy(best = Some(filter))
  def allFilters = best.map(List(_)).getOrElse(
    oneThreshold.foldMap(_.allFilters) ++
      twoThreshold.foldMap(_.allFilters) ++
      threeThreshold.foldMap(_.allFilters)
  )
}

case class OneThresholdFilterSpace(thresholds: List[Double] = Nil) {
  def allFilters = thresholds.map(BeamFilter.oneThreshold)
}
case class TwoThresholdFilterSpace(
  qaThresholds: List[Double] = Nil,
  invalidThresholds: List[Double] = Nil
) {
  def allFilters = for {
    qa <- qaThresholds
    i <- invalidThresholds
  } yield BeamFilter.twoThreshold(qa, i)
}

case class ThreeThresholdFilterSpace(
  questionThresholds: List[Double] = Nil,
  spanThresholds: List[Double] = Nil,
  invalidThresholds: List[Double] = Nil
) {
  def allFilters = for {
    q <- questionThresholds
    s <- spanThresholds
    i <- invalidThresholds
    b <- List(true, false)
  } yield BeamFilter.threeThreshold(q, s, i, b)
}
