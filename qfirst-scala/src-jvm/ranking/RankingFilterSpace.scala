package qfirst

case class RankingFilterSpace(
  clauseThresholds: List[Double],
  baseSpace: FilterSpace,
  best: Option[RankingBeamFilter]
) {
  def withBest(filter: RankingBeamFilter): RankingFilterSpace = this.copy(best = Some(filter))
  def allFilters = best.map(List(_)).getOrElse(
    baseSpace.allFilters.flatMap(base =>
      clauseThresholds.map(thresh =>
        RankingBeamFilter.oneThreshold(base, thresh)
      )
    )
  )
}
