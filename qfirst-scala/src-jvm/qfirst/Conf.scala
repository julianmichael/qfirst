package qfirst

import cats.Monoid

case class Conf(
  tp: Int = 0,
  tn: Int = 0,
  fp: Int = 0,
  fn: Int = 0
) {

  def numPredicted = tp + fp

  def numGold = tp + fn

  def precision = if(tp + fp > 0) {
    tp.toDouble / (tp + fp)
  } else 0.0

  def recall = if(tp + fn > 0) {
    tp.toDouble / (tp + fn)
  } else 0.0

  def f1 = if((precision + recall) > 0.0) {
    2 * (precision * recall) / (precision + recall)
  } else 0.0

  def allStats: MapTree[String, MetricValue] = MapTree.fromPairs(
    "num gold" -> MetricValue(numGold),
    "num predicted" -> MetricValue(numPredicted),
    "precision" -> MetricValue(precision),
    "recall" -> MetricValue(recall),
    "f1" -> MetricValue(f1)
  )
}
object Conf {
  implicit val confMonoid = new Monoid[Conf] {
    def empty: Conf = Conf()
    def combine(x: Conf, y: Conf): Conf =
      Conf(x.tp + y.tp, x.tn + y.tn, x.fp + y.fp, x.fn + y.fn)
  }
  implicit val confHasMetrics = new HasMetrics[Conf] {
    def getMetrics(conf: Conf) = conf.allStats
  }

  def fromSets[A](
    gold: Set[A],
    pred: Set[A],
    all: Set[A]
  ): Conf = Conf(
    tp = gold.intersect(pred).size,
    tn = (all -- pred -- gold).size,
    fp = (pred -- gold).size,
    fn = (gold -- pred).size
  )

  def fromSets[A](
    gold: Set[A],
    pred: Set[A]
  ): Conf = fromSets(gold, pred, gold ++ pred)
}
