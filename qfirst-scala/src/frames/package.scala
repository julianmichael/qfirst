package qfirst

import scala.collection.immutable.SortedSet

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

package object frames {
  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))
}
