package qfirst

import qfirst.clause.ArgStructure
import qasrl.ArgumentSlot

import freelog.LogLevel

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Info

  def getArgumentSlotsForClauseTemplate(clauseTemplate: ArgStructure): Set[ArgumentSlot] = {
    (clauseTemplate.args.keys.toList: List[ArgumentSlot]).filter {
      case qasrl.Obj2 => clauseTemplate.args.get(qasrl.Obj2) match {
        case Some(qasrl.Prep(_, None)) => false
        case _ => true
      }
      case _ => true
    }.toSet
  }

  type ParaphraseAnnotations = Map[
    // sentence
    String, Map[
      // verb index
      Int, VerbParaphraseLabels
    ]
  ]

  import scala.collection.immutable.SortedSet
  import cats.Order
  import cats.data.NonEmptySet
  import cats.implicits._
  import monocle.Iso

  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))
}
