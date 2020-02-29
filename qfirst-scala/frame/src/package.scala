package qfirst

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

import qfirst.clause.ArgStructure
import qasrl.ArgumentSlot
import qasrl.Frame

import freelog.LogLevel
import freelog.ProgressSpec

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace
  implicit val progressSpec = ProgressSpec.simple(barLength = 50)

  type TemplateQ = (ArgStructure, ArgumentSlot)
  type ClausalQ = (Frame, ArgumentSlot)
  type QAPairs = Map[ClausalQ, List[List[ESpan]]]

  type Instances[VerbType, A] = Map[
    VerbType, Map[String, NonMergingMap[Int, A]]
  ]
  object Instances {
    type Qasrl = Instances[InflectedForms, QAPairs]
    type PropBank = Instances[String, QAPairs]
  }

  // type PropBank = Instances[String, QAPairs]
  // type PropBankElmo = Instances[String, DenseVector[Float]]
  // type PropBankLabels = Instances[String, String]
  // type Qasrl = Instances[InflectedForms, QAPairs]
  // type QasrlElmo = Instances[InflectedForms, DenseVector[Float]]

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
