package qfirst

import qfirst.clause.ArgStructure

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

import qasrl.ArgumentSlot
import qasrl.Frame

import freelog.LogLevel
import freelog.ProgressSpec

import scala.collection.immutable.SortedSet

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace
  implicit val progressSpec = ProgressSpec.simple(barLength = 50)

  type TemplateQ = (ArgStructure, ArgumentSlot)
  type QAPairs = Map[ClausalQuestion, List[List[ESpan]]]

  type VerbFeatures[VerbType, A] = Map[
    VerbType, NonMergingMap[VerbId, A]
  ]
  type ArgFeatures[VerbType, A, B] = Map[
    VerbType, NonMergingMap[ArgumentId[A], B]
  ]

  type Instances[VerbType, A] = Map[
    VerbType, Map[String, NonMergingMap[Int, A]]
  ]
  object Instances {
    type Qasrl = Instances[InflectedForms, QAPairs]
    type PropBank = Instances[String, QAPairs]
  }
  type ParaphraseAnnotations = Map[
    // sentence
    String, Map[
      // verb index
      Int, VerbParaphraseLabels
    ]
  ]

  // type PropBank = Instances[String, QAPairs]
  // type PropBankElmo = Instances[String, DenseVector[Float]]
  // type PropBankLabels = Instances[String, String]
  // type Qasrl = Instances[InflectedForms, QAPairs]
  // type QasrlElmo = Instances[InflectedForms, DenseVector[Float]]

  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))
}
