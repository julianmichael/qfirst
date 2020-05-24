package qfirst

import qfirst.frame.util.Duad
import qfirst.frame.util.NonMergingMap

import qfirst.clause.ArgStructure

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.implicits._

import qasrl.ArgumentSlot
import qasrl.Frame

import freelog.LogLevel
import freelog.ProgressSpec

import scala.collection.immutable.SortedSet

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace
  implicit val progressSpec = ProgressSpec.simple(barLength = 50)

  type QuestionId = ArgumentId[ClausalQuestion]
  object QuestionId {
    def apply(verbId: VerbId, question: ClausalQuestion) = ArgumentId(verbId, question)
    def unapply(argId: QuestionId): Some[(VerbId, ClausalQuestion)] = Some(argId.verbId -> argId.argument)
  }

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

  implicit class RichAny[A](val x: A) extends AnyVal {
    def <->(y: A)(implicit o: Order[A]): Duad[A] = Duad(x, y)
  }

  object Auxiliaries {
    final val doVerbs = Set("do", "does", "doing", "did", "done").map(_.lowerCase)
    final val beVerbs =
      Set("be", "being", "been", "am", "'m", "is", "'s", "ai", "are", "'re", "was", "were").map(
        _.lowerCase
      )
    val willVerbs = Set("will", "'ll", "wo").map(_.lowerCase)

    val haveVerbs =
      Set("have", "having", "'ve", "has", "had", "'d").map(_.lowerCase)
    val wouldVerbs = Set("would", "'d").map(_.lowerCase)

    val modalVerbs = Set("can", "ca", "could", "may", "might", "must", "shall", "should", "ought")
      .map(_.lowerCase) ++ wouldVerbs

    val auxiliaryVerbs =
      doVerbs ++ beVerbs ++ willVerbs ++ haveVerbs ++ modalVerbs

    val negationWords = Set("no", "not", "n't").map(_.lowerCase)
  }
}
