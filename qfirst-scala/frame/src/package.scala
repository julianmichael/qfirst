package qfirst

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

import jjm.Duad
import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.implicits._

import qasrl.ArgStructure
import qasrl.ArgumentSlot
import qasrl.Frame

import freelog.LogLevel

import scala.collection.immutable.SortedSet
import _root_.cats.Functor

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace

  import freelog.EphemeralTreeLogger
  var loggerUnsafe: EphemeralTreeLogger[cats.effect.IO, String] = null

  type QuestionId = ArgumentId[ClausalQuestion]
  object QuestionId {
    def apply(verbId: VerbId, question: ClausalQuestion) = ArgumentId(verbId, question)
    def unapply(argId: QuestionId): Some[(VerbId, ClausalQuestion)] = Some(argId.verbId -> argId.argument)
  }

  type TemplateQ = (ArgStructure, ArgumentSlot)
  type QAPairs = Map[ClausalQuestion, List[List[ESpan]]]

  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))

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
