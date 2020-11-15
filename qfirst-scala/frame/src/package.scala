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

  implicit class RichAny[A](val x: A) extends AnyVal {
    def <->(y: A)(implicit o: Order[A]): Duad[A] = Duad(x, y)
  }

  implicit class RichMap[A, B](val x: Map[A, B]) extends AnyVal {
    def mapVals[C](f: B => C): Map[A, C] = x.transform { case (_, v) => f(v) }
    def zipValues[C](y: Map[A, C]) = {
      val keys = x.keySet.intersect(y.keySet)
      keys.iterator.map(k => k -> (x(k) -> y(k))).toMap
    }
  }

  import cats.Monoid
  import cats.Monad
  import cats.Foldable
  implicit class RichNested[M[_], F[_], A](val x: M[F[A]]) extends AnyVal {
    def flatFoldMapM[B: Monoid](f: A => M[B])(implicit M: Monad[M], F: Foldable[F]): M[B] =
      M.flatMap(x)(_.foldMapM(f))
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

  import io.circe.{KeyEncoder, KeyDecoder}
  val ESpanString = "\\[(-?[0-9]+), (-?[0-9]+)\\)".r
  implicit val eSpanKeyEncoder: KeyEncoder[ESpan] = KeyEncoder.encodeKeyString.contramap[ESpan](_.toString)
  implicit val eSpanKeyDecoder: KeyDecoder[ESpan] = KeyDecoder.instance[ESpan] {
    case ESpanString(beginStr, endStr) => for {
      begin <- scala.util.Try(beginStr.toInt).toOption
      end <- scala.util.Try(endStr.toInt).toOption
    } yield ESpan(begin, end)
    case _ => None
  }
}
