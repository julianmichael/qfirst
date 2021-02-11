package qfirst

import qfirst.frame.util.Duad
import qfirst.frame.util.NonMergingMap

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

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

  implicit class RichAny[A](val x: A) extends AnyVal {
    def <->(y: A)(implicit o: Order[A]): Duad[A] = Duad(x, y)
  }

  implicit class RichPair[F[_], A, B](val x: F[(A, B)]) extends AnyVal {
    def mapFirst[C](f: A => C)(implicit F: Functor[F]): F[(C, B)] =
      x.map { case (a, b) => f(a) -> b }
    def mapSecond[C](f: B => C)(implicit F: Functor[F]): F[(A, C)] =
      x.map { case (a, b) => a -> f(b) }
  }

  implicit class RichMap[A, B](val x: Map[A, B]) extends AnyVal {
    def mapVals[C](f: B => C): Map[A, C] = x.transform { case (_, v) => f(v) }
    def zipValues[C](y: Map[A, C]) = {
      val keys = x.keySet.intersect(y.keySet)
      keys.iterator.map(k => k -> (x(k) -> y(k))).toMap
    }
    def normalize(implicit N: Numeric[B]): Map[A, Double] = {
      val total = N.toDouble(x.values.sum)
      mapVals(v => N.toDouble(v) / total)
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
