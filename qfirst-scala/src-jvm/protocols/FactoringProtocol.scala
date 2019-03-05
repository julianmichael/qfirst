package qfirst.protocols

import qfirst.BeamProtocol
import qfirst.frames.TAN
import qfirst.VerbPrediction

import cats.Show
import cats.implicits._

import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.LowerCaseStrings._

import qasrl.{PresentTense, PastTense, Modal}
import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

object FactoringProtocol {

  private def getTanFromString(s: String): TAN = {
    val tense = s.takeWhile(_ != ' ') match {
      case "past"    => PastTense
      case "present" => PresentTense
      case m         => Modal(m.lowerCase)
    }
    TAN(
      tense = tense,
      isPerfect = s.contains("+pf"),
      isProgressive = s.contains("+prog"),
      isNegated = s.contains("+neg")
    )
  }

  private def makeTanMap(tanStringList: List[(String, Double)]): Map[TAN, Double] = {
    tanStringList.map { case (tan, prob) => getTanFromString(tan) -> prob }.toMap
  }

  case class Beam[A](
    qa_beam: List[A],
    tans: Option[List[(String, Double)]],
    span_tans: Option[List[(AnswerSpan, List[(String, Double)])]],
    animacy: Option[List[(AnswerSpan, Double)]]) {
    val tanMap: Option[Map[TAN, Double]] = tans.map(makeTanMap)
    val spanTanMap: Option[Map[AnswerSpan, Map[TAN, Double]]] = span_tans.map {
      pairList => pairList.map {
        case (span, tanStringList) => span -> makeTanMap(tanStringList)
      }.toMap
    }
    val animacyMap = animacy.map(_.toMap)
  }

  case class Filter[F](
    tanThreshold: Double,
    useSpanTans: Boolean,
    animacyNegativeThreshold: Double,
    animacyPositiveThreshold: Double,
    innerFilter: F)
  object Filter {
    def useSpanTansStr[F](f: Filter[F]) = if(f.useSpanTans) "∧ s -> t " else ""
    implicit def filterShow[F: Show] = Show.show[Filter[F]](f =>
      f"{ ${f.innerFilter.show}; t ≥ ${f.tanThreshold}%.2f ∧ a_n <= ${f.animacyNegativeThreshold}%.2f ∧ a_p ≥ ${f.animacyPositiveThreshold}%.2f ${useSpanTansStr(f)}}"
    )
  }

  case class FilterSpace[FS, F](
    tanThresholds: List[Double],
    useSpanTans: List[Boolean],
    animacyNegativeThresholds: List[Double],
    animacyPositiveThresholds: List[Double],
    innerSpace: FS,
    best: Option[Filter[F]])
}

import FactoringProtocol._

trait FactoringProtocol[A, F, FS] extends BeamProtocol[
  Beam[A], Filter[F], FilterSpace[FS, F]
] {

  def getAllInnerFilters(fs: FS): List[F]

  def getQAs(
    item: A,
    beam: List[A],
    filter: F,
    getSpanTans: Option[AnswerSpan => Set[TAN]],
    getAnimacy: Option[AnswerSpan => Option[Boolean]]
  ): List[(SlotBasedLabel[VerbForm], AnswerSpan)]

  override def getAllFilters(fs: FilterSpace[FS, F]): List[Filter[F]] = {
    fs.best.map(List(_)).getOrElse(
      for {
        t <- fs.tanThresholds
        st <- fs.useSpanTans
        an <- fs.animacyNegativeThresholds
        ap <- fs.animacyPositiveThresholds
        if ap >= an
        f <- getAllInnerFilters(fs.innerSpace)
      } yield Filter(t, st, an, ap, f)
    )
  }

  def withBestFilter(fs: FilterSpace[FS, F], f: Option[Filter[F]]): FilterSpace[FS, F] = {
    fs.copy(best = f)
  }

  def filterBeam(filter: Filter[F], verb: VerbPrediction[Beam[A]]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
    val beam = verb.beam
    val getSpanTans: Option[AnswerSpan => Set[TAN]] = {
      if(filter.useSpanTans) {
        beam.spanTanMap.map { mapping =>
          (s: AnswerSpan) => {
            mapping(s).toList.filter(_._2 >= filter.tanThreshold).map(_._1).toSet
          }
        }
      } else {
        beam.tanMap.map(mapping =>
          (s: AnswerSpan) => {
            mapping.toList.filter(_._2 >= filter.tanThreshold).map(_._1).toSet
          }
        )
      }
    }
    val getAnimacy: Option[AnswerSpan => Option[Boolean]] = {
      beam.animacyMap.map { animacyScores =>
        (s: AnswerSpan) => {
          if(animacyScores(s) >= filter.animacyPositiveThreshold) Some(true)
          else if(animacyScores(s) < filter.animacyNegativeThreshold) Some(false)
          else None
        }
      }
    }
    verb.beam.qa_beam
      .flatMap(item => getQAs(item, verb.beam.qa_beam, filter.innerFilter, getSpanTans, getAnimacy))
      .groupBy(_._1.renderQuestionString(verb.verbInflectedForms))
      .map { case (qString, qaItems) =>
        qString -> (qaItems.head._1 -> qaItems.map(_._2).toSet)
      }
  }
}
