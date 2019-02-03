package qfirst.protocols
import qfirst.BeamProtocol
import qfirst.VerbPrediction

import cats.Show
import cats.implicits._

import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

object SimpleQAs {
  // TODO switch naming convention
  case class BeamItem(
    question_slots: SlotBasedLabel[VerbForm],
    question_prob: Double,
    span: AnswerSpan,
    span_prob: Double)

  case class Filter(
    questionThreshold: Double,
    spanThreshold: Double)
  object Filter {
    implicit val filterShow = Show.show[Filter](f =>
      f"{ q ≥ ${f.questionThreshold}%.2f ∧ s ≥ ${f.spanThreshold}%.2f }"
    )
  }

  case class FilterSpace(
    questionThresholds: List[Double],
    spanThresholds: List[Double],
    best: Option[Filter])

  val protocol = new BeamProtocol[List[BeamItem], Filter, FilterSpace] {
    def getAllFilters(fs: FilterSpace): List[Filter] = {
      fs.best.fold(
        for {
          q <- fs.questionThresholds
          s <- fs.spanThresholds
        } yield Filter(q, s)
      )(List(_))
    }
    def withBestFilter(fs: FilterSpace, f: Option[Filter]): FilterSpace = {
      fs.copy(best = f)
    }
    def filterBeam(filter: Filter, verb: VerbPrediction[List[BeamItem]]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      verb.beam
        .filter(item =>
          // TODO remove exp when predictor output is fixed
          math.exp(item.question_prob) >= filter.questionThreshold &&
            item.span_prob >= filter.spanThreshold)
        .groupBy(_.question_slots.renderQuestionString(verb.verbInflectedForms))
        .map { case (qString, qaItems) =>
          qString -> (qaItems.head.question_slots -> qaItems.map(_.span).toSet)
        }
    }
    // use this to mimic the old A-first decoding method
    private[this] def filterBeamAfirstOld(filter: Filter, verb: VerbPrediction[List[BeamItem]]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      def overlaps(x: AnswerSpan)(y: AnswerSpan): Boolean = x.begin < y.end && y.begin < x.end
      verb.beam.groupBy(i => i.span -> i.span_prob)
        .filter(_._1._2 >= filter.spanThreshold)
        .toList.sortBy(-_._1._2)
        .foldLeft(List.empty[(String, (SlotBasedLabel[VerbForm], AnswerSpan))]) {
          case (acc, ((span, spanProb), items)) =>
            if(!acc.map(_._2._2).exists(overlaps(span))) {
              val qSlots = items.maxBy(_.question_prob).question_slots
              val qString = qSlots.renderQuestionString(verb.verbInflectedForms)
              (qString -> (qSlots -> span)) :: acc
            } else acc
        }.groupBy(_._1)
        .map { case (qString, tuples) =>
          qString -> (tuples.head._2._1 -> tuples.map(_._2._2).toSet)
        }
    }
    // use this to mimic the old Q-first decoding method
    private[this] def filterBeamQfirstOld(filter: Filter, verb: VerbPrediction[List[BeamItem]]): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
      def overlaps(x: AnswerSpan)(y: AnswerSpan): Boolean = x.begin < y.end && y.begin < x.end
      verb.beam.groupBy(i => i.question_slots -> i.question_prob)
        .filter(p => math.exp(p._1._2) >= filter.questionThreshold) // TODO remove exp when fixing thing
        .toList.sortBy(-_._1._2)
        .foldLeft(List.empty[(String, (SlotBasedLabel[VerbForm], Set[AnswerSpan]))]) {
          case (acc, ((qSlots, qProb), items)) =>
            val answers = items
              .filter(_.span_prob >= filter.spanThreshold)
              .map(_.span)
              .foldLeft(Set.empty[AnswerSpan]) { (spanSet, span) =>
                if(!spanSet.exists(overlaps(span)) && !acc.flatMap(_._2._2).toSet.exists(overlaps(span))) {
                  spanSet + span
                } else spanSet
              }
            if(answers.isEmpty) acc else {
              val qString = qSlots.renderQuestionString(verb.verbInflectedForms)
              (qString -> (qSlots -> answers)) :: acc
            }
        }.toMap
    }
  }
}


// package qfirst.protocols
// import qfirst.BeamProtocol

// import cats.Show

// import nlpdata.datasets.wiktionary.VerbForm

// import qasrl.data.AnswerSpan
// import qasrl.labeling.SlotBasedLabel

// object SimpleQAProtocol extends BeamProtocol {
//   // TODO switch naming convention
//   case class BeamItem(
//     question_slots: SlotBasedLabel[VerbForm],
//     question_prob: Double,
//     span: AnswerSpan,
//     span_prob: Double)
//   type Beam = List[BeamItem]

//   case class Filter(
//     questionThreshold: Double,
//     spanThreshold: Double)

//   case class FilterSpace(
//     questionThresholds: List[Double],
//     spanThresholds: List[Double],
//     best: Option[Filter])

//   def getAllFilters(fs: FilterSpace): List[Filter] = {
//     fs.best.fold(
//       for {
//         q <- fs.questionThresholds
//         s <- fs.spanThresholds
//       } yield Filter(q, s)
//     )(List(_))
//   }
//   def withBestFilter(fs: FilterSpace, f: Option[Filter]): FilterSpace = {
//     fs.copy(best = f)
//   }
//   def filterBeam(filter: Filter, beam: Beam): Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])] = {
//     val validItems = beam.filter(item =>
//       item.question_prob >= filter.questionThreshold &&
//         item.span_prob >= filter.spanThreshold
//     )
//     ???
//   }
//   val codecs = new BeamProtocol.Codecs[
//     Beam, Filter, FilterSpace] {
//     import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
//     import qasrl.data.JsonCodecs.{spanEncoder, spanDecoder}
//     import io.circe.generic.semiauto._
//     implicit val beamItemDecoder = deriveDecoder[BeamItem]
//     implicit val beamItemEncoder = deriveEncoder[BeamItem]
//     implicit val filterDecoder = deriveDecoder[Filter]
//     implicit val filterEncoder = deriveEncoder[Filter]

//     implicit val filterShow = Show.show[Filter](f =>
//       f"{ q ≥ ${f.questionThreshold}%.2f ∧ s ≥ ${f.spanThreshold}%.2f }"
//     )
//     import io.circe.{Encoder, Decoder}
//     implicit val beamDecoder: Decoder[Beam] = deriveDecoder[List[BeamItem]]
//     implicit val beamEncoder: Encoder[Beam] = deriveEncoder[List[BeamItem]]
//     implicit val filterSpaceDecoder: Decoder[FilterSpace] = deriveDecoder[FilterSpace]
//     implicit val filterSpaceEncoder: Encoder[FilterSpace] = deriveEncoder[FilterSpace]
//   }
// }
