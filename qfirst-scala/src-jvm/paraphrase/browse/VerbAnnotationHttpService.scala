package qfirst.paraphrase.browse
import qfirst.paraphrase._

import qfirst.frames.ArgumentSlot
import qfirst.frames.Frame
import qfirst.frames.SimpleFrameInduction

import qasrl.bank.DataIndex
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.Domain
import qasrl.bank.SentenceId

import qasrl.data.Dataset
import qasrl.data.AnswerSpan
import qasrl.labeling.SlotBasedLabel

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref

import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

import io.circe.generic.JsonCodec

import monocle.macros._
import monocle.function.{all => Optics}

import qasrl.bank.JsonCodecs._
import qasrl.bank.service.JsonCodecs._
import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}

import io.circe.syntax._

// @Lenses case class VerbFrameData(
//   inflectionCounts: Map[InflectedForms, Int],
//   allFrames: Map[InflectedForms, VerbFrameset])
// object VerbFrameData {
//   import io.circe.{Encoder, Decoder}
//   def fromLists(
//     inflectionCountsList: List[(InflectedForms, Int)],
//     framesList: List[(InflectedForms, VerbFrameset)],
//   ) = VerbFrameData(inflectionCountsList.toMap, framesList.toMap)
//   implicit val verbFrameDataDecoder: Decoder[VerbFrameData] =
//     Decoder.forProduct2("inflectionCounts", "allFrames")(fromLists)
//   implicit val verbFrameDataEncoder: Encoder[VerbFrameData] =
//     Encoder.forProduct2("inflectionCounts", "allFrames")(d =>
//       (d.inflectionCounts.toList, d.allFrames.toList)
//     )
// }

import EvalApp.ParaphraseAnnotations

case class VerbFrameServiceIO(
  inflectionCounts: Map[InflectedForms, Int],
  frameInductionResults: FrameInductionResults,
  dataset: Dataset,
  getEvaluationItem: Int => (InflectedForms, String, Int), // sentence ID, verb index
  paraphraseStoreRef: Ref[IO, ParaphraseAnnotations],
  saveParaphrases: ParaphraseAnnotations => IO[Unit]
) extends VerbFrameService[IO] {

  def getVerbs: IO[Map[InflectedForms, Int]] =
    IO.pure(inflectionCounts)

  def getFrameset(verb: InflectedForms): IO[VerbFrameset] =
    IO(frameInductionResults.frames(verb))

  def getParaphrasingInfo(i: Int): IO[ParaphrasingInfo] = {
    val (verbInflectedForms, sentenceId, verbIndex) = getEvaluationItem(i)
    val verbFrameset = frameInductionResults.frames(verbInflectedForms)
    val frameDistribution = frameInductionResults.assignments(verbInflectedForms)(sentenceId)(verbIndex)
    paraphraseStoreRef.get.map(paraphrases =>
      ParaphrasingInfo(
        sentenceId, verbIndex,
        dataset.sentences(sentenceId).verbEntries(verbIndex),
        verbFrameset, frameDistribution,
        paraphrases.get(sentenceId).flatMap(_.get(verbIndex)).getOrElse(VerbParaphraseLabels.empty)
      )
    )
  }

  def saveParaphraseAnnotations(
    sentenceId: String, verbIndex: Int, paraphrases: VerbParaphraseLabels
  ): IO[VerbParaphraseLabels] = {
    def updateFn(p: ParaphraseAnnotations) = {
      val newSentenceLabels = p.getOrElse(sentenceId, Map()) + (verbIndex -> paraphrases)
      p + (sentenceId -> newSentenceLabels)
    }
    for {
      _ <- paraphraseStoreRef.update(updateFn)
      store <- paraphraseStoreRef.get
      _ <- saveParaphrases(store)
    } yield store.getOrElse(sentenceId, Map()).getOrElse(verbIndex, VerbParaphraseLabels.empty)
  }
}

object VerbFrameHttpService {

  def make(service: VerbFrameServiceIO) = {

    import io.circe.Encoder

    implicit val verbCountsEncoder =
      implicitly[Encoder[List[(InflectedForms, Int)]]]
        .contramap[Map[InflectedForms, Int]](_.toList)

    implicit val inflectedFormsEntityDecoder = jsonOf[IO, InflectedForms]
    implicit val verbParaphraseLabelsEntityDecoder = jsonOf[IO, VerbParaphraseLabels]
    implicit val intEntityDecoder = jsonOf[IO, Int]
    implicit val tupleEntityDecoder = jsonOf[IO, (String, Int, VerbParaphraseLabels)]

    implicit val verbCountsEntityEncoder = jsonEncoderOf[IO, Map[InflectedForms, Int]]
    implicit val verbFramesetEntityEncoder = jsonEncoderOf[IO, VerbFrameset]
    implicit val paraphrasingInfoEntityEncoder = jsonEncoderOf[IO, ParaphrasingInfo]
    implicit val verbParaphraseLabelsEntityEncoder = jsonEncoderOf[IO, VerbParaphraseLabels]

    // implicit val verbFrameEntityDecoder = jsonOf[IO, VerbFrameset]

    import org.http4s.dsl.io._

    HttpRoutes.of[IO] {
      case POST -> Root / "getVerbs" =>
        service.getVerbs.flatMap(Ok(_))
      case req @ POST -> Root / "getFrameset" =>
        req.as[InflectedForms].flatMap(service.getFrameset).flatMap(Ok(_))
      case req @ POST -> Root / "getParaphrasingInfo" =>
        req.as[Int].flatMap(service.getParaphrasingInfo).flatMap(Ok(_))
      case req @ POST -> Root / "saveParaphraseAnnotations" =>
        req.as[(String, Int, VerbParaphraseLabels)].flatMap(Function.tupled(service.saveParaphraseAnnotations(_, _, _))).flatMap(Ok(_))
    }
  }
}
