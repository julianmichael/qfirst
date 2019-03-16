package qfirst.frames.verbal

import qfirst.frames.ArgumentSlot
import qfirst.frames.Frame
import qfirst.frames.SimpleFrameInduction

import qasrl.bank.DataIndex
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.Domain
import qasrl.bank.SentenceId

import qasrl.data.Dataset

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

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

@Lenses case class VerbFrameData(
  inflectionCounts: Map[InflectedForms, Int],
  allFrames: Map[InflectedForms, VerbFrame])
object VerbFrameData {
  import io.circe.{Encoder, Decoder}
  def fromLists(
    inflectionCountsList: List[(InflectedForms, Int)],
    framesList: List[(InflectedForms, VerbFrame)],
  ) = VerbFrameData(inflectionCountsList.toMap, framesList.toMap)
  implicit val verbFrameDataDecoder: Decoder[VerbFrameData] =
    Decoder.forProduct2("inflectionCounts", "allFrames")(fromLists)
  implicit val verbFrameDataEncoder: Encoder[VerbFrameData] =
    Encoder.forProduct2("inflectionCounts", "allFrames")(d =>
      (d.inflectionCounts.toList, d.allFrames.toList)
    )

  def newFromDataset(dataset: Dataset) = {
    val inflectionCounts = Dataset.verbEntries.getAll(dataset)
    .groupBy(_.verbInflectedForms)
    .map { case (forms, verbs) => forms -> verbs.size }
    .toMap
    val initFrames = inflectionCounts.keys
      .map(forms => forms -> VerbFrame(forms, Nil))
      .toMap
    VerbFrameData(inflectionCounts, initFrames)
  }
}


case class VerbAnnotationServiceIO(
  storeRef: Ref[IO, VerbFrameData],
  saveData: VerbFrameData => IO[Unit]
) extends VerbAnnotationService[IO] {

  def getVerbs: IO[Map[InflectedForms, Int]] =
    storeRef.get.map(_.inflectionCounts)

  def getFrame(verb: InflectedForms): IO[VerbFrame] =
    storeRef.get.map(_.allFrames(verb))

  def saveFrame(frame: VerbFrame): IO[VerbFrame] = {
    val lens = VerbFrameData.allFrames
      .composeLens(Optics.at(frame.inflectedForms))
    for {
      _ <- storeRef.update(lens.set(Some(frame)))
      store <- storeRef.get
      _ <- saveData(store)
    } yield store.allFrames(frame.inflectedForms)
  }
}

object VerbAnnotationHttpService {

  def make(service: VerbAnnotationServiceIO) = {

    import io.circe.Encoder

    implicit val verbCountsEncoder =
      implicitly[Encoder[List[(InflectedForms, Int)]]]
        .contramap[Map[InflectedForms, Int]](_.toList)

    implicit val inflectedFormsEntityDecoder = jsonOf[IO, InflectedForms]
    implicit val verbCountsEntityEncoder = jsonEncoderOf[IO, Map[InflectedForms, Int]]
    implicit val verbFrameEntityEncoder = jsonEncoderOf[IO, VerbFrame]
    implicit val verbFrameEntityDecoder = jsonOf[IO, VerbFrame]

    import org.http4s.dsl.io._

    HttpRoutes.of[IO] {
      case POST -> Root / "getVerbs" =>
        service.getVerbs.flatMap(Ok(_))
      case req @ POST -> Root / "getFrame" =>
        req.as[InflectedForms].flatMap(service.getFrame).flatMap(Ok(_))
      case req @ POST -> Root / "saveFrame" =>
        req.as[VerbFrame].flatMap(service.saveFrame).flatMap(Ok(_))
    }
  }
}
