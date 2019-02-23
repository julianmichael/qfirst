package qfirst.frames.annotation

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
import io.circe.syntax._

@Lenses case class ClauseResolutionData(
  localResolutions: Map[ClauseAmbiguity, Set[ClauseChoice]],
  fullResolutions: Map[ClauseAmbiguity, Set[ClauseChoice]])
object ClauseResolutionData {
  import io.circe.{Encoder, Decoder}
  def fromLists(
    local: List[(ClauseAmbiguity, Set[ClauseChoice])],
    full: List[(ClauseAmbiguity, Set[ClauseChoice])]
  ) = ClauseResolutionData(local.toMap, full.toMap)
  implicit val clauseResolutionDataDecoder: Decoder[ClauseResolutionData] =
    Decoder.forProduct2("localResolutions", "fullResolutions")(fromLists)
  implicit val clauseResolutionDataEncoder: Encoder[ClauseResolutionData] =
    Encoder.forProduct2("localResolutions", "fullResolutions")(d =>
      (d.localResolutions.toList, d.fullResolutions.toList)
    )
}


case class ClauseAnnotationServiceIO(
  dataset: Dataset,
  storeRef: Ref[IO, ClauseResolutionData],
  saveData: ClauseResolutionData => IO[Unit]
) extends ClauseAnnotationService[IO] {
  val instances = SimpleFrameInduction.getInstances(dataset)
  val emptyModel = SimpleFrameInduction.Model.init(instances)

  val (localAmbiguities, fullAmbiguities) = {
    dataset.sentences.toVector.flatMap { case (sidStr, sentence) =>
      val sid = SentenceId.fromString(sidStr)
      sentence.verbEntries.toVector.flatMap { case (verbIndex, verb) =>
        val instance = SimpleFrameInduction.getInstance(verb)
        val allQs = instance.allFramesWithAnswer.keySet
        val unambiguousQs = instance.allFramesWithAnswer.toList.filter(_._2.size == 1).map(_._1).toSet
        val locallyResolvedInstance = emptyModel.chooseBestFrames(instance)
        val locallyResolvedQs = locallyResolvedInstance.allFramesWithAnswer.toList.filter(_._2.size == 1).map(_._1).toSet
        val locallyAmbiguousQs = locallyResolvedQs -- unambiguousQs
        val fullyAmbiguousQs = allQs -- locallyResolvedQs
        def getAmbiguity(qString: String) = ClauseAmbiguity(
          sid, verbIndex, qString, instance.allFramesWithAnswer(qString).map(ClauseChoice.make)
        )
        locallyAmbiguousQs.toVector.map(getAmbiguity).map(Left(_)) ++
          fullyAmbiguousQs.toVector.map(getAmbiguity).map(Right(_))
      }
    }
  }.separate

  val rand = new scala.util.Random(35876295327897L)

  val sortedLocalAmbiguities = rand.shuffle(localAmbiguities)
  val sortedFullAmbiguities  = rand.shuffle(fullAmbiguities)

  override def getResolution(isFull: Boolean, index: Int): IO[ClauseResolution] = for {
    store <- storeRef.get
  } yield {
    val ambig =
      if(isFull) sortedFullAmbiguities(index)
      else sortedLocalAmbiguities(index)
    val choiceOpt =
      if(isFull) store.fullResolutions.get(ambig)
      else store.localResolutions.get(ambig)
    ClauseResolution(ambig, choiceOpt)
  }

  override def saveResolution(isFull: Boolean, index: Int, choice: Set[ClauseChoice]): IO[ClauseResolution] = {
    val ambig = if(isFull) sortedFullAmbiguities(index) else sortedLocalAmbiguities(index)
    val lens = (
      if(isFull) ClauseResolutionData.fullResolutions
      else ClauseResolutionData.localResolutions
    ).composeLens(Optics.at(ambig))
    for {
      _ <- storeRef.update(lens.set(Some(choice)))
      store <- storeRef.get
      _ <- saveData(store)
    } yield ClauseResolution(ambig, lens.get(store))
  }
}

object ClauseAnnotationHttpService {

  def make(service: ClauseAnnotationServiceIO) = {

    implicit val clauseChoiceDecoder = jsonOf[IO, Set[ClauseChoice]]
    implicit val clauseResolutionEncoder = jsonEncoderOf[IO, ClauseResolution]
    implicit val clauseChoiceOptEncoder = jsonEncoderOf[IO, Option[ClauseChoice]]

    import org.http4s.dsl.io._

    HttpRoutes.of[IO] {
      case GET -> Root / "local" / IntVar(index) =>
        service.getResolution(false, index).flatMap(Ok(_))
      case GET -> Root / "full" / IntVar(index) =>
        service.getResolution(true, index).flatMap(Ok(_))
      case req @ POST -> Root / "local" / "save" / IntVar(index) =>
        req.as[Set[ClauseChoice]].map(service.saveResolution(false, index, _)).flatMap(Ok(_))
      case req @ POST -> Root / "full" / "save" / IntVar(index) =>
        req.as[Set[ClauseChoice]].map(service.saveResolution(true, index, _)).flatMap(Ok(_))
    }
  }
}
