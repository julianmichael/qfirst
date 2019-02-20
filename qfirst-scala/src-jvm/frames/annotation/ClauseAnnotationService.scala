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

@JsonCodec case class ClauseChoice(
  frame: Frame,
  argumentSlot: ArgumentSlot)
object ClauseChoice {
  def make(p: (Frame, ArgumentSlot)) = ClauseChoice(p._1, p._2)
}

@JsonCodec case class ClauseAmbiguity(
  sentenceId: SentenceId,
  verbIndex: Int,
  questionString: String,
  structures: Set[ClauseChoice])

@JsonCodec case class ClauseResolution(
  ambiguity: ClauseAmbiguity,
  choiceOpt: Option[ClauseChoice]
)

@Lenses case class ClauseResolutionData(
  localResolutions: Map[ClauseAmbiguity, ClauseChoice],
  fullResolutions: Map[ClauseAmbiguity, ClauseChoice])
object ClauseResolutionData {
  import io.circe.{Encoder, Decoder}
  def fromLists(
    local: List[(ClauseAmbiguity, ClauseChoice)],
    full: List[(ClauseAmbiguity, ClauseChoice)]
  ) = ClauseResolutionData(local.toMap, full.toMap)
  implicit val clauseResolutionDataDecoder: Decoder[ClauseResolutionData] =
    Decoder.forProduct2("localResolutions", "fullResolutions")(fromLists)
  implicit val clauseResolutionDataEncoder: Encoder[ClauseResolutionData] =
    Encoder.forProduct2("localResolutions", "fullResolutions")(d =>
      (d.localResolutions.toList, d.fullResolutions.toList)
    )
}

object ClauseAnnotationService {

  def makeService(
    dataset: Dataset,
    initData: ClauseResolutionData,
    saveData: ClauseResolutionData => IO[Unit]
  ) = {

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

    val storeRef = Ref[IO].of(initData)

    def registerClauseChoice(
      isFull: Boolean,
      index: Int,
      frameChoice: ClauseChoice) = {
      val ambig = if(isFull) sortedFullAmbiguities(index) else sortedLocalAmbiguities(index)
      val lens = (
        if(isFull) ClauseResolutionData.fullResolutions
        else ClauseResolutionData.localResolutions
      ).composeLens(Optics.at(ambig))
      for {
        ref <- storeRef
        _ <- ref.update(lens.set(Some(frameChoice)))
        store <- ref.get
        _ <- saveData(store)
      } yield ()
    }

    implicit val clauseChoiceDecoder = jsonOf[IO, ClauseChoice]
    implicit val clauseResolutionEncoder = jsonEncoderOf[IO, ClauseResolution]

    import org.http4s.dsl.io._

    HttpRoutes.of[IO] {
      case GET -> Root / "local" / IntVar(index) =>
        val ambig = sortedLocalAmbiguities(index)
        val choiceIO = for {
          ref <- storeRef
          store <- ref.get
        } yield store.localResolutions.get(ambig)
        choiceIO.flatMap(choiceOpt => Ok(ClauseResolution(ambig, choiceOpt)))
      case GET -> Root / "full" / IntVar(index) =>
        val ambig = sortedFullAmbiguities(index)
        val choiceIO = for {
          ref <- storeRef
          store <- ref.get
        } yield store.fullResolutions.get(ambig)
        choiceIO.flatMap(choiceOpt => Ok(ClauseResolution(ambig, choiceOpt)))
      case req @ POST -> Root / "local" / "save" / IntVar(index) =>
        req.as[ClauseChoice].map(registerClauseChoice(false, index, _)).as(Response(Status.Ok))
      case req @ POST -> Root / "full" / "save" / IntVar(index) =>
        req.as[ClauseChoice].map(registerClauseChoice(true, index, _)).as(Response(Status.Ok))
    }
  }
}
