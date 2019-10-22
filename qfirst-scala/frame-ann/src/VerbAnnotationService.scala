package qfirst.frames.verbal

import jjm.DotKleisli
import jjm.DotFunctionK
import jjm.{DotEncoder, DotDecoder}
import jjm.ling.en.InflectedForms

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._
import cats.effect.IO
import cats.effect.concurrent.Ref

import qasrl.bank.SentenceId

import qasrl.data.Dataset

import qfirst.frames.ArgumentSlot
import qfirst.frames.ArgStructure
import qfirst.frames.Frame

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import monocle.function.{all => Optics}
import monocle.macros._

@Lenses @JsonCodec case class QuestionId(
  sentenceId: SentenceId,
  verbIndex: Int,
  questionString: String)
object QuestionId {
  implicit val questionIdOrder =
    Order.whenEqual(
      Order.by[QuestionId, SentenceId](_.sentenceId),
      Order.whenEqual(
        Order.by[QuestionId, Int](_.verbIndex),
        Order.by[QuestionId, String](_.questionString)
      )
    )
}

@Lenses @JsonCodec case class FrameClause(
  args: ArgStructure,
  argMapping: Map[ArgumentSlot, String],
  instances: Map[ArgumentSlot, NonEmptySet[QuestionId]])
object FrameClause


@Lenses @JsonCodec case class VerbFrame(
  inflectedForms: InflectedForms,
  clauseSets: List[List[FrameClause]]
)
object VerbFrame

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

case class VerbAnnotationService[F[_]](
  f: DotKleisli[F, VerbAnnotationService.Request]) { self =>
  import VerbAnnotationService._
  def getVerbs: F[Map[InflectedForms, Int]] = f(GetVerbs)
  def getFrame(verb: InflectedForms): F[VerbFrame] = f(GetFrame(verb))
  def saveFrame(frame: VerbFrame): F[VerbFrame] = f(SaveFrame(frame))
}

object VerbAnnotationService {
  @JsonCodec sealed trait Request { type Out }

  case object GetVerbs extends Request { type Out = Map[InflectedForms, Int] }
  @JsonCodec case class GetFrame(verb: InflectedForms) extends Request { type Out = VerbFrame }
  @JsonCodec case class SaveFrame(frame: VerbFrame) extends Request { type Out = VerbFrame }

  object Request {
    // TODO: why do we need the casts here?? and no matter how I rephrase it, it still complains...
    implicit val verbAnnotationServiceRequestDotEncoder = new DotKleisli[Encoder, Request] {
      def apply(req: Request): Encoder[req.Out] = req match {
        case GetVerbs => implicitly[Encoder[List[(InflectedForms, Int)]]]
          .contramap[Map[InflectedForms, Int]](_.toList).asInstanceOf[Encoder[req.Out]]
        case GetFrame(_) => implicitly[Encoder[VerbFrame]].asInstanceOf[Encoder[req.Out]]
        case SaveFrame(_) => implicitly[Encoder[VerbFrame]].asInstanceOf[Encoder[req.Out]]
      }
    }
    implicit val verbAnnotationServiceRequestDotDecoder = new DotKleisli[Decoder, Request] {
      def apply(req: Request): Decoder[req.Out] = req match {
        case GetVerbs => implicitly[Decoder[List[(InflectedForms, Int)]]]
          .map(_.toMap).asInstanceOf[Decoder[req.Out]]
        case GetFrame(_) => implicitly[Decoder[VerbFrame]].asInstanceOf[Decoder[req.Out]]
        case SaveFrame(_) => implicitly[Decoder[VerbFrame]].asInstanceOf[Decoder[req.Out]]
      }
    }
    // implicit val verbAnnotationServiceRequestDotDecoder: DotDecoder[Request] = new DotDecoder[Request] {
    //   def apply(req: Request): Decoder[req.Out] = _apply[req.Out](req)
    //   private[this] def _apply[A](req: Request { type Out = A }): Decoder[A] = req match {
    //     case GetVerbs => implicitly[Decoder[List[(InflectedForms, Int)]]].map(_.toMap)
    //     case GetFrame(_) => implicitly[Decoder[VerbFrame]]
    //     case SaveFrame(_) => implicitly[Decoder[VerbFrame]]
    //   }
    // }
  }

  def basicIOService(
    storeRef: Ref[IO, VerbFrameData],
    saveData: VerbFrameData => IO[Unit]
  ): DotKleisli[IO, Request] = DotKleisli.fromFunctionK(
    new DotFunctionK[IO, Request] {
      def apply[A](req: Request { type Out = A }): IO[A] = req match {
        case GetVerbs =>
          storeRef.get.map(_.inflectionCounts): IO[Map[InflectedForms, Int]]

        case GetFrame(verb) =>
          storeRef.get.map(_.allFrames(verb)).asInstanceOf[IO[A]]

        case SaveFrame(frame) =>
          val lens = VerbFrameData.allFrames
            .composeLens(Optics.at(frame.inflectedForms))
          for {
            _ <- storeRef.update(lens.set(Some(frame)))
            store <- storeRef.get
            _ <- saveData(store)
          } yield store.allFrames(frame.inflectedForms).asInstanceOf[A]
      }
    }
  )
}
