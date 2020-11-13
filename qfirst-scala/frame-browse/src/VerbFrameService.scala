package qfirst.frame.browse
import qfirst.frame._

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import qasrl.labeling.SlotBasedLabel
import qasrl.data.Dataset
import qasrl.data.VerbEntry

import jjm.DotKleisli
import jjm.DotFunctionK
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

case class VerbFrameService[F[_], VerbType, Arg](
  f: DotKleisli[F, VerbFrameService.Request[VerbType, Arg]]) { self =>
  import VerbFrameService._
  def getVerbs: F[Map[VerbType, Int]] = f(GetVerbs())
  def getModel(verb: VerbType): F[VerbClusterModel[VerbType, Arg]] = f(GetModel(verb))
}
object VerbFrameService {
  @JsonCodec sealed trait Request[VerbType, Arg] { type Out }
  case class GetVerbs[VerbType, Arg]() extends Request[VerbType, Arg] { type Out = Map[VerbType, Int] }
  @JsonCodec case class GetModel[VerbType, Arg](verb: VerbType) extends Request[VerbType, Arg] { type Out = VerbClusterModel[VerbType, Arg] }

  object Request {
    implicit def verbFrameServiceRequestDotEncoder[VerbType: Encoder, Arg: Encoder] = new DotKleisli[Encoder, Request[VerbType, Arg]] {
      def apply(req: Request[VerbType, Arg]): Encoder[req.Out] = req match {
        case GetVerbs() => implicitly[Encoder[List[(VerbType, Int)]]]
            .contramap[Map[VerbType, Int]](_.toList).asInstanceOf[Encoder[req.Out]]
        case GetModel(_) => implicitly[Encoder[VerbClusterModel[VerbType, Arg]]].asInstanceOf[Encoder[req.Out]]
      }
    }
    implicit def verbFrameServiceRequestDotDecoder[VerbType: Decoder, Arg: Decoder] = new DotKleisli[Decoder, Request[VerbType, Arg]] {
      def apply(req: Request[VerbType, Arg]): Decoder[req.Out] = req match {
        case GetVerbs() => implicitly[Decoder[List[(VerbType, Int)]]]
            .map(_.toMap).asInstanceOf[Decoder[req.Out]]
        case GetModel(_) => implicitly[Decoder[VerbClusterModel[VerbType, Arg]]].asInstanceOf[Decoder[req.Out]]
      }
    }
  }

  def basicIOService[VerbType, Arg](
    verbModels: Map[VerbType, VerbClusterModel[VerbType, Arg]],
  ): DotKleisli[IO, Request[VerbType, Arg]]  = DotKleisli.fromFunctionK(
    new DotFunctionK[IO, Request[VerbType, Arg]] {
      def apply[A](req: Request[VerbType, Arg] { type Out = A }): IO[A] = {
        // @SuppressWarnings(Array("all"))
        val res = req match {
          case GetVerbs() => IO.pure(verbModels.mapVals(_.verbClustering.size.toInt)): IO[Map[VerbType, Int]]
          case GetModel(verb) => IO(verbModels(verb)): IO[VerbClusterModel[VerbType, Arg]]
        }
        res.asInstanceOf[IO[A]]
      }
    }
  )
}
