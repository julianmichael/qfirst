package freelog
package loggers.remote

import jjm.DotKleisli
import jjm.{DotEncoder, DotDecoder}

import cats.effect.Effect

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

trait RemoteLoggerService[SessionRequest, SessionId, Msg, F[_]]
    extends DotKleisli[F, RemoteLoggerService.Request[SessionRequest, SessionId, Msg]] {
  def start(request: SessionRequest): F[SessionId]
  def log(id: SessionId, msg: Msg): F[Unit]
  def end(id: SessionId): F[Unit]
  def apply(req: RemoteLoggerService.Request[SessionRequest, SessionId, Msg]): F[req.Out] =
    req match {
      case RemoteLoggerService.Request.Start(request) => start(request).asInstanceOf[F[req.Out]]
      case RemoteLoggerService.Request.Log(id, msg) => log(id, msg).asInstanceOf[F[req.Out]]
      case RemoteLoggerService.Request.End(id) => end(id).asInstanceOf[F[req.Out]]
    }
}

object RemoteLoggerService extends RemoteLoggerServicePlatformExtensions {
  def getLogger[SessionRequest, SessionId, Msg, F[_]](
    f: DotKleisli[F, RemoteLoggerService.Request[SessionRequest, SessionId, Msg]],
    id: SessionId
  ) = new Logger[F, Msg] {
    def log(msg: Msg) = f(Request.Log(id, msg))
  }

  private[this] case class RemoteLoggerServiceDKWrapper[SessionRequest, SessionId, Msg, F[_]](
    f: DotKleisli[F, RemoteLoggerService.Request[SessionRequest, SessionId, Msg]]
  ) extends RemoteLoggerService[SessionRequest, SessionId, Msg, F] {
    def start(request: SessionRequest): F[SessionId] = f(Request.Start(request))
    def log(id: SessionId, msg: Msg): F[Unit] = f(Request.Log(id, msg))
    def end(id: SessionId): F[Unit] = f(Request.End(id))
    override def apply(req: RemoteLoggerService.Request[SessionRequest, SessionId, Msg]): F[req.Out] =
      f(req)
  }

  def fromDotKleisli[F[_], SessionRequest, SessionId, Msg](
    f: DotKleisli[F, Request[SessionRequest, SessionId, Msg]]
  ): RemoteLoggerService[SessionRequest, SessionId, Msg, F] =
    RemoteLoggerServiceDKWrapper(f)

  @JsonCodec sealed trait Request[SessionRequest, SessionId, Msg] { type Out }
  object Request {
    @JsonCodec case class Start[SessionRequest, SessionId, Msg](
      request: SessionRequest
    ) extends Request[SessionRequest, SessionId, Msg] {
      type Out = SessionId
    }
    @JsonCodec case class Log[SessionRequest, SessionId, Msg](
      id: SessionId, msg: Msg
    ) extends Request[SessionRequest, SessionId, Msg] {
      type Out = Unit
    }
    @JsonCodec case class End[SessionRequest, SessionId, Msg](
      id: SessionId
    ) extends Request[SessionRequest, SessionId, Msg] {
      type Out = Unit
    }

    implicit def remoteLoggerServiceRequestDotEncoder[
      SessionRequest, SessionId: Encoder, Msg
    ] = new DotEncoder[Request[SessionRequest, SessionId, Msg]] {
      def apply(req: Request[SessionRequest, SessionId, Msg]) = req match {
        case Request.Start(_) => implicitly[Encoder[SessionId]].asInstanceOf[Encoder[req.Out]]
        case Request.Log(_, _) => implicitly[Encoder[Unit]].asInstanceOf[Encoder[req.Out]]
        case Request.End(_) => implicitly[Encoder[Unit]].asInstanceOf[Encoder[req.Out]]
      }
    }

    implicit def remoteLoggerServiceRequestDotDecoder[
      SessionRequest, SessionId: Decoder, Msg
    ] = new DotDecoder[Request[SessionRequest, SessionId, Msg]] {
      def apply(req: Request[SessionRequest, SessionId, Msg]) = req match {
        case Request.Start(_) => implicitly[Decoder[SessionId]].asInstanceOf[Decoder[req.Out]]
        case Request.Log(_, _) => implicitly[Decoder[Unit]].asInstanceOf[Decoder[req.Out]]
        case Request.End(_) => implicitly[Decoder[Unit]].asInstanceOf[Decoder[req.Out]]
      }
    }
  }
}
