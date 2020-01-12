package freelog
package loggers.remote

import scala.collection.immutable.TreeMap

import cats.Monad
import cats.effect.ConcurrentEffect
import cats.effect.concurrent.Ref
import cats.implicits._

import io.circe.{Encoder, Decoder}

import org.http4s._
import org.http4s.client.blaze._
// import org.http4s.dsl.io._
// import org.http4s.implicits._

import jjm.io.HttpUtil


// case class HttpLoggingServer()
trait RemoteLoggerServicePlatformExtensions {

  def withHttpBlazeClientLogger[
    SessionRequest: Encoder, SessionId: Encoder: Decoder, Msg: Encoder,
    F[_]: ConcurrentEffect, A](
    endpoint: Uri,
    sessionRequest: SessionRequest,
    metaLogger: Logger[F, String])(
    run: Logger[F, Msg] => F[A])(
    implicit logLevel: LogLevel
  ): F[A] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BlazeClientBuilder[F](global).resource.use { client =>
      val loggerClient = RemoteLoggerService.fromDotKleisli(
        HttpUtil.makeHttpPostClient[
          F, RemoteLoggerService.Request[SessionRequest, SessionId, Msg]](
          client, endpoint
        )
      )
      for {
        sessionId <- loggerClient.start(sessionRequest)
        _ <- metaLogger.info(s"Logging session $sessionId started at $endpoint")
        res <- run(RemoteLoggerService.getLogger(loggerClient, sessionId))
        _ <- loggerClient.end(sessionId)
        _ <- metaLogger.info(s"Logging session $sessionId completed at $endpoint")
      } yield res
    }
  }

  // TODO: error reporting
  def basicLogListingService[SessionRequest, Msg, F[_]: Monad](
    startingId: Ref[F, Int],
    currentSessions: Ref[F, TreeMap[Int, Vector[(Msg, LogLevel)]]],
    finishedSessions: Ref[F, TreeMap[Int, Vector[(Msg, LogLevel)]]])(
    implicit logLevel: LogLevel
  ) = new RemoteLoggerService[Unit, Int, Msg, F] {
    def start(request: Unit): F[Int] = for {
      id <- startingId.get
      _ <- startingId.update(_ + 1)
      _ <- currentSessions.update(_ + (id -> Vector.empty))
    } yield id

    def log(id: Int, msg: Msg, logLevel: LogLevel): F[Unit] = currentSessions.update(logs =>
      logs + (id -> (logs(id) :+ (msg -> logLevel)))
    )

    def end(id: Int): F[Unit] = for {
      logs <- currentSessions.get.map(_.apply(id))
      _ <- currentSessions.update(_ - id)
      _ <- finishedSessions.update(_ + (id -> logs))
    } yield ()
  }

  def sessionPrefixingService[F[_]: Monad](
    nextSessionId: Ref[F, Int],
    logger: Logger[F, String])(
    implicit ambientLevel: LogLevel
  ) = new RemoteLoggerService[Unit, Int, String, F] {
    def start(request: Unit): F[Int] = for {
      id <- nextSessionId.get
      _ <- logger.info(s"[META] Begun session $id")
      _ <- nextSessionId.update(_ + 1)
    } yield id

    def log(id: Int, msg: String, logLevel: LogLevel): F[Unit] = {
      logger.log(f"[$id%4d] $msg", logLevel)
    }

    def end(id: Int): F[Unit] = {
      logger.info(s"[META] Finished session $id")
    }
  }
}
