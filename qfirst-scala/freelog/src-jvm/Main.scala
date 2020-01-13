package freelog

import cats._
import cats.effect.IO
import cats.effect.ExitCode
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import scala.concurrent.duration._

object Main extends CommandIOApp(
  name = "mill freelog.run",
  header = "FreeLog sandbox."){

  implicit val logLevel = LogLevel.Info
  implicit val progBar = ProgressSpec.simple(50)

  implicit class RichTraverse[F[_]: Traverse, A](fa: F[A]) {

    def logTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = {
      logger.logTraverse(fa, prefix, logLevel, sizeHint)(f)
    }

    def infoTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg)(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = {
      logger.logTraverse(fa, prefix, LogLevel.Info)(f)
    }

    def infoBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg)(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = {
      logger.logBarTraverse(fa, prefix, LogLevel.Info, fa.size)(f)
    }

    class Old {

    // def traverseLogging[G[_]: Monad, B](
    //   getMessage: Int => String)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   // val prefix = if(label.isEmpty) "" else s"$label: "
    //   logger.block {
    //     logger.info(getMessage(0)) >>
    //       fa.traverseWithIndexM { (a, index) =>
    //         f(a) <* (logger.rewind >> logger.info(getMessage(index + 1)))
    //       }
    //   }
    // }

    // def traverseLoggingIter[G[_]: Monad, B](
    //   prefix: String)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   traverseLogging(i => s"${prefix}${i}it")(f)
    // }

    // def getProgressBar(length: Int)(total: Long)(cur: Long): String = {
    //   val num = math.round(cur.toDouble / total * length).toInt
    //   val pct = math.round(cur.toDouble / total * 100.0).toInt
    //   val bars = "#" * num
    //   val spaces = " " * (length - num)
    //   f"$pct%3d%% [$bars%s$spaces%s] $cur%d/$total%d"
    // }

    // def traverseLoggingProgress[G[_]: Monad, B](
    //   prefix: String, size: Long = fa.size)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   val progress = getProgressBar(30)(size) _
    //   traverseLogging(i => s"${prefix}${progress(i)}")(f)
    // }

    // def traverseLoggingIterSync[G[_]: Monad : Sync, B](
    //   label: String)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   val prefix = if(label.isEmpty) "" else s"$label: "
    //   Ref[G].of(0) >>= { iter =>
    //     logger.block {
    //       fa.traverse { a =>
    //         iter.get >>= (curIter =>
    //           logger.rewind >> logger.log(s"${prefix}${curIter}it") >>
    //             f(a) >>
    //             iter.update(_ + 1)
    //         )
    //       }
    //     }
    //   }
    // }

    // def traverseLoggingIterSync[G[_]: Monad : Sync, B](
    //   label: String)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   val prefix = if(label.isEmpty) "" else s"$label: "
    //   Ref[G].of(0) >>= { iter =>
    //     logger.block {
    //       fa.traverse { a =>
    //         iter.get >>= (curIter =>
    //           logger.rewind >> logger.log(s"${prefix}${curIter}it") >>
    //             f(a) >>
    //             iter.update(_ + 1)
    //         )
    //       }
    //     }
    //   }
    // }

    // private[this] def makeProgressBar // TODO

    // def traverseLoggingProgressSync[G[_]: Monad : Sync, B](
    //   label: String, size: Long = fa.size)(
    //   f: A => G[B])(
    //   implicit logger: EphemeralLogger[G, String]) = {
    //   val prefix = if(label.isEmpty) "" else s"$label: "
    //   Ref[G].of(0) >>= { iter =>
    //     logger.block {
    //       fa.traverse { a =>
    //         iter.get >>= (curIter =>
    //           logger.rewind >> logger.log(s"${prefix}${curIter}it") >>
    //             f(a) >>
    //             iter.update(_ + 1)
    //         )
    //       }
    //     }
    //   }
    // }
    }
  }

  def ephemeralTreeDemo: IO[ExitCode] = {
    for {
      implicit0(logger: EphemeralTreeLogger[IO, String]) <- freelog.loggers.TimingEphemeralTreeConsoleLogger.create()
      _ <- (1 to 1000).toList.infoTraverse("Initializing: ")(_ => IO.sleep(0.002.seconds))

      _ <- (1 to 1000).toList.infoBarTraverse("Buffering: ")(_ => IO.sleep(0.002.seconds))
      _ <- logger.infoBranch("Recruiting") {
        (1 to 1000).toList.infoBarTraverse("New recruits: ")(_ => IO.sleep(0.002.seconds))
      }
      _ <- logger.infoBranch("Dominating") {
        logger.info("Trying soft power..") >> IO.sleep(0.5.seconds) >>
          logger.info("Acknowledging failure of soft power...") >> IO.sleep(1.second) >>
          logger.rewind >>
          logger.info("Mobilizing army...") >> IO.sleep(1.second) >>
          logger.infoBranch("Invading...") {
            logger.info("Landfall!") >> IO.sleep(0.5.seconds) >>
              (1 to 100).toList.infoBarTraverse("Enemies crushed: ")(
                _ => IO.sleep(0.01.seconds)
              ) >> IO.sleep(0.5.seconds) >>
              logger.info("Victory!") >> IO.sleep(1.seconds)
          } >> IO.sleep(0.5.seconds) >>
          logger.info("Now what?")
      }
    } yield ExitCode.Success
  }

  import jjm.io.HttpUtil
  import org.http4s.Uri
  import org.http4s.server.Router
  import org.http4s.server.blaze.BlazeServerBuilder
  import org.http4s.implicits._
  import freelog.loggers.remote.RemoteLoggerService

  // def clientServerDemo: IO[ExitCode] = for {
  //   (simpleLogger: Logger[IO, String]) <- loggers.IndentingLogger.console()
  //   nextSessionId <- Ref[IO].of(0)
  //   port = 9564
  //   serverFiber <- BlazeServerBuilder[IO]
  //   .bindHttp(port, "0.0.0.0")
  //   .withoutBanner.withNio2(true)
  //   .withHttpApp(
  //     Router(
  //       "/" -> HttpUtil.makeHttpPostServer(
  //         RemoteLoggerService.sessionPrefixingService[IO](
  //           nextSessionId, simpleLogger
  //         )
  //       )
  //     ).orNotFound
  //   ).resource.use(_ => IO.never).start
  //   // .serve.compile.drain
  //   _ <- RemoteLoggerService.withHttpBlazeClientLogger[
  //     Unit, Int, String, IO, Unit](
  //     Uri.unsafeFromString(s"http://localhost:$port"), (), cats.Contravariant[Logger[IO, ?]].contramap[String, String](simpleLogger)("[CLIENT] " + _)
  //   ) { logger =>
  //     logger.info("Message 1") >> logger.info("Message 2")
  //   }
  //   _ <- RemoteLoggerService.withHttpBlazeClientLogger[
  //     Unit, Int, String, IO, Unit](
  //     Uri.unsafeFromString(s"http://localhost:$port"), (), cats.Contravariant[Logger[IO, ?]].contramap[String, String](simpleLogger)("[CLIENT] " + _)
  //   ) { logger =>
  //     logger.info("Message 3") >> logger.info("Message 4")
  //   }
  //   _ <- serverFiber.cancel
  // } yield ExitCode.Success

  val main: Opts[IO[ExitCode]] = {
    Opts.unit.as(ephemeralTreeDemo)
  }
}
