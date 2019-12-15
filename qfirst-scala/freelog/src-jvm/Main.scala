package freelog

import cats._
import cats.implicits._
import cats.effect.IO
import cats.effect.ExitCode
import cats.effect.concurrent.Ref

import com.monovore.decline._
import com.monovore.decline.effect._

import scala.concurrent.duration._

object Main extends CommandIOApp(
  name = "mill freelog.run",
  header = "FreeLog sandbox."){

  implicit class StringLogger[F[_]](logger: Logger[String, F]) {
    def logln(msg: String): F[Unit] = logger.log(msg + "\n")
    def logln: F[Unit] = logln("")
  }

  implicit class RichTraverse[F[_]: Traverse, A](fa: F[A]) {
    def traverseLogging[B](
      f: A => IO[B])(
      implicit logger: EphemeralLogger[String, IO]) = {
      Ref[IO].of(0) >>= { iter =>
        logger.save >> fa.traverse { a =>
          iter.get >>= (curIter =>
            logger.rewind >> logger.log(s"${curIter}it") >>
              f(a) >>
              iter.update(_ + 1)
          )
        } >> logger.commit
      }
    }
  }

  def program: IO[ExitCode] = {
    for {
      logger <- Loggers.EphemeralConsoleLogger.create()
      _ <- logger.save
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.log) >> IO.sleep(1.seconds)
      _ <- logger.restore >> logger.save >> logger.flush >> IO.sleep(1.seconds)
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.log) >> IO.sleep(1.seconds)
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.logln) >> IO.sleep(1.seconds)
      _ <- logger.restore >> logger.save >> logger.flush >> IO.sleep(1.seconds)
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.log) >> IO.sleep(1.seconds)
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.logln) >> IO.sleep(1.seconds)
      _ <- logger.checkpointState.get.map(_.toString).flatMap(logger.logln) >> IO.sleep(1.seconds)
      _ <- logger.restore >> logger.save >> logger.flush >> IO.sleep(1.seconds)
      _ <- (1 to 5).toList.traverse(i =>
        logger.restore >> logger.save >> logger.log(s"Counting $i") >> IO.sleep(0.5.seconds) >>
          List.fill(3)(logger.log(".") >> IO.sleep(0.5.seconds)).sequence
      )
      _ <- logger.commit
      _ <- logger.logln
      _ <- logger.log("Counting ")
      _ <- logger.save
      _ <- (1 to 5).toList.traverse(i =>
        logger.restore >> logger.save >> logger.log(s"$i") >> IO.sleep(0.5.seconds) >>
          List.fill(3)(logger.log(".") >> IO.sleep(0.5.seconds)).sequence
      )
      _ <- logger.commit
      _ <- logger.logln
      _ <- (1 to 10000).toList.traverseLogging(_ => IO.sleep(0.001.seconds))(logger)
    } yield ExitCode.Success
  }

  val main: Opts[IO[ExitCode]] = {
    Opts.unit.as(program)
  }
}
