package qfirst.browse
import qfirst.FileUtil

import qasrl.bank.Data

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.{IOApp, ExitCode}
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import qasrl.bank.service.HttpDocumentService
import qasrl.bank.service.DocumentServiceWebServer
import qasrl.bank.service.Search

import java.nio.file.Path
import java.nio.file.Files

import com.monovore.decline._

import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

object Serve extends IOApp {

  import scala.concurrent.ExecutionContext.Implicits.global

  def _run(
    jsDepsPath: Path, jsPath: Path,
    qasrlBankPath: Path, savePath: Path,
    domain: String, port: Int
  ): IO[ExitCode] = {
    IO(Data.readFromQasrlBank(qasrlBankPath).toEither.right.get).flatMap { data =>
      val docApiSuffix = "doc"
      val verbApiSuffix = "verb"
      val pageService = StaticPageService.makeService(
        domain,
        docApiSuffix, verbApiSuffix,
        jsDepsPath, jsPath, port
      )

      val index = data.index
      val docs = data.documentsById
      val searchIndex = Search.createSearchIndex(docs.values.toList)
      val docService = HttpDocumentService.makeService(index, docs, searchIndex)

      val saveData = (data: VerbFrameData) => {
        FileUtil.writeJson(savePath, io.circe.Printer.noSpaces)(data)
      }

      for {
        frameData <- FileUtil.readJson[VerbFrameData](savePath)
        frameDataRef <- Ref[IO].of(frameData)
        annotationService = VerbFrameHttpService.make(
          VerbFrameServiceIO(frameDataRef, saveData)
        )
        app = Router(
          "/" -> pageService,
          s"/$docApiSuffix" -> docService,
          s"/$verbApiSuffix" -> annotationService,
        ).orNotFound
        _ <- BlazeServerBuilder[IO]
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(app)
        .serve.compile.drain
      } yield ExitCode.Success
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    val jsDepsPathO = Opts.option[Path](
      "jsDeps", metavar = "path", help = "Where to get the JS deps file."
    )

    val jsPathO = Opts.option[Path](
      "js", metavar = "path", help = "Where to get the JS main file."
    )

    val qasrlBankO = Opts.option[Path](
      "qasrl-bank", metavar = "path", help = "Path to the QA-SRL Bank 2.0 data, e.g., ../qasrl-bank/data/qasrl-v2."
    )

    val saveO = Opts.option[Path](
      "load", metavar = "path", help = "Where to load the verb frames from."
    )

    val domainO = Opts.option[String](
      "domain", metavar = "domain", help = "domain name the server is being hosted at."
    )

    val portO = Opts.option[Int](
      "port", metavar = "port number", help = "Port to host the HTTP service on."
    )

    // val domainRestrictionO = Opts.option[String](
    //   "domain", metavar = "http://...",
    //   help = "Domain to impose CORS restrictions to (otherwise, all domains allowed)."
    // ).map(NonEmptySet.of(_)).orNone

    val command = Command(
      name = "mill -i qfirst.jvm.runVerbAnn",
      header = "Spin up the annotation server for QA-SRL Clause frames.") {
      (jsDepsPathO, jsPathO, qasrlBankO, saveO, domainO, portO).mapN(_run(_, _, _, _, _, _))
    }

    command.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
