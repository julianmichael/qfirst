package qfirst.paraphrase.browse
import qfirst._
import qfirst.paraphrase._
import qfirst.protocols.SimpleQAs
import qfirst.FileUtil

import qasrl.bank.Data
import qasrl.data.Dataset
import qasrl.labeling.SlotBasedLabel

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

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import EvalApp.ParaphraseAnnotations
import EvalApp.QABeam

object Serve extends IOApp {

  import scala.concurrent.ExecutionContext.Implicits.global

  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))

  val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)

  // def _run(
  //   jsDepsPath: Path, jsPath: Path,
  //   experimentName: String,
  //   testOnTest: Boolean,
  //   domain: String,
  //   port: Int
  // ): IO[ExitCode] = {
  //   Config.make(experimentName, None, testOnTest).flatMap { config =>
  //     config.readWholeQasrlBank.flatMap { data =>
  //       val docApiSuffix = "doc"
  //       val verbApiSuffix = "verb"
  //       val pageService = StaticPageService.makeService(
  //         domain,
  //         docApiSuffix, verbApiSuffix,
  //         config.trainOnDev,
  //         jsDepsPath, jsPath, port
  //       )

  //       val index = data.index
  //       val docs = data.documentsById
  //       val searchIndex = Search.createSearchIndex(docs.values.toList)
  //       val docService = HttpDocumentService.makeService(index, docs, searchIndex)

  //       implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
  //       val fullSet = config.getInputSet(data) |+| config.getEvalSet(data)
  //       val inflectionCounts = Dataset.verbEntries.getAll(fullSet).foldMap(v => Map(v.verbInflectedForms -> 1))

  //       for {
  //         verbFramesets <- config.readFramesets
  //         goldParaphrases <- config.readGoldParaphrases
  //         evaluationItems <- config.getEvaluationItems
  //         goldParaphraseDataRef <- Ref[IO].of(goldParaphrases)
  //         annotationService = VerbFrameHttpService.make(
  //           VerbFrameServiceIO(
  //             inflectionCounts,
  //             verbFramesets,
  //             fullSet,
  //             evaluationItems.apply,
  //             goldParaphraseDataRef,
  //             config.saveGoldParaphrases(_))
  //         )
  //         app = Router(
  //           "/" -> pageService,
  //           s"/$docApiSuffix" -> docService,
  //           s"/$verbApiSuffix" -> annotationService,
  //           ).orNotFound
  //         _ <- BlazeServerBuilder[IO]
  //         .bindHttp(port, "0.0.0.0")
  //         .withHttpApp(app)
  //         .serve.compile.drain
  //       } yield ExitCode.Success
  //     }
  //   }
  // }

  override def run(args: List[String]): IO[ExitCode] = {

    val jsDepsPathO = Opts.option[Path](
      "jsDeps", metavar = "path", help = "Where to get the JS deps file."
    )

    val jsPathO = Opts.option[Path](
      "js", metavar = "path", help = "Where to get the JS main file."
    )

    val experimentNameO = Opts.option[String](
      "name", metavar = "path", help = "Relative path to the model output directory."
    )

    val testOnTestO = Opts.flag(
      "test", help = "Whether to view results on the test data."
    ).orFalse

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

    // val command = Command(
    //   name = "mill -i qfirst.jvm.runVerbAnn",
    //   header = "Spin up the annotation server for QA-SRL Clause frames.") {
    //   (jsDepsPathO, jsPathO, experimentNameO, testOnTestO, domainO, portO).mapN(_run)
    // }

    // command.parse(args) match {
    //   case Left(help) => IO { System.err.println(help); ExitCode.Error }
    //   case Right(main) => main
    // }
    IO.pure(ExitCode.Success)
  }
}
