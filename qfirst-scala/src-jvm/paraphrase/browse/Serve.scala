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

  def _run(
    jsDepsPath: Path, jsPath: Path,
    qasrlBankPath: Path,
    predDir: Path,
    relativeOutDir: String,
    domain: String, port: Int
  ): IO[ExitCode] = {
    IO(Data.readFromQasrlBank(qasrlBankPath).toEither.right.get).flatMap { data =>
      val outDir = predDir.resolve(relativeOutDir)
      val dev = Files.exists(outDir.resolve("dev"))

      val docApiSuffix = "doc"
      val verbApiSuffix = "verb"
      val pageService = StaticPageService.makeService(
        domain,
        docApiSuffix, verbApiSuffix,
        dev,
        jsDepsPath, jsPath, port
      )

      val index = data.index
      val docs = data.documentsById
      val searchIndex = Search.createSearchIndex(docs.values.toList)
      val docService = HttpDocumentService.makeService(index, docs, searchIndex)

      val paraphraseGoldPath = predDir.resolve("gold-paraphrases.json")

      val saveData = (data: ParaphraseAnnotations) => {
        FileUtil.writeJson(paraphraseGoldPath, io.circe.Printer.noSpaces)(data)
      }

      val inputSet = if(dev) data.devExpanded else data.trainExpanded
      val inflectionCounts = Dataset.verbEntries.getAll(inputSet).foldMap(v => Map(v.verbInflectedForms -> 1))

      // TODO include test as well
      val evaluationItemsPath = predDir.resolve("eval-sample-dev.jsonl")

      val predFilename = predDir.resolve("predictions.jsonl")

      for {
        frameInductionResults <- FileUtil.readJson[FrameInductionResults](outDir.resolve("results.json"))
        filter <- {
          import io.circe.generic.auto._
          FileUtil.readJson[SimpleQAs.Filter](predDir.resolve("filter.json"))
        }
        predictions <- {
          import qasrl.data.JsonCodecs._
          import io.circe.generic.auto._
          FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename).map { predSentence =>
            val verbMap = predSentence.verbs.foldMap { verb =>
              val predictedQAs = protocol.filterBeam(filter, verb)
              Vector(verb.verbIndex -> predictedQAs)
            }
            Map(predSentence.sentenceId -> verbMap)
          }.compile.foldMonoid.map(_.map { case (k, vs) => k -> vs.toMap })
        }
        goldParaphrases <- {
          if(!Files.exists(paraphraseGoldPath)) {
            IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
              IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
          } else FileUtil.readJson[EvalApp.ParaphraseAnnotations](paraphraseGoldPath)
        }
        evaluationItems <- {
          import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
          FileUtil.readJsonLines[(InflectedForms, String, Int)](evaluationItemsPath).compile.toList
        }
        goldParaphraseDataRef <- Ref[IO].of(goldParaphrases)
        annotationService = VerbFrameHttpService.make(
          VerbFrameServiceIO(
            inflectionCounts,
            frameInductionResults,
            predictions,
            evaluationItems.apply,
            goldParaphraseDataRef,
            saveData)
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
      "qasrl-gold", metavar = "path", help = "Path to the QA-SRL Bank 2.0 data, e.g., ../qasrl-bank/data/qasrl-v2."
    )

    val predO = Opts.option[Path](
      "qasrl-pred", metavar = "path", help = "Where the predictions and saved info is."
    )

    val outO = Opts.option[String](
      "out", metavar = "path", help = "Relative path to the model output directory."
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
      (jsDepsPathO, jsPathO, qasrlBankO, predO, outO, domainO, portO).mapN(_run(_, _, _, _, _, _, _))
    }

    command.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
