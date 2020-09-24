package qfirst.frame.browse
import qfirst._
import qfirst.frame._
import qfirst.frame.features.GoldQasrlFeatures
import qfirst.model.eval.protocols.SimpleQAs

import qasrl.bank.Data
import qasrl.data.Dataset
import qasrl.labeling.SlotBasedLabel

import cats.~>
import cats.Id
import cats.data.NonEmptySet
import cats.data.Validated
import cats.effect.IO
import cats.effect.{IOApp, ExitCode}
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import qasrl.bank.service.DocumentService
import qasrl.bank.service.Search

import java.nio.file.Path
import java.nio.file.Files

import com.monovore.decline._
import com.monovore.decline.effect._

import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

import jjm.io.FileUtil
import jjm.io.HttpUtil
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

object Serve extends CommandIOApp(
  name = "mill -i qfirst.jvm.runVerbAnn",
  header = "Spin up the annotation server for QA-SRL Clause frames.") {

  import scala.concurrent.ExecutionContext.Implicits.global

  // val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)

  def _run(
    jsDepsPath: Path, jsPath: Path,
    data: FrameInductionApp.DataSetting,
    mode: RunMode,
    model: JointModel,
    domain: String,
    port: Int
  ): IO[ExitCode] = {
    freelog.loggers.TimingEphemeralTreeFansiLogger.create().flatMap { implicit Log =>
      // val features = data.getFeatures(mode)
      val features = new GoldQasrlFeatures(mode)

      val docApiSuffix = "doc"
      val verbApiSuffix = "verb"
      val featureApiSuffix = "feature"

      val pageService = StaticPageService.makeService(
        domain,
        docApiSuffix, verbApiSuffix, featureApiSuffix,
        mode,
        jsDepsPath, jsPath, port
      )

      features.qasrlBank.get.flatMap { data =>
        val index = data.index
        val docs = data.documentsById
        val searchIndex = Search.createSearchIndex(docs.values.toList)
        val docService = HttpUtil.makeHttpPostServer(
          DocumentService.basic(index, docs, searchIndex)
            .andThenK(Lambda[Id ~> IO](IO.pure(_)))
        )
        val featureService = HttpUtil.makeHttpPostServer(FeatureService.baseService(features))

        for {
          dataset <- features.dataset.get
          inflectionCounts = Dataset.verbEntries.getAll(dataset).foldMap(v => Map(v.verbInflectedForms -> 1))
          verbModels <- FrameInductionApp.getVerbFrames[InflectedForms, ClausalQuestion](
            model, features
          ).flatMap(_.read.map(_.get))
          goldParaphrases <- features.readGoldParaphrases
          evaluationItems <- features.evaluationItems.get
          goldParaphraseDataRef <- Ref[IO].of(goldParaphrases)
          annotationService = HttpUtil.makeHttpPostServer(
            VerbFrameService.basicIOService(
              inflectionCounts,
              verbModels,
              dataset,
              evaluationItems.apply,
              goldParaphraseDataRef,
              features.saveGoldParaphrases(_)
            )
          )
          app = Router(
            "/" -> pageService,
            s"/$docApiSuffix" -> docService,
            s"/$verbApiSuffix" -> annotationService,
            s"/$featureApiSuffix" -> featureService
          ).orNotFound
          _ <- Log.info("Starting server.")
          _ <- BlazeServerBuilder[IO]
          .bindHttp(port, "0.0.0.0")
          .withHttpApp(app)
          .serve.compile.drain
        } yield ExitCode.Success
      }
    }
  }

  def main: Opts[IO[ExitCode]] = {

    val jsDepsPathO = Opts.option[Path](
      "jsDeps", metavar = "path", help = "Where to get the JS deps file."
    )

    val jsPathO = Opts.option[Path](
      "js", metavar = "path", help = "Where to get the JS main file."
    )

    val dataO = FrameInductionApp.dataO

    val modeO = FrameInductionApp.modeO

    val modelO = FrameInductionApp.modelO.mapValidated {
      case m: JointModel => Validated.valid(m)
      case other => Validated.invalidNel(s"Invalid model $other: must be a joint verb frame model for browsing.")
    }

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

    (jsDepsPathO, jsPathO, dataO, modeO, modelO, domainO, portO).mapN(_run)
  }
}
