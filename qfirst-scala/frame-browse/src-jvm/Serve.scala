package qfirst.frame.browse
import qfirst._
import qfirst.frame._
import qfirst.frame.features._
import qfirst.clustering.MergeTree

import qasrl.bank.Data
import qasrl.data.Dataset
import qasrl.labeling.SlotBasedLabel

import cats.~>
import cats.Id
import cats.Order
import cats.data.NonEmptySet
import cats.data.Validated
import cats.effect.IO
import cats.effect.{IOApp, ExitCode}
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.{Encoder, Decoder}

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
import jjm.implicits._

import freelog.EphemeralTreeLogger

object Serve extends CommandIOApp(
  name = "mill -i qfirst.jvm.runVerbAnn",
  header = "Spin up the annotation server for QA-SRL Clause frames.") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val docApiSuffix = "doc"
  val verbApiSuffix = "verb"
  val featureApiSuffix = "feature"

  def getModelFromSpec(spec: ClusterModelSpec): ClusteringModel = {
    ClusteringModel.fromString(spec.specString).get
  }

  def readAllClusterModels[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[ClusterModelSpec, Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    ClusterModelSpec.all.map(getModelFromSpec).traverse {
      case m: JointModel => FrameInductionApp.getVerbFrames(m, features)
          .flatMap(_.get)
      case m: VerbModel => FrameInductionApp.getVerbClusters(m, features)
          .flatMap(_.get)
          .map(clusterings =>
            clusterings.map { case (verbType, verbTree) =>
              verbType -> VerbClusterModel[VerbType, Arg](verbType, verbTree, Clustering(None, Map()))
            }
          )
      case m: ArgumentModel => FrameInductionApp.getArgumentClusters(m, features)
          .flatMap(_.get)
          .map(clusterings =>
            clusterings.map { case (verbType, argClustering) =>
              val allArgIds = argClustering.clusterTreeOpt.foldMap(_.unorderedFold) ++ argClustering.extraClusters.unorderedFold
              val allVerbIds = allArgIds.map(_.verbId)
              val verbTree = MergeTree.Leaf(0.0, allVerbIds)
              val verbClustering = Clustering(Some(verbTree))
              verbType -> VerbClusterModel[VerbType, Arg](verbType, verbClustering, argClustering)
            }
          )
    }.map(models => ClusterModelSpec.all.zip(models).toMap)
  }

  def _runSpecified[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    features: Features[VerbType, Arg],
    pageService: org.http4s.HttpRoutes[IO],
    port: Int)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[ExitCode] = {
    val featureService = HttpUtil.makeHttpPostServer(FeatureService.baseService(features))

    import scala.concurrent.duration._

    for {
      _ <- features.argQuestionDists.get
      allVerbModels <- readAllClusterModels[VerbType, Arg](features)
      verbCounts = allVerbModels.head._2.mapVals(_.numVerbInstances)
      verbModelService = HttpUtil.makeHttpPostServer(
        VerbFrameService.basicIOService(verbCounts, allVerbModels)
      )
      app = Router(
        "/" -> pageService,
        s"/$verbApiSuffix" -> verbModelService,
        s"/$featureApiSuffix" -> featureService
      ).orNotFound
      _ <- Log.info("Starting server.")
      _ <- BlazeServerBuilder[IO](global)
      .withIdleTimeout(5.minutes)
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(app)
      .serve.compile.drain
    } yield ExitCode.Success
  }

  def _run(
    jsDepsPath: Path, jsPath: Path,
    dataSetting: DataSetting,
    mode: RunMode,
    domain: String,
    port: Int,
    behindProxy: Boolean
  ): IO[ExitCode] = {
    freelog.loggers.TimingEphemeralTreeFansiLogger.create().flatMap { implicit Log =>
      val portOpt = if(behindProxy) None else Some(port)
      val useHttps = behindProxy

      val pageService = StaticPageService.makeService(
        domain,
        docApiSuffix, verbApiSuffix, featureApiSuffix,
        dataSetting, mode,
        jsDepsPath, jsPath, portOpt, useHttps
      )

      dataSetting match {
        case d @ DataSetting.Qasrl         => _runSpecified(FrameInductionApp.getFeatures(d, mode), pageService, port)
        case d @ DataSetting.Ontonotes5(_) => _runSpecified(FrameInductionApp.getFeatures(d, mode), pageService, port)
        case d @ DataSetting.CoNLL08(_)    => _runSpecified(FrameInductionApp.getFeatures(d, mode), pageService, port)
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

    val domainO = Opts.option[String](
      "domain", metavar = "domain", help = "domain name the server is being hosted at."
    )

    val portO = Opts.option[Int](
      "port", metavar = "port number", help = "Port to host the HTTP service on."
    )

    val proxyO = Opts.flag(
      "proxy", help = "Whether the server is behind an HTTPS reverse proxy."
    ).orFalse

    // val domainRestrictionO = Opts.option[String](
    //   "domain", metavar = "http://...",
    //   help = "Domain to impose CORS restrictions to (otherwise, all domains allowed)."
    // ).map(NonEmptySet.of(_)).orNone

    (jsDepsPathO, jsPathO, dataO, modeO, domainO, portO, proxyO).mapN(_run)
  }
}
