// package qfirst.paraphrase.browse
// import qfirst._
// import qfirst.paraphrase._
// import qfirst.protocols.SimpleQAs
// 
// import qasrl.bank.Data
// import qasrl.data.Dataset
// import qasrl.labeling.SlotBasedLabel
// 
// import cats.data.NonEmptySet
// import cats.data.Validated
// import cats.effect.IO
// import cats.effect.{IOApp, ExitCode}
// import cats.effect.concurrent.Ref
// import cats.implicits._
// 
// import fs2.Stream
// 
// import qasrl.bank.service.HttpDocumentService
// import qasrl.bank.service.DocumentServiceWebServer
// import qasrl.bank.service.Search
// 
// import java.nio.file.Path
// import java.nio.file.Files
// 
// import com.monovore.decline._
// import com.monovore.decline.effect._
// 
// import org.http4s.server.Router
// import org.http4s.server.blaze.BlazeServerBuilder
// import org.http4s.implicits._
// 
// import jjm.io.FileUtil
// import jjm.ling.en.InflectedForms
// import jjm.ling.en.VerbForm
// 
// import EvalApp.ParaphraseAnnotations
// import EvalApp.QABeam
// 
// object Serve extends CommandIOApp(
//   name = "mill -i qfirst.jvm.runVerbAnn",
//   header = "Spin up the annotation server for QA-SRL Clause frames.") {
// 
//   import scala.concurrent.ExecutionContext.Implicits.global
// 
//   def logOp[A](msg: String, op: IO[A]): IO[A] =
//     IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))
// 
//   def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))
// 
//   // val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)
// 
//   def _run(
//     jsDepsPath: Path, jsPath: Path,
//     config: Config,
//     verbSenseConfig: VerbSenseConfig,
//     domain: String,
//     port: Int
//   ): IO[ExitCode] = config.qasrlBank.get.flatMap { data =>
//     val docApiSuffix = "doc"
//     val verbApiSuffix = "verb"
//     val pageService = StaticPageService.makeService(
//       domain,
//       docApiSuffix, verbApiSuffix,
//       config.mode,
//       jsDepsPath, jsPath, port
//     )
// 
//     val index = data.index
//     val docs = data.documentsById
//     val searchIndex = Search.createSearchIndex(docs.values.toList)
//     val docService = HttpDocumentService.makeService(index, docs, searchIndex)
// 
//     for {
//       fullSet <- config.full.get
//       inflectionCounts = Dataset.verbEntries.getAll(fullSet).foldMap(v => Map(v.verbInflectedForms -> 1))
//       verbModels <- config.getCachedVerbModels(verbSenseConfig).map(_.get)
//       goldParaphrases <- config.readGoldParaphrases
//       evaluationItems <- config.evaluationItems.get
//       goldParaphraseDataRef <- Ref[IO].of(goldParaphrases)
//       // TODO make service automatically from basic version
//       annotationService = VerbFrameHttpService.make(
//         VerbFrameServiceIO(
//           inflectionCounts,
//           verbModels,
//           fullSet,
//           evaluationItems.apply,
//           goldParaphraseDataRef,
//           config.saveGoldParaphrases(_))
//       )
//       app = Router(
//         "/" -> pageService,
//         s"/$docApiSuffix" -> docService,
//         s"/$verbApiSuffix" -> annotationService,
//         ).orNotFound
//       _ <- BlazeServerBuilder[IO]
//       .bindHttp(port, "0.0.0.0")
//       .withHttpApp(app)
//       .serve.compile.drain
//     } yield ExitCode.Success
//   }
// 
//   def main: Opts[IO[ExitCode]] = {
// 
//     val jsDepsPathO = Opts.option[Path](
//       "jsDeps", metavar = "path", help = "Where to get the JS deps file."
//     )
// 
//     val jsPathO = Opts.option[Path](
//       "js", metavar = "path", help = "Where to get the JS main file."
//     )
// 
//     val configO = Opts.option[String](
//       "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
//     ).mapValidated { string =>
//       RunMode.fromString(string)
//         .map(Validated.valid)
//         .getOrElse(Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test."))
//         .map(Config(_))
//     }
//     val verbSenseConfigO = Opts.option[String](
//       "model", metavar = "entropy|elmo|<float>", help = "Verb sense model configuration."
//     ).mapValidated { string =>
//       VerbSenseConfig.fromString(string)
//         .map(Validated.valid)
//         .getOrElse(Validated.invalidNel(s"Invalid model $string: must be entropy, elmo, or a float (interpolation param)."))
//     }
// 
//     val domainO = Opts.option[String](
//       "domain", metavar = "domain", help = "domain name the server is being hosted at."
//     )
// 
//     val portO = Opts.option[Int](
//       "port", metavar = "port number", help = "Port to host the HTTP service on."
//     )
// 
//     // val domainRestrictionO = Opts.option[String](
//     //   "domain", metavar = "http://...",
//     //   help = "Domain to impose CORS restrictions to (otherwise, all domains allowed)."
//     // ).map(NonEmptySet.of(_)).orNone
// 
//     (jsDepsPathO, jsPathO, configO, verbSenseConfigO, domainO, portO).mapN(_run)
//   }
// }
