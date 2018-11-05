// package qfirst.frames

// import qasrl.bank.Data

// import cats.data.NonEmptySet
// import cats.effect.IO
// import cats.implicits._

// import fs2.{Stream, StreamApp}
// import fs2.StreamApp.ExitCode

// import qasrl.bank.service.DocumentServiceWebServer

// import java.nio.file.Path

// import com.monovore.decline._

// object Serve extends StreamApp[IO] {
//   override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {

//     val qasrlBankO = Opts.option[Path](
//       "qasrl-bank", metavar = "path", help = "Path to the QA-SRL Bank 2.0 data, e.g., ../qasrl-bank/data/qasrl-v2."
//     )

//     val portO = Opts.option[Int](
//       "port", metavar = "port number", help = "Port to host the HTTP service on."
//     )

//     val domainRestrictionO = Opts.option[String](
//       "domain", metavar = "http://...",
//       help = "Domain to impose CORS restrictions to (otherwise, all domains allowed)."
//     ).map(NonEmptySet.of(_)).orNone

//     val command = Command(
//       name = "mill -i qfirst.jvm.runAnnotation",
//       header = "Spin up the annotation server for QA-SRL Clause frames.") {
//       (qasrlBankO, portO, domainRestrictionO).mapN((_, _, _))
//     }

//     val resStreamEither =
//       command.parse(args).flatMap { case (qasrlBankPath, port, domainRestrictionOpt) =>
//         Data.readFromQasrlBank(qasrlBankPath).toEither.left.map(_.toString).map { data =>
//           DocumentServiceWebServer.serve(data.small, port, domainRestrictionOpt)
//         }
//       }

//     resStreamEither.left.map(message =>
//       Stream.eval(IO { System.err.println(message); ExitCode.Error })
//     ).merge
//   }
// }
