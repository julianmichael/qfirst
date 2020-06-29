package qfirst.ontonotes

import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import java.nio.file.Paths

import freelog._
import freelog.implicits._

class OntonotesTests extends FunSuite with Matchers {

  implicit val ambLevel = LogLevel.Trace
  implicit val progressSpec = ProgressSpec.simple(50)
  implicit def executionContext = scala.concurrent.ExecutionContext.global
  implicit val timer: Timer[IO] = IO.timer(executionContext)
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

  val location = Paths.get("data/conll-formatted-ontonotes-5.0")

  val service = new CoNLLFileSystemService(location)

  implicit val Log: EphemeralTreeLogger[IO, String] = freelog.loggers.TimingEphemeralTreeFansiLogger.create().unsafeRunSync

  var allPaths: List[CoNLLPath] = null

  test("Get all paths") {
    allPaths = service.getAllPaths.unsafeRunSync
    Log.info(s"${allPaths.size} files").unsafeRunSync
  }

  test("Get some files") {
    allPaths.take(10).infoBarTraverse("Getting some files") { path =>
      Log.info(s"$path") >> service.getFile(path)
    }.unsafeRunSync
  }

  test("Get all sentences") {
    val res = for {
      sentenceCounts <- allPaths.infoBarTraverse("Getting all sentences") { path =>
        Log.info(path.toString) >>
          service.getFile(path).map(f => Map(path.split -> f.sentences.size))
      }.map(_.combineAll)
      _ <- Log.info(s"sentence counts: $sentenceCounts")
      _ <- Log.info(s"Total: ${sentenceCounts.values.sum}")
    } yield ()
    res.unsafeRunSync
  }

  // def showLogTree(tree: LogTree[String]) = tree.cata[String](
  //   labeled = (label, children) => label + children.map(s =>
  //     "\n  " + s.replaceAll("\n", "\n  ") // indent whole string and add indented newline
  //   ).mkString,
  //   unlabeled = _.mkString("\n")
  // )

  // import org.scalatest.Inside._
  // import org.scalatest.AppendedClues._

  // def runTest1[F[_]: Monad](logger: TreeLogger[String, F]) = for {
  //   _ <- logger.branch("Beginning...") {
  //     (1 to 5).toList.traverse(i =>
  //       // List('a', 'b', 'c').traverse
  //       logger.log(s"Counting $i...")
  //     )
  //   }
  //   _ <- logger.log("Done.")
  // } yield ()

  // val test1Res = """
  //   |Beginning...
  //   |  Counting 1...
  //   |  Counting 2...
  //   |  Counting 3...
  //   |  Counting 4...
  //   |  Counting 5...
  //   |Done.
  // """.stripMargin.trim

  // test("IndentingWriterLogger") {
  //   runTest1(freelog.loggers.IndentingWriterLogger()).run._1.trim shouldEqual test1Res
  // }

  // test("TreeWriterLogger") {
  //   showLogTree(
  //     runTest1(freelog.loggers.TreeWriterLogger[String]()).run._1
  //   ).trim shouldEqual test1Res
  // }

  // val ephLoggerIO = freelog.loggers.RewindingConsoleLineLogger.create(x => IO.unit)

  // test("ephemeral logging 1") {
  //   val runTest = for {
  //     logger <- ephLoggerIO
  //     initState <- logger.checkpointState.get
  //     _ <- logger.save >> logger.log("Test log message.") >> logger.restore >> logger.flush
  //     endState <- logger.checkpointState.get
  //   } yield initState shouldEqual endState

  //   runTest.unsafeRunSync
  // }

  // test("ephemeral logging 2") {
  //   val runTest = for {
  //     logger <- ephLoggerIO
  //     initState <- logger.checkpointState.get
  //     _ <- logger.save >> logger.restore >> logger.flush
  //     endState <- logger.checkpointState.get
  //   } yield initState shouldEqual endState

  //   runTest.unsafeRunSync
  // }
}
