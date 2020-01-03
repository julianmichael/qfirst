package freelog

import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

class FrameTests extends FunSuite with Matchers {

  def showLogTree(tree: LogTree[String]) = tree.cata[String](
    labeled = (label, children) => label + children.map(s =>
      "\n  " + s.replaceAll("\n", "\n  ") // indent whole string and add indented newline
    ).mkString,
    unlabeled = _.mkString("\n")
  )

  import org.scalatest.Inside._
  import org.scalatest.AppendedClues._

  def runTest1[F[_]: Monad](logger: TreeLogger[String, F]) = for {
    _ <- logger.branch("Beginning...") {
      (1 to 5).toList.traverse(i =>
        // List('a', 'b', 'c').traverse
        logger.log(s"Counting $i...")
      )
    }
    _ <- logger.log("Done.")
  } yield ()

  val test1Res = """
    |Beginning...
    |  Counting 1...
    |  Counting 2...
    |  Counting 3...
    |  Counting 4...
    |  Counting 5...
    |Done.
  """.stripMargin.trim

  test("IndentingWriterLogger") {
    runTest1(freelog.loggers.IndentingWriterLogger()).run._1.trim shouldEqual test1Res
  }

  test("TreeWriterLogger") {
    showLogTree(
      runTest1(freelog.loggers.TreeWriterLogger[String]()).run._1
    ).trim shouldEqual test1Res
  }

  val ephLoggerIO = freelog.loggers.RewindingConsoleLineLogger.create(x => IO.unit)

  test("ephemeral logging 1") {
    val runTest = for {
      logger <- ephLoggerIO
      initState <- logger.checkpointState.get
      _ <- logger.save >> logger.log("Test log message.") >> logger.restore >> logger.flush
      endState <- logger.checkpointState.get
    } yield initState shouldEqual endState

    runTest.unsafeRunSync
  }

  test("ephemeral logging 2") {
    val runTest = for {
      logger <- ephLoggerIO
      initState <- logger.checkpointState.get
      _ <- logger.save >> logger.restore >> logger.flush
      endState <- logger.checkpointState.get
    } yield initState shouldEqual endState

    runTest.unsafeRunSync
  }
}
