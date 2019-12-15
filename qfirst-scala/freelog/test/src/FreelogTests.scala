package freelog

import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

class FrameTests extends FunSuite with Matchers {

  import org.scalatest.Inside._
  import org.scalatest.AppendedClues._

  val writerLogger = Loggers.IndentingWriterLogger()

  val test1 = for {
    _ <- writerLogger.log(
      "Beginning...", (1 to 5).toList.traverse(i =>
        // List('a', 'b', 'c').traverse
        writerLogger.log(s"Counting $i...")
      )
    )
  } yield ()

  val test1Res = """
    |Beginning...
    |  Counting 1...
    |  Counting 2...
    |  Counting 3...
    |  Counting 4...
    |  Counting 5...
  """.trim.stripMargin + "\n  "

  test("logging sandbox") {
    test1.run._1 shouldEqual test1Res
  }

  val ephLoggerIO = Loggers.EphemeralConsoleLogger.create(x => IO.unit)

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
