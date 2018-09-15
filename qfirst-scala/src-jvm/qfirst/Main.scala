package qfirst

import cats.implicits._
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.Path

object Main {

  val runMetrics = Opts.subcommand("metrics", help = "Calculate metrics.") {
    val goldPath = Opts.option[Path](
      "gold", metavar = "path", help = "Path to the gold data file."
    )
    val predPath = Opts.option[Path](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )

    (goldPath, predPath).mapN(Run.main)
  }

  val runConsolidate = Opts.subcommand("consolidate", help = "Consolidate prediction file into nice data.") {
    val predPath = Opts.option[Path](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )
    predPath.map(_ => ()) // XXX Consolidate.main
  }

  val runMain = Command(
    name = "mill qfirst.run",
    header = "Run metrics-related utilities."
  )(runMetrics orElse runConsolidate)

  def main(args: Array[String]): Unit = {
    val result = runMain.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(main) => IO { main }
    }
    result.unsafeRunSync
  }

}
