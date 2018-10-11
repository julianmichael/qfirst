package qfirst.learn
import qfirst._
import qfirst.metrics._

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.VerbEntry

import HasMetrics.ops._

object LearnApp {

  def program(qasrlBankPath: NIOPath) = {
    // TODO validate using opts stuff from decline?
    val trainData = Data.readDataset(qasrlBankPath.resolve("orig").resolve("train.jsonl.gz"))
  }

  val runLearn = Command(
    name = "mill qfirst.runLearn",
    header = "Do simple structure learning on QA-SRL."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )

    (goldPath).map(program)
  }

  def main(args: Array[String]): Unit = {
    val result = runLearn.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(main) => IO { main }
    }
    result.unsafeRunSync
  }
}
