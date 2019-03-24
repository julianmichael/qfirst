package qfirst

import qfirst._
import qfirst.frames.implicits._

import cats._
import cats.effect._
import cats.implicits._

import java.nio.file._

import qasrl._
import qasrl.data._
import qasrl.data.JsonCodecs._
import qasrl.labeling._
import qasrl.util._
import qasrl.util.implicits._

import qasrl.bank._

import nlpdata.datasets.wiktionary._
import nlpdata.util.LowerCaseStrings._

import io.circe.Json
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import fs2.Stream

object ClauseResolutionApp extends IOApp {

  import ClauseResolution._

  val paths = List(
    Paths.get("qasrl-v2_1").resolve("expanded").resolve("train.jsonl.gz"),
    Paths.get("qasrl-v2_1").resolve("expanded").resolve("dev.jsonl.gz")
  )

  def processSentence(sentence: Sentence): String = {
    val printer = io.circe.Printer.noSpaces
    printer.pretty(
      Json.obj(
        "sentenceId" -> sentence.sentenceId.asJson,
        "verbs" -> Json.obj(
          sentence.verbEntries.values.toList.map { verb =>
            val qLabels = filterGoldNonDense(verb)._2.toList.map(_._2)
            val framePairs = getResolvedFramePairs(verb.verbInflectedForms, qLabels)
            verb.verbIndex.toString -> Json.obj(
              qLabels.zip(framePairs).map { case (qLabel, (frame, slot)) =>
                qLabel.questionString -> Json.obj(
                  "clause" -> printer.pretty(getClauseTemplate(frame).asJson).asJson,
                  "slot" -> ArgumentSlot.toString(slot).asJson
                )
              }: _*
            )
          }: _*
        )
      )
    )
  }

  def run(args: List[String]): IO[ExitCode] = {
    val outPath = Paths.get(args(0))
    Stream.resource(FileUtil.blockingExecutionContext).flatMap { _ec =>
      Stream.emits[IO, Path](paths)
        .flatMap(path => FileUtil.streamJsonLines[Sentence](path, _ec))
        .map(processSentence)
        .intersperse("\n")
        .through(fs2.text.utf8Encode)
        .through(fs2.io.file.writeAll(outPath, _ec))
    }.compile.drain.as(ExitCode.Success)
  }
}
