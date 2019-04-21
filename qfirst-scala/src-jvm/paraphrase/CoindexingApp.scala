package qfirst.paraphrase

import qfirst._
import qfirst.frames.implicits._

import cats.Id
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

object CoindexingApp extends IOApp {

  import ClauseResolution.ArgStructure

  type ClausalQ = (ArgStructure, ArgumentSlot)

  @JsonCodec case class ClauseQAQuestion(
    clause: String,
    slot: String
  ) {
    val clauseTemplate = io.circe.parser.decode[ArgStructure](clause).right.get
    val answerSlot = ArgumentSlot.fromString(slot).get
    def clausalQ = (clauseTemplate, answerSlot)
  }

  def adjacencyProb(
    x: List[(AnswerSpan, Double)],
    y: List[(AnswerSpan, Double)]
  ): Double = {
    val xMap = x.toMap
    val yMap = y.toMap
    xMap.keySet.intersect(yMap.keySet).iterator.map { span =>
      xMap(span) * yMap(span)
    }.sum
  }

  @JsonCodec case class ClauseQAOutput(
    question: ClauseQAQuestion,
    spans: List[(AnswerSpan, Double)]
  )

  @JsonCodec case class SentenceQAOutput(
    sentenceId: String,
    verbs: Map[String, List[ClauseQAOutput]]
  )

  def coindexAllFrames(
    dataset: Dataset,
    frameInductionResults: FrameInductionResults,
    qaOutputs: Map[String, SentenceQAOutput]
  ): FrameInductionResults = {
    val result = frameInductionResults.copy(
      frames = frameInductionResults.frames.map { case (verbForms, frameset) =>
        val verbAssignments = frameInductionResults.assignments(verbForms)
        verbForms -> frameset.copy(
          frames = frameset.frames.zipWithIndex.map { case (frame, frameIndex) =>
            val clauseTemplateSet = frame.clauseTemplates.map(_.args).toSet
            val frameQAOutputs: List[List[ClauseQAOutput]] =
              verbAssignments.toList.flatMap { case (sentenceId, sentenceAssignments) =>
                val sentenceQAs = qaOutputs(sentenceId)
                // println(qaOutputs)
                // println(sentenceQAs)
                sentenceAssignments.toList.flatMap { case (verbIndex, frameDist) =>
                  // println(sentenceQAs.verbs(verbIndex.toString))
                  if(frameDist(frameIndex) != frameDist.max) None else {
                    sentenceQAs.verbs.get(verbIndex.toString) // TODO why is it sometimes not present?
                  }
                }
              }
            val adjacencyPseudoCounts = frameQAOutputs.foldLeft(
              Map.empty[(ClausalQ, ClausalQ), (Double, Int)]
            ) { case (adjCounts, qaOutputs) =>
                qaOutputs.tails.toList.foldLeft(adjCounts) {
                  case (counts, Nil) => counts
                  case (counts, headQA :: otherQAs) =>
                    otherQAs.foldLeft(counts) {
                      case (cs, otherQA) =>
                        val (curProb, curCounts) = cs.get(headQA.question.clausalQ -> otherQA.question.clausalQ).getOrElse(0.0 -> 0)
                        val totalAdjProb = curProb + adjacencyProb(headQA.spans, otherQA.spans)
                        cs + ((headQA.question.clausalQ, otherQA.question.clausalQ) -> (totalAdjProb -> (curCounts + 1)))
                    }
                }
            }
            val symmetricAvgAdjacencyProbs = adjacencyPseudoCounts.flatMap { case (pair, (pcount, numTotal)) =>
              val (otherPcount, otherTotal) = adjacencyPseudoCounts.get(pair.swap).getOrElse(0.0 -> 0)
              val avg = (pcount + otherPcount) / (numTotal + otherTotal)
              if(avg > 0.25) List(pair -> avg, pair.swap -> avg)
              else Nil
            }
            frame.copy(coindexingScores = symmetricAvgAdjacencyProbs)
          }
        )
      })
    System.gc()
    result
  }

  // lazy val trainOrig = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz"))
  lazy val trainExpanded = Data.readDataset(Paths.get("qasrl-v2_1").resolve("expanded").resolve("train.jsonl.gz"))
  lazy val devDense = Data.readDataset(Paths.get("qasrl-v2_1").resolve("dense").resolve("dev.jsonl.gz"))
  lazy val dev = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("dev.jsonl.gz"))
  // lazy val devMini = Data.readDataset(Paths.get("dev-mini.jsonl.gz"))
  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def run(args: List[String]): IO[ExitCode] = {
    val outDir = Paths.get(s"predictions/afirst/${args(0)}")
    val dataset = if(Files.exists(outDir.resolve("dev"))) dev |+| devDense else trainExpanded |+| devDense
    val frameInductionResultsPath = outDir.resolve("results.json")
    val qaOutputPath = outDir.resolve("results-qa-output.jsonl.gz")
    val outPath = outDir.resolve("results-coindexed.json")
    for {
      frameInductionResults <- FileUtil.readJson[FrameInductionResults](frameInductionResultsPath)
      qaOutput <- FileUtil.readJsonLines[SentenceQAOutput](qaOutputPath).map(output => output.sentenceId -> output).compile.toList
      _ <- FileUtil.writeJson(outPath, io.circe.Printer.noSpaces)(coindexAllFrames(dataset, frameInductionResults, qaOutput.toMap))
    } yield ExitCode.Success
  }
}
