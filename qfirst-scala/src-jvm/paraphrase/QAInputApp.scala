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

import java.nio.file.{Path => NIOPath}
import com.monovore.decline._

import ClauseResolution.ArgStructure

object CoindexingApp extends IOApp {

  import io.circe.Json
  import io.circe.syntax._

  def getJsonInputsForQA(dataset: Dataset): List[Json] = {
    val verbToClauseTemplates = dataset.sentences.values.toList.foldMap { sentence =>
      sentence.verbEntries.values.toList.foldMap { verb =>
        val structures: Set[ArgStructure] = ClauseResolution.getResolvedStructures(
          verb.questionLabels.values.toList.map(_.questionSlots)
        ).map(_._1).toSet
        Map(verb.verbInflectedForms -> structures)
      }
    }
    dataset.sentences.iterator.map { case (sentenceId, sentence) =>
      Json.obj(
        "sentenceId" -> sentenceId.asJson,
        "sentenceTokens" -> sentence.sentenceTokens.asJson,
        "verbs" -> sentence.verbEntries.values.toList.map { verb =>
          val clauses = verbToClauseTemplates(verb.verbInflectedForms).toList.map { clauseTemplate =>
            val clauseTemplateString = io.circe.Printer.noSpaces.pretty(clauseTemplate.asJson)
            val argSlots = clauseTemplate.args.keys.toList.map(ArgumentSlot.toString)
            argSlots.map(slot =>
              Json.obj(
                "clause" -> clauseTemplateString.asJson,
                "slot" -> slot.asJson
              )
            )
          }.toList
          Json.obj(
            "verbIndex" -> verb.verbIndex.asJson,
            "clauses" -> clauses.asJson
          )
        }.asJson
      )
    }.toList
  }

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

  def getPerFrameFuzzyEquivalences(
    dataset: Dataset,
    frameInductionResults: FrameInductionResults,
    qaOutputs: Map[String, SentenceQAOutput]
  ): Map[InflectedForms, List[Map[(ClausalQ, ClausalQ), Double]]] = {
    frameInductionResults.frames.transform { case (verbForms, frameset) =>
      val verbAssignments = frameInductionResults.assignments(verbForms)
      frameset.frames.zipWithIndex.map { case (frame, frameIndex) =>
        val clauseTemplateSet = frame.clauseTemplates.map(_.args).toSet
        val adjacencyInstances = verbAssignments.toList.foldMap { case (sentenceId, sentenceAssignments) =>
          val sentenceQAs = qaOutputs(sentenceId)
          sentenceAssignments.toList.foldMap { case (verbIndex, frameDist) =>
            if(frameDist(frameIndex) != frameDist.max) Vector() else {
              val verbQAs = sentenceQAs.verbs.getOrElse(verbIndex.toString, Vector())
              val adjacencyPcounts = verbQAs.tails.flatMap {
                case Nil => Nil
                case fst :: tail => tail.map { snd =>
                  (fst.question.clausalQ -> snd.question.clausalQ, adjacencyProb(fst.spans, snd.spans))
                }
              }.toVector
              adjacencyPcounts
            }
          }
        }

        val instancesByQpair = adjacencyInstances.groupBy(_._1)
        val instancesByUnorderedQpair = instancesByQpair.transform { case (qpair, instances) =>
          (instances ++ instancesByQpair.getOrElse(qpair.swap, Vector())).map(_._2)
        }
        val probsByUnorderedQpair = instancesByUnorderedQpair.map { case (qpair, instances) =>
          qpair -> (instances.sum / instances.size)
        }
        probsByUnorderedQpair
      }
    }
  }

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def program(goldPath: NIOPath, outDir: NIOPath): IO[ExitCode] = for {
    trainExpanded <- logOp("Reading QA-SRL expanded/train", readDataset(goldPath.resolve("expanded/train.jsonl.gz")))
    devExpanded <- logOp("Reading QA-SRL expanded/dev", readDataset(goldPath.resolve("expanded/dev.jsonl.gz")))
    testOrig <- logOp("Reading QA-SRL orig/test", readDataset(goldPath.resolve("orig/test.jsonl.gz")))
    devDense <- logOp("Reading QA-SRL dense/dev", readDataset(goldPath.resolve("dense/dev.jsonl.gz")))
    testDense <- logOp("Reading QA-SRL dense/test", readDataset(goldPath.resolve("dense/test.jsonl.gz")))
    fullDataset <- logOp("Constructing full dataset", trainExpanded |+| devExpanded |+| testOrig |+| devDense |+| testDense)
    _ <- logOp(
      "Writing QA input file",
      FileUtil.writeJson(outDir.resolve("qa-input.jsonl.gz"), io.circe.Printer.noSpaces)(getJsonInputsForQA(fullDataset))
    )
  } yield ExitCode.Success

  val runQAInput = Command(
    name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.QAInputApp",
    header = "Write the QA input file for similarity-scoring slots."
  ) {
    val goldPath = Opts.option[NIOPath](
      "qasrl-gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val outDir = Opts.option[NIOPath](
      "out", metavar = "path", help = "Relative path to the output directory."
    )

    (goldPath, outDir).mapN(program)
  }

  def run(args: List[String]): IO[ExitCode] = {
    runQAInput.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
