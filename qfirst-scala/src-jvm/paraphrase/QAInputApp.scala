package qfirst.paraphrase

import qfirst._
import qfirst.frames.implicits._

import cats.Applicative
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

object QAInputApp extends IOApp {

  import io.circe.Json
  import io.circe.syntax._

  import fs2.Stream

  def getJsonInputsForQA[F[_]: Applicative](dataset: Dataset): Stream[F, Json] = {
    val verbToClauseTemplates = dataset.sentences.values.toList.foldMap { sentence =>
      sentence.verbEntries.values.toList.foldMap { verb =>
        val structures: Set[ArgStructure] = ClauseResolution.getResolvedStructures(
          filterGoldNonDense(verb)._2.values.toList.map(_.questionSlots)
        ).map(_._1).toSet
        Map(verb.verbInflectedForms -> structures)
      }
    }
    dataset.sentences.toList
      .foldMap(x => Stream.eval(Applicative[F].pure(x)))
      .map { case (sentenceId, sentence) =>
      Json.obj(
        "sentenceId" -> sentenceId.asJson,
        "sentenceTokens" -> sentence.sentenceTokens.asJson,
        "verbs" -> sentence.verbEntries.values.toList.map { verb =>
          val clauses = verbToClauseTemplates(verb.verbInflectedForms).toList.flatMap { clauseTemplate =>
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
    }
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

  import qfirst.metrics.ExpectedCount

  def getUnsymmetrizedFuzzyArgumentEquivalences(
    dataset: Dataset,
    framesets: Map[InflectedForms, VerbFrameset],
    sentenceQAs: SentenceQAOutput,
    hardAssignments: Boolean
  ): Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]] = {
    val sentence = dataset.sentences(sentenceQAs.sentenceId)
    sentenceQAs.verbs.toList.foldMap { case (verbIndexStr, verbQAs) =>
      val verbIndex = verbIndexStr.toInt
      val verbForms = sentence.verbEntries(verbIndexStr.toInt).verbInflectedForms
      val frameset = framesets(verbForms)
      val verbId = VerbId(sentenceQAs.sentenceId, verbIndex)
      val frameProbs = frameset.instances(verbId)
      val maxFrameProb = frameProbs.max

      (frameset.frames, frameset.frames.indices, frameProbs).zipped.flatMap {
        case (frame, frameIndex, frameProb) =>
          if(hardAssignments && frameProb < maxFrameProb) None else Some {
            val adjacencyExpectations = verbQAs.tails.toList.flatMap {
              case Nil => Nil
              case fst :: tail => tail.map { snd =>
                val adjProb = adjacencyProb(fst.spans, snd.spans)
                val expectedCount = if(hardAssignments) {
                  ExpectedCount(adjProb, 1.0)
                } else {
                  ExpectedCount(adjProb * frameProb, frameProb)
                }
                val qPair = fst.question.clausalQ -> snd.question.clausalQ
                Map(qPair -> expectedCount)
              }
            }.combineAll
            Map(verbForms -> Map(frameIndex -> adjacencyExpectations))
          }
      }.toList.combineAll
    }
  }

  def symmetrizeArgumentEquivalences(
    equivalences: Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]]
  ): Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]] = {
    equivalences.transform { case (_, frameEqs) =>
      frameEqs.transform { case (_, equivalence) =>
        equivalence.transform { case (qPair, ec) =>
          ec |+| equivalence.get(qPair.swap).combineAll
        }
      }
    }
  }

  def getFuzzyArgumentEquivalences(
    dataset: Dataset,
    framesets: Map[InflectedForms, VerbFrameset],
    allSentenceQAs: Stream[IO, SentenceQAOutput],
    hardAssignments: Boolean = false
  ): IO[Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]]] = {
    allSentenceQAs.map { sentenceQAs =>
      getUnsymmetrizedFuzzyArgumentEquivalences(
        dataset, framesets, sentenceQAs, hardAssignments
      )
    }.compile.foldMonoid.map(symmetrizeArgumentEquivalences)
  }

  // def getFuzzyArgumentEquivalences(
  //   dataset: Dataset,
  //   frameset: VerbFrameset,
  //   qaOutputs: Map[String, SentenceQAOutput]
  // ): List[Map[(ClausalQ, ClausalQ), Double]] = {
  //   val verbForms = frameset.inflectedForms
  //   frameset.frames.zipWithIndex.map { case (frame, frameIndex) =>
  //     val clauseTemplateSet = frame.clauseTemplates.map(_.args).toSet
  //     val adjacencyInstances = frameset.instances.toList.foldMap {
  //       case (VerbId(sentenceId, verbIndex), frameDist) =>
  //         if(frameDist(frameIndex) != frameDist.max) Vector() else {
  //           val verbQAs = qaOutputs(sentenceId).verbs.getOrElse(verbIndex.toString, Vector())
  //           val adjacencyPcounts = verbQAs.tails.flatMap {
  //             case Nil => Nil
  //             case fst :: tail => tail.map { snd =>
  //               (fst.question.clausalQ -> snd.question.clausalQ, adjacencyProb(fst.spans, snd.spans))
  //             }
  //           }.toVector
  //           adjacencyPcounts
  //         }
  //     }
  //     val instancesByQpair = adjacencyInstances.groupBy(_._1)
  //     val instancesByUnorderedQpair = instancesByQpair.transform { case (qpair, instances) =>
  //       (instances ++ instancesByQpair.getOrElse(qpair.swap, Vector())).map(_._2)
  //     }
  //     val probsByUnorderedQpair = instancesByUnorderedQpair.map { case (qpair, instances) =>
  //       qpair -> (instances.sum / instances.size)
  //     }
  //     probsByUnorderedQpair
  //   }
  // }

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def program(goldPath: NIOPath, outDir: NIOPath, test: Boolean): IO[ExitCode] = for {
    inputSet <- logOp("Reading QA-SRL expanded/train", readDataset(goldPath.resolve("expanded/train.jsonl.gz")))
    evalSet <- {
      if(test) logOp("Reading QA-SRL orig/test", readDataset(goldPath.resolve("orig/test.jsonl.gz")))
      else logOp("Reading QA-SRL orig/dev", readDataset(goldPath.resolve("orig/dev.jsonl.gz")))
    }
    fullDataset <- logOp("Constructing full dataset", inputSet |+| evalSet)
    _ <- logOp(
      "Writing QA input file",
      FileUtil.writeJsonLinesStreaming(
        outDir.resolve(if(test) "qa-input-test.jsonl.gz" else "qa-input-dev.jsonl.gz"), io.circe.Printer.noSpaces)(
        getJsonInputsForQA[IO](fullDataset))
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
    val test = Opts.flag(
      "test", help = "Whether to use test data for QA inputs"
    ).orFalse

    (goldPath, outDir, test).mapN(program)
  }

  def run(args: List[String]): IO[ExitCode] = {
    runQAInput.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
