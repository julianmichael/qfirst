package qfirst.frame

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution
import qfirst.model.eval.filterGoldNonDense

import cats.Applicative
import cats.Id
import cats.effect._
import cats.implicits._

import java.nio.file._

import qasrl._
import qasrl.data._
import qasrl.labeling._

import qasrl.bank._

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.implicits._
import jjm.io.FileUtil

import io.circe.Json
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import java.nio.file.{Path => NIOPath}
import com.monovore.decline._
import com.monovore.decline.effect._

import freelog._
import freelog.implicits._

object QAInputApp extends CommandIOApp(
  name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.QAInputApp",
  header = "Write the QA input file for similarity-scoring slots.") {

  // implicit val logLevel = LogLevel.Debug

  import io.circe.Json
  import io.circe.syntax._

  import fs2.Stream

  type ClausalQ = (ArgStructure, ArgumentSlot)

  @JsonCodec case class ClauseQAQuestion(
    clause: String,
    slot: String
  ) {
    val clauseTemplate = io.circe.parser.decode[ArgStructure](clause).right.get
    if(clauseTemplate.forgetAnimacy != clauseTemplate) {
      println("AHHH! " + clauseTemplate)
        ???
    }
    val answerSlot = ArgumentSlot.fromString(slot).get
    def clausalQ = (clauseTemplate, answerSlot)
  }

  def adjacencyProb(
    x: List[(ESpan, Double)],
    y: List[(ESpan, Double)]
  ): Double = {
    val xMap = x.toMap
    val yMap = y.toMap
    xMap.keySet.intersect(yMap.keySet).iterator.map { span =>
      xMap(span) * yMap(span)
    }.sum
  }

  @JsonCodec case class ClauseQAOutput(
    question: ClauseQAQuestion,
    spans: List[(ESpan, Double)]
  )

  @JsonCodec case class SentenceQAOutput(
    sentenceId: String,
    verbs: Map[String, List[ClauseQAOutput]]
  )


  def getJsonInputsForQA[F[_]: Applicative](dataset: Dataset): Stream[F, Json] = {
    val verbToQuestionTemplates = dataset.sentences.values.toList.foldMap { sentence =>
      sentence.verbEntries.values.toList.foldMap { verb =>
        val templateQs: Set[(ArgStructure, ArgumentSlot)] = ClauseResolution.getResolvedStructures(
          filterGoldNonDense(verb)._2.values.toList.map(_.questionSlots)
        ).toSet
        Map(verb.verbInflectedForms -> templateQs)
      }
    }
    dataset.sentences.toList
      .foldMap(x => Stream.eval(Applicative[F].pure(x)))
      .map { case (sentenceId, sentence) =>
      Json.obj(
        "sentenceId" -> sentenceId.asJson,
        "sentenceTokens" -> sentence.sentenceTokens.asJson,
        "verbs" -> sentence.verbEntries.values.toList.map { verb =>
          val questions = verbToQuestionTemplates(verb.verbInflectedForms).toList.map { case (clauseTemplate, slot) =>
            val clauseTemplateString = io.circe.Printer.noSpaces.pretty(clauseTemplate.asJson)
            Json.obj(
              "clause" -> clauseTemplateString.asJson,
              "slot" -> slot.asJson
            )
            // val argSlots = getArgumentSlotsForClauseTemplate(clauseTemplate).map(ArgumentSlot.toString)
            // argSlots.map(slot =>
            //   Json.obj(
            //     "clause" -> clauseTemplateString.asJson,
            //     "slot" -> slot.asJson
            //   )
            // )
          }.toList
          Json.obj(
            "verbIndex" -> verb.verbIndex.asJson,
            "clauses" -> questions.asJson
          )
        }.asJson
      )
    }
  }

  def writeQAInputDataForDataset(
    outDir: NIOPath, splitName: String, dataset: Dataset)(
    implicit Log: TreeLogger[IO, String]
  ): IO[Unit] = {
    val outPath = outDir.resolve(s"qa-input-$splitName.jsonl.gz")
    Log.infoBranch(s"Writing QA-SRL $splitName QA input data to $outPath")(
      FileUtil.writeJsonLinesStreaming(
        outPath, io.circe.Printer.noSpaces
      )(getJsonInputsForQA[IO](dataset))
    )
  }

  def writeQAInputDataForDataSplit(
    goldPath: NIOPath, outDir: NIOPath, splitPath: String, splitName: String)(
    implicit Log: TreeLogger[IO, String]
  ): IO[Unit] = {
    Log.infoBranch(s"Reading QA-SRL $splitPath")(
      readDataset(goldPath.resolve(s"$splitPath.jsonl.gz"))
    ) >>= (dataset =>
      writeQAInputDataForDataset(outDir, splitName, dataset)
    )
  }

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def program(goldPath: NIOPath, outDir: NIOPath): IO[ExitCode] = for {
    implicit0(logger: EphemeralTreeLogger[IO, String]) <- freelog.loggers.EphemeralTreeConsoleLogger.create()
    _ <- writeQAInputDataForDataSplit(goldPath, outDir, "expanded/train", "train")
    _ <- writeQAInputDataForDataSplit(goldPath, outDir, "expanded/dev", "dev")
    _ <- writeQAInputDataForDataSplit(goldPath, outDir, "orig/test", "test")
  } yield ExitCode.Success

  def main: Opts[IO[ExitCode]] = {
    val goldPath = Opts.option[NIOPath](
      "qasrl-gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val outDir = Opts.option[NIOPath](
      "out", metavar = "path", help = "Relative path to the output directory."
    )

    (goldPath, outDir).mapN(program)
  }
}


  // XXXXXXXXX

  // import qfirst.metrics.ExpectedCount

  // def getUnsymmetrizedFuzzyArgumentEquivalences(
  //   dataset: Dataset,
  //   framesets: Map[InflectedForms, VerbFrameset],
  //   sentenceQAs: SentenceQAOutput,
  // ): Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]] = {
  //   dataset.sentences.get(sentenceQAs.sentenceId).foldMap { sentence =>
  //     sentenceQAs.verbs.toList.foldMap { case (verbIndexStr, verbQAs) =>
  //       val verbIndex = verbIndexStr.toInt
  //       val verbForms = sentence.verbEntries(verbIndexStr.toInt).verbInflectedForms
  //       val frameset = framesets(verbForms)
  //       val verbId = VerbId(sentenceQAs.sentenceId, verbIndex)
  //       frameset.frames.zip(frameset.frames.indices).toList
  //         .filter(_._1.verbIds.contains(verbId))
  //         .foldMap { case (frame, frameIndex) =>
  //             val adjacencyExpectations = verbQAs.tails.toList.flatMap {
  //               case Nil => Nil
  //               case fst :: tail => tail.map { snd =>
  //                 val adjProb = adjacencyProb(fst.spans, snd.spans)
  //                 val qPair = fst.question.clausalQ -> snd.question.clausalQ
  //                 val expectedCount = ExpectedCount(adjProb, 1.0)
  //                 Map(qPair -> expectedCount)
  //               }
  //             }.combineAll
  //             Map(verbForms -> Map(frameIndex -> adjacencyExpectations))
  //         }
  //     }
  //   }
  // }

  // def symmetrizeArgumentEquivalences(
  //   equivalences: Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]]
  // ): Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), ExpectedCount]]] = {
  //   equivalences.transform { case (_, frameEqs) =>
  //     frameEqs.transform { case (_, equivalence) =>
  //       equivalence.transform { case (qPair, ec) =>
  //         ec |+| equivalence.get(qPair.swap).combineAll
  //       }
  //     }
  //   }
  // }

  // def getFuzzyArgumentEquivalences(
  //   dataset: Dataset,
  //   framesets: Map[InflectedForms, VerbFrameset],
  //   allSentenceQAs: Stream[IO, SentenceQAOutput]
  // ): IO[Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), Double]]]] = {
  //   allSentenceQAs.map { sentenceQAs =>
  //     getUnsymmetrizedFuzzyArgumentEquivalences(
  //       dataset, framesets, sentenceQAs
  //     )
  //   }.compile.foldMonoid.map(symmetrizeArgumentEquivalences)
  //     .map(
  //       _.transform { case (_, frameRels) =>
  //         frameRels.transform { case (_, frameRel) =>
  //           frameRel.transform { case (_, ec) =>
  //             ec.expectationPerInstance
  //           }
  //         }
  //       }
  //     )
  // }

  // def getUnsymmetrizedCollapsedFuzzyArgumentEquivalences(
  //   dataset: Dataset,
  //   sentenceQAs: SentenceQAOutput)(
  //   implicit Log: TreeLogger[IO, String]
  // ): IO[Map[InflectedForms, Map[(ClausalQ, ClausalQ), ExpectedCount]]] = {
  //   dataset.sentences.get(sentenceQAs.sentenceId).foldMapM { sentence =>
  //     Log.debug(jjm.ling.Text.render(sentence.sentenceTokens)) >> {
  //       IO(
  //         sentenceQAs.verbs.toList.foldMap { case (verbIndexStr, verbQAs) =>
  //           sentence.verbEntries.get(verbIndexStr.toInt).foldMap { verbEntry =>
  //             val verbForms = verbEntry.verbInflectedForms
  //             val adjacencyExpectations = verbQAs.tails.toList.flatMap {
  //               case Nil => Nil
  //               case fst :: tail => tail.map { snd =>
  //                 val adjProb = adjacencyProb(fst.spans, snd.spans)
  //                 val qPair = fst.question.clausalQ -> snd.question.clausalQ
  //                 Map(qPair -> ExpectedCount(adjProb, 1.0))
  //               }
  //             }.combineAll
  //             Map(verbForms -> adjacencyExpectations)
  //           }
  //         }
  //       )
  //     }
  //   }
  // }

  // def symmetrizeCollapsedArgumentEquivalences(
  //   equivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), ExpectedCount]]
  // ): Map[InflectedForms, Map[(ClausalQ, ClausalQ), ExpectedCount]] = {
  //   equivalences.transform { case (_, equivalence) =>
  //     equivalence.transform { case (qPair, ec) =>
  //       ec |+| equivalence.get(qPair.swap).combineAll
  //     }
  //   }
  // }

  // def getCollapsedFuzzyArgumentEquivalences(
  //   dataset: Dataset,
  //   allSentenceQAs: Stream[IO, SentenceQAOutput])(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]] = {
  //   Log.infoBranch("Getting collapsed fuzzy argument equivalences")(
  //     allSentenceQAs.evalMap { sentenceQAs =>
  //       getUnsymmetrizedCollapsedFuzzyArgumentEquivalences(
  //         dataset, sentenceQAs
  //       )
  //     }.infoCompile("Aggregating counts from predictions", dataset.sentences.size)(_.foldMonoid)
  //       .flatMap(x =>
  //         Log.infoBranch("Symmetrizing equivalence counts")(
  //           IO(symmetrizeCollapsedArgumentEquivalences(x))
  //         )
  //       )
  //       .map(
  //         _.transform { case (_, verbRel) =>
  //           verbRel.transform { case (_, ec) =>
  //             ec.expectationPerInstance
  //           }
  //         }
  //       )
  //   )
  // }

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

  // END XXXXXXXXX
