package qfirst.paraphrase
import qfirst.paraphrase.browse._
import qfirst._
import qfirst.metrics.HasMetrics.ops._
import qfirst.protocols.SimpleQAs
// import qfirst.frames._
// import qfirst.metrics._

import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files
import java.nio.file.Paths

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank._
import qasrl.data._
import qasrl.labeling.SlotBasedLabel

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

import scala.collection.immutable.Map

import scala.util.Random

case class FrameInductionResults(
  frames: Map[InflectedForms, VerbFrameset],
  assignments: Map[InflectedForms, Map[String, Map[Int, Vector[Double]]]] // verb type -> sentence id -> verb token -> dist. over frames for verb type
)
object FrameInductionResults {
  def fromLists(
    framesList: List[(InflectedForms, VerbFrameset)],
    assignmentsList: List[(InflectedForms, Map[String, Map[Int, Vector[Double]]])],
    ) = FrameInductionResults(framesList.toMap, assignmentsList.toMap)
  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  implicit val frameInductionResultsDecoder: Decoder[FrameInductionResults] =
    Decoder.forProduct2("frames", "assignments")(fromLists)
  implicit val frameInductionResultsEncoder: Encoder[FrameInductionResults] =
    Encoder.forProduct2("frames", "assignments")(d =>
      (d.frames.toList, d.assignments.toList)
    )
}

class Vocab[A](
  indexToItem: Vector[A],
  itemToIndex: Map[A, Int]
) {
  def getItem(index: Int) = indexToItem(index)
  def getIndex(item: A) = itemToIndex(item)
  def size = indexToItem.size
}
object Vocab {
  def make[A](items: Set[A]): Vocab[A] = {
    val itemsVec = items.toVector
    new Vocab(itemsVec, itemsVec.zipWithIndex.toMap)
  }
}

object FrameInductionApp extends IOApp {

  import ClauseResolution.ArgStructure

  type QABeam = List[SimpleQAs.BeamItem[SlotBasedLabel[VerbForm]]]
  type Instances = Map[InflectedForms, Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  // type ResolvedInstances = Map[InflectedForms, Map[SentenceId, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]

  def makeVerbVocab(instances: Instances): Vocab[InflectedForms] = {
    Vocab.make(instances.keySet)
  }
  def makeClauseVocab(instances: Instances): Vocab[ArgStructure] = {
    val qSlotsSet = instances.values.iterator.flatMap(sentMap =>
      sentMap.values.iterator.flatMap(verbMap =>
        verbMap.values.iterator.flatMap(qMap =>
          qMap.keySet.iterator
        )
      )
    ).toSet
    val clauseTemplateSet = qSlotsSet.flatMap(slots =>
      ClauseResolution.getFramesWithAnswerSlots(slots)
        .map(_._1).map(ClauseResolution.getClauseTemplate)
    )
    Vocab.make(clauseTemplateSet)
  }

  def getGoldInstances(dataset: Dataset): Instances = {
    dataset.sentences
      .iterator.flatMap { case (sid, sentence) => sentence.verbEntries.values.map(sid -> _) }.toList
      .groupBy(_._2.verbInflectedForms).map { case (verbInflectedForms, pairs) =>
        verbInflectedForms -> pairs.groupBy(_._1).map { case (sid, pairs) =>
          sid -> pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
            verbIndex -> filterGoldNonDense(verb)._2.map { case (qString, qLabel) =>
              qLabel.questionSlots -> (
                qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
              )
            }
          }
        }
      }
  }

  def getPredictedInstances(
    predictions: Stream[IO, SentencePrediction[QABeam]],
    filter: SimpleQAs.Filter
  ): IO[Instances] = {
    val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)
    predictions.map { sentencePred =>
      sentencePred.verbs.foldMap(
        verbPred => Map(
          verbPred.verbInflectedForms -> Map(
            sentencePred.sentenceId -> Map(
              verbPred.verbIndex ->
                protocol.filterBeam(filter, verbPred).map {
                  case (qString, (slots, spans)) => slots -> spans
                }
            )
          )
        )
      )
    }.compile.foldMonoid
  }

  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))

  object Induce {
    import qfirst.topics.MixtureOfUnigrams
    import MixtureOfUnigrams.UnigramMixtureModel
    def mixtureOfUnigrams(
      instances: Instances,
      numFrames: Int,
      priorConcentrationParameter: Double,
      clusterConcentrationParameter: Double,
      rand: Random
    ): IO[FrameInductionResults] = for {
      clauseVocab <- logOp("Indexing clauses", makeClauseVocab(instances))
      (indexedInstances, instanceIds) <- logOp(
        "Indexing instances",
        instances.iterator.flatMap { case (verbInflectedForms, verbTypeInstances) =>
          verbTypeInstances.iterator.flatMap { case (sentenceId, sentenceInstances) =>
            sentenceInstances.iterator.map { case (verbIndex, verbInstances) =>
              val questions = verbInstances.keySet.toList
              val indexedClauseCounts = ClauseResolution.getResolvedFramePairs(
                verbInflectedForms, questions
              ).map(_._1).map(ClauseResolution.getClauseTemplate)
                .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
              val instanceId = (verbInflectedForms, sentenceId, verbIndex)
              indexedClauseCounts -> instanceId
            }
          }
        }.toList.unzip
      )
      (model, assignments, _) <- IO(
        MixtureOfUnigrams.runSoftEM(
          initModel = UnigramMixtureModel.initClever(
            indexedInstances, numFrames, clauseVocab.size, clusterConcentrationParameter, rand
          ),
          instances = indexedInstances,
          priorConcentrationParameter = priorConcentrationParameter,
          clusterConcentrationParameter = clusterConcentrationParameter,
          stoppingThreshold = 0.001
        )
      )
    } yield {
      val globalFrames = model.prior.zip(model.clusters).map { case (framePriorProb, frameClauseDist) =>
        val uniformProbability = 1.0 / frameClauseDist.size
        val clauseTemplates = frameClauseDist.zipWithIndex.map { case (prob, index) =>
          FrameClause(clauseVocab.getItem(index), Map(), prob)
        }.sortBy(-_.probability).takeWhile(_.probability > uniformProbability) // TODO probably want to add higher threshold as a parameter
        VerbFrame(clauseTemplates.toList, framePriorProb)
      }
      val frameDistributionsByVerb: Map[InflectedForms, Vector[Double]] = instanceIds.zip(assignments).foldMap {
        case ((verbInflectedForms, sentenceId, verbIndex), dist) =>
          Map(verbInflectedForms -> Vector(dist))
      }.map { case (inflectedForms, dists) =>
          val pcounts = dists.transpose.map(_.sum)
          val total = pcounts.sum
          inflectedForms -> pcounts.map(_ / total)
      }
      val frameMaxCountsByVerb: Map[InflectedForms, Map[Int, Int]] = instanceIds.zip(assignments).foldMap {
        case ((verbInflectedForms, sentenceId, verbIndex), dist) =>
          val maxIndex = dist.zipWithIndex.maxBy(_._1)._2
          Map(verbInflectedForms -> Map(maxIndex -> 1))
      }
      val allFramesetsAndIndices = frameDistributionsByVerb.map { case (verbInflectedForms, frameDist) =>
        val uniformProbability = 1.0 / frameDist.size
        val frameIndicesHavingMax = frameMaxCountsByVerb(verbInflectedForms).keySet
        val framesWithIndices = frameDist.zipWithIndex.collect {
          case (frameProb, frameIndex) if frameProb > uniformProbability || frameIndicesHavingMax.contains(frameIndex) =>
            globalFrames(frameIndex).copy(probability = frameProb) -> frameIndex
        }.sortBy(-_._1.probability)
        val frameset = VerbFrameset(verbInflectedForms, framesWithIndices.map(_._1).toList)
        val frameIndices = framesWithIndices.map(_._2)
        verbInflectedForms -> (frameset -> frameIndices)
      }
      val allFramesets = allFramesetsAndIndices.map { case (k, (v, _)) => k -> v }
      val allAssignments: Map[InflectedForms, Map[String, Map[Int, Vector[Double]]]] = {
        instanceIds.zip(assignments).foldMap {
          case ((verbInflectedForms, sentenceId, verbIndex), dist) =>
            val selectedDist = allFramesetsAndIndices(verbInflectedForms)._2.map(dist(_))
            Map(verbInflectedForms -> Map(sentenceId -> Map(verbIndex -> selectedDist)))
            // shouldn't count anything twice so it should be fine
        }
      }
      FrameInductionResults(
        frames = allFramesets,
        assignments = allAssignments // verb type -> sentence id -> verb token -> dist. over frames for verb type
      )
    }
  }

  def program(
    qasrlBankPath: NIOPath, predDir: NIOPath,
    relativeOutDirOpt: Option[String],
    trainOnDev: Boolean,
    testOnTest: Boolean
  ): IO[ExitCode] = {
    val trainSetFilename = if(trainOnDev) "dev.jsonl.gz" else "train.jsonl.gz"
    val evalSetName = if(testOnTest) "test" else "dev"
    val evalSetFilename = s"$evalSetName.jsonl.gz"
    val evalSetPath = qasrlBankPath.resolve(s"dense/$evalSetFilename")
    val paraphraseGoldPath = predDir.resolve("gold-paraphrases.json")
    val predFilename = if(testOnTest) predDir.resolve("predictions-test.jsonl") else predDir.resolve("predictions.jsonl")
    val outDir = relativeOutDirOpt.map(predDir.resolve).getOrElse {
      scala.collection.immutable.Stream.from(0)
        .map(i => predDir.resolve(s"trial-$i"))
        .filter(p => !Files.exists(p))
        .head
    }
    val evaluationItemsPath = predDir.resolve(s"eval-sample-$evalSetName.jsonl")
    val resultsFilename = if(testOnTest) "results-test.json" else "results.json"
    val resultsPath = outDir.resolve(resultsFilename)
    for {
      _ <- if(Files.exists(outDir)) IO.unit else IO {
        println(s"Creating output directory $outDir")
        Files.createDirectories(outDir)
      }
      _ <- if(!trainOnDev) IO.unit else {
        import sys.process._
        IO(s"touch ${outDir.resolve("dev")}".!)
      }
      trainSet <- logOp("Reading training set", qasrl.bank.Data.readDataset(qasrlBankPath.resolve("expanded").resolve(trainSetFilename)))
      trainInstances <- logOp("Constructing training instances", getGoldInstances(trainSet))
      filter <- {
        import qasrl.data.JsonCodecs._
        import io.circe.generic.auto._
        FileUtil.readJson[SimpleQAs.Filter](predDir.resolve("filter.json"))
      }
      predInstances <- logOp(
        "Loading predictions",
        {
          import qasrl.data.JsonCodecs._
          import io.circe.generic.auto._
          getPredictedInstances(FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename), filter)
        }
      )
      // TODO properly pass in hyperparameters and stuff. maybe want a config file lol...
      results <- Induce.mixtureOfUnigrams(
        instances = trainInstances |+| predInstances,
        numFrames = 20,
        priorConcentrationParameter = 1.0,
        clusterConcentrationParameter = 1.0,
        rand = new scala.util.Random(3266435L)
      )
      _ <- logOp("Writing learned frames", FileUtil.writeJson(resultsPath, io.circe.Printer.noSpaces)(results))
      paraphraseGold <- {
        if(!Files.exists(paraphraseGoldPath)) {
          IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
            IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
        } else FileUtil.readJson[EvalApp.ParaphraseAnnotations](paraphraseGoldPath)
      }
      evalSet <- IO(qasrl.bank.Data.readDataset(evalSetPath))
      predictionsStream = {
        import qasrl.data.JsonCodecs._
        import io.circe.generic.auto._
        FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename)
      }
      evaluationItems <- EvalApp.getEvaluationItems(evalSet, evaluationItemsPath)
      _ <- EvalApp.runEvaluation(evalSet, evaluationItems.toSet, predictionsStream, filter, results, paraphraseGold)
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val goldPath = Opts.option[NIOPath](
      "qasrl-gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val predPath = Opts.option[NIOPath](
      "qasrl-pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val outDir = Opts.option[String](
      "out", metavar = "path", help = "Relative path to the output directory."
    ).orNone
    val trainOnDev = Opts.flag(
      "dev", help = "Run on the dev set for fast iteration."
    ).orFalse
    val testOnTest = Opts.flag(
      "test", help = "Evaluate on the test set instead of dev."
    ).orFalse

    (goldPath, predPath, outDir, trainOnDev, testOnTest).mapN(program)
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
