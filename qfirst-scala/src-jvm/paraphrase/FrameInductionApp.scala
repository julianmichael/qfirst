package qfirst.paraphrase
import qfirst._
import qfirst.paraphrase.browse._
import qfirst.frames.implicits._
import qfirst.metrics.HasMetrics.ops._
import qfirst.protocols.SimpleQAs
// import qfirst.frames._
// import qfirst.metrics._

import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._

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

import qasrl.ArgumentSlot
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

class Vocab[A] private (
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

// TODO applicative
case class ClusteringInstances[A](
  instances: Map[InflectedForms, Map[String, Map[Int, A]]]
) {
  def map[B](f: A => B): ClusteringInstances[B] = {
    ClusteringInstances(
      instances.transform { case (k, v) =>
        v.transform { case (k, v) =>
          v.transform { case (k, v) =>
            f(v)
          }
        }
      }
    )
  }
  // TODO merge and stuff
}
object ClusteringInstances {
  implicit def clusteringInstancesMonoid[A]: Monoid[ClusteringInstances[A]] =
    new Monoid[ClusteringInstances[A]] {
      def empty: ClusteringInstances[A] = ClusteringInstances(Map())
      def combine(x: ClusteringInstances[A], y: ClusteringInstances[A]) = {
        ClusteringInstances(
          (x.instances.keySet ++ y.instances.keySet).iterator.map { forms =>
            val xSents = x.instances(forms)
            val ySents = y.instances(forms)
            forms -> (xSents.keySet ++ ySents.keySet).iterator.map { sid =>
              val xVerbs = xSents(sid)
              val yVerbs = ySents(sid)
              sid -> (xVerbs ++ yVerbs)
            }.toMap
          }.toMap
        )
      }
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

  import breeze.linalg.DenseVector

  @JsonCodec case class VerbId(
    sentenceId: String,
    verbIndex: Int)

  def getGoldELMoInstances(
    dataset: Dataset,
    filePrefix: String
  ): IO[ClusteringInstances[DenseVector[Float]]] = {
    val idsPath = Paths.get(filePrefix + "_ids.jsonl")
    val embPath = Paths.get(filePrefix + "_emb.bin")
    val embDim = 1024
    for {
      ids <- logOp(
        "Reading verb IDs",
        FileUtil.readJsonLines[VerbId](idsPath).compile.toList
      )
      embeddings <- logOp(
        "Reading verb embeddings",
        FileUtil.readDenseFloatVectorsNIO(embPath, embDim)
      )
      // _ <- IO(println(s"Number of IDs: ${ids.size}; Number of embeddings: ${embeddings.size}; embedding size: ${embeddings.head.size}"))
      _ <- IO {
        val numToCheck = 5
        val propSane = embeddings.take(numToCheck).foldMap(_.activeValuesIterator.map(math.abs).filter(f => f > 1e-2 && f < 1e2).size).toDouble / (numToCheck * embDim)
        val warnText = if(propSane < 0.8) "[== WARNING ==] there might be endianness issues with how you're reading the ELMo embeddings; " else ""
        println(warnText + f"Sanity check: ${propSane}%.3f of ELMo embedding units have absolute value between ${1e-2}%s and ${1e2}%s.")
        // embeddings.take(numToCheck).foreach(e => println(e.activeValuesIterator.take(10).mkString("\t")))
      }
    } yield ClusteringInstances(
      ids.zip(embeddings).foldMap { case (VerbId(sentenceId, verbIndex), embedding) =>
        val verbForms = dataset.sentences(sentenceId).verbEntries(verbIndex).verbInflectedForms
        Map(verbForms -> Map(sentenceId -> Map(verbIndex -> List(embedding))))
      }
    ).map(_.head)
  }

  // def getPredictedInstances(
  //   predictions: Stream[IO, SentencePrediction[QABeam]],
  //   filter: SimpleQAs.Filter
  // ): IO[Instances] = {
  //   val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)
  //   predictions.map { sentencePred =>
  //     sentencePred.verbs.foldMap(
  //       verbPred => Map(
  //         verbPred.verbInflectedForms -> Map(
  //           sentencePred.sentenceId -> Map(
  //             verbPred.verbIndex ->
  //               protocol.filterBeam(filter, verbPred).map {
  //                 case (qString, (slots, spans)) => slots -> spans
  //               }
  //           ).filter(_._2.nonEmpty)
  //         ).filter(_._2.nonEmpty)
  //       ).filter(_._2.nonEmpty)
  //     )
  //   }.compile.foldMonoid
  // }

  import qfirst.paraphrase.models._
  import breeze.stats.distributions.Multinomial

  def runVerbWiseSoftEMWithComposite(
    instances: Instances,
    elmoVecs: ClusteringInstances[DenseVector[Float]],
    rand: Random
  ): IO[FrameInductionResults] = {
    val algorithm = new CompositeClusteringAlgorithm {
      val _1 = DirichletMAPClustering
      val _2 = VectorMeanClustering
      val lambda = 0.0
    }
    val verbCounts = instances.map { case (forms, sentMap) => forms -> sentMap.iterator.map(_._2.size).sum }
    val verbs = instances.map { case (forms, sentMap) =>
      forms -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
        verbIndices.keys.toVector
          .filter(vi =>
            instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty &&
              elmoVecs.instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
          )
          .map(vi => VerbId(sid, vi))
      }
    }.filter(_._2.nonEmpty)
    val clauseVocab = makeClauseVocab(instances)
    val makeInstance = (forms: InflectedForms, verbId: VerbId) => {
      val questions = instances(forms)(verbId.sentenceId)(verbId.verbIndex).keySet.toList
      val clauseCounts = ClauseResolution.getResolvedFramePairs(
        forms, questions
      ).map(_._1).map(ClauseResolution.getClauseTemplate)
        .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
      val vector = elmoVecs.instances(forms)(verbId.sentenceId)(verbId.verbIndex)
      clauseCounts -> vector
    }
    runVerbWiseSoftEM(algorithm)(
      verbs = verbs,
      makeInstance = makeInstance,
      hyperparams = (DirichletMAPClustering.Hyperparams(clauseVocab.size, 0.1), ()),
      numFrames = v => math.max(2, math.round(math.log(verbCounts(v).toDouble) / math.log(10)).toInt), // can change, idk
      getFrame = {
        case (multinomial, _) => (multinomial.params / multinomial.sum).toScalaVector.toList.zipWithIndex
            .filter(_._1 > 0.01).map {
              case (prob, index) => FrameClause(clauseVocab.getItem(index), prob)
            }
      },
      rand = rand
    )
  }

  def runVerbWiseSoftEM(
    algorithm: ClusteringAlgorithm)(
    verbs: Map[InflectedForms, Vector[VerbId]],
    makeInstance: (InflectedForms, VerbId) => algorithm.Instance,
    hyperparams: algorithm.Hyperparams,
    numFrames: (InflectedForms => Int),
    getFrame: algorithm.ClusterParam => List[FrameClause],
    rand: Random,
    shouldLog: Boolean = false
  ): IO[FrameInductionResults] = for {
    frameInfos <- verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
      val instances = verbIds.map(makeInstance(verbInflectedForms, _))
      for {
        modelInit <- {
          val init = IO(algorithm.initPlusPlus(instances, hyperparams, math.min(numFrames(verbInflectedForms), instances.size)))
          if(shouldLog) logOp("Initializing model", init)
          else init
        }
        (model, assignments, _) <- IO(
          algorithm.runSoftEM(
            initModel = modelInit,
            instances = instances,
            hyperparams = hyperparams,
            stoppingThreshold = 0.001,
            shouldLog = shouldLog
          )
        )
      } yield {
        val prior = Multinomial(assignments.iterator.map(m => m.params / m.sum).reduce(_ + _))
        println(prior)
        println(assignments)
        val frames = model.map(getFrame).zipWithIndex.map { case (frameClauses, clusterIndex) =>
          VerbFrame(frameClauses, Map(), prior.probabilityOf(clusterIndex))
        }
        val frameset = VerbFrameset(verbInflectedForms, frames.toList)
        val probabilities = verbIds.zip(assignments).foldMap { case (verbId, assignmentProbs) =>
          Map(verbId.sentenceId -> Map(verbId.verbIndex -> assignmentProbs.params.toScalaVector))
        }
        (verbInflectedForms, frameset, probabilities)
      }
    }
  } yield FrameInductionResults(
    frameInfos.map { case (forms, frameset, _) => forms -> frameset }.toMap,
    frameInfos.map { case (forms, _, assignments) => forms -> assignments }.toMap
  )

  // TODO:
  // finish implementing example clustering
  // change frameset to include assignments inside it
  // change frame induction results to just be a list of framesets, written as gzipped jsonl
  // agglomerative clustering
  // change frameset to allow agglomerative style too
  // change eval/browser to be over gold qa-srl
  // run predictions over propbank
  // convert propbank predictions to qa-srl dataset
  // run elmo over propbank
  // implement propbank metrics

  object Induce {
    import qfirst.paraphrase.models._
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
      modelInit <- logOp(
        "Initializing model",
        UnigramMixtureModel.initClever(
          indexedInstances, numFrames, clauseVocab.size, clusterConcentrationParameter, rand
        )
      )
      (model, assignments, _) <- IO(
        MixtureOfUnigrams.runSoftEM(
          initModel = modelInit,
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
          FrameClause(clauseVocab.getItem(index), prob)
        }.sortBy(-_.probability).takeWhile(_.probability > uniformProbability) // TODO probably want to add higher threshold as a parameter
        VerbFrame(clauseTemplates.toList, Map(), framePriorProb)
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

    import LDA.LDAModel
    def lda(
      instances: Instances,
      numFrames: Int,
      priorConcentrationParameter: Double,
      clusterConcentrationParameter: Double,
      rand: Random
    ): IO[FrameInductionResults] = for {
      verbVocab <- logOp("Indexing verbs", makeVerbVocab(instances))
      clauseVocab <- logOp("Indexing clauses", makeClauseVocab(instances))
      (indexedInstances, instanceIds) <- logOp(
        "Indexing instances",
        instances.toList.sortBy(p => verbVocab.getIndex(p._1)).map { case (verbInflectedForms, verbTypeInstances) =>
          verbTypeInstances.iterator.flatMap { case (sentenceId, sentenceInstances) =>
            sentenceInstances.iterator.map { case (verbIndex, verbInstances) =>
              val questions = verbInstances.keySet.toList
              val indexedClauseCounts = ClauseResolution
                .getResolvedFramePairs(verbInflectedForms, questions)
                .map(_._1).map(ClauseResolution.getClauseTemplate)
                .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
              val instanceId = (sentenceId, verbIndex)
              indexedClauseCounts -> instanceId
            }
          }.toVector.unzip
        }.toVector.unzip
      )
      modelInit <- logOp(
        "Initializing model",
        LDAModel.initClever(indexedInstances, numFrames, clauseVocab.size, clusterConcentrationParameter, rand)
      )

      (model, assignments, _) <- IO(
        LDA.runSoftEM(
          modelInit, indexedInstances, priorConcentrationParameter, clusterConcentrationParameter, stoppingThreshold = 0.001
        )
      )
    } yield {
      val allFrames = model.clusters.map { cluster =>

      }
      val globalFrames = model.clusters.map { frameClauseDist =>
        val uniformProbability = 1.0 / frameClauseDist.size
        val clauseTemplates = frameClauseDist.zipWithIndex.map { case (prob, index) =>
          FrameClause(clauseVocab.getItem(index), prob)
        }.sortBy(-_.probability).takeWhile(_.probability > uniformProbability) // TODO probably want to add higher threshold as a parameter
        VerbFrame(clauseTemplates.toList, Map(), 0.0) // TODO set prob later
      }
      // val allFramesAndAssignments = model.priors.zipWithIndex.map { case (verbPrior, verbIndex) =>
      //   val verbForms = verbVocab.getItem(verbIndex)
      //   val frameUniformProbability = 1.0 / numFrames.toDouble
      //   val frameset = VerbFrameset(verbForms, )
      // }
      val frameMaxCountsForVerbs: Vector[Map[Int, Int]] = assignments.map(
        _.foldMap { dist =>
          val maxIndex = dist.zipWithIndex.maxBy(_._1)._2
          Map(maxIndex -> 1)
        }
      )
      val allFramesetsAndIndices = model.priors.zipWithIndex.map { case (frameDist, verbIndex) =>
        val verbInflectedForms = verbVocab.getItem(verbIndex)
        val uniformProbability = 1.0 / frameDist.size
        val frameIndicesHavingMax = frameMaxCountsForVerbs(verbIndex).keySet
        val framesWithIndices = frameDist.zipWithIndex.collect {
          case (frameProb, frameIndex) if frameProb > uniformProbability || frameIndicesHavingMax.contains(frameIndex) =>
            globalFrames(frameIndex).copy(probability = frameProb) -> frameIndex
        }.sortBy(-_._1.probability)
        val frameset = VerbFrameset(verbInflectedForms, framesWithIndices.map(_._1).toList)
        val frameIndices = framesWithIndices.map(_._2)
        frameset -> frameIndices
      }
      val allFramesets = allFramesetsAndIndices.map { case (v, _) => v.inflectedForms -> v }.toMap
      val allAssignments: Map[InflectedForms, Map[String, Map[Int, Vector[Double]]]] = {
        instanceIds.zip(assignments).zipWithIndex.foldMap {
          case ((verbInstanceIds, verbAssignments), verbTypeIndex) =>
            val verbInflectedForms = verbVocab.getItem(verbTypeIndex)
            verbInstanceIds.zip(verbAssignments).foldMap {
              case ((sentenceId, verbInstanceIndex), dist) =>
                val selectedDist = allFramesetsAndIndices(verbTypeIndex)._2.map(dist(_))
                Map(verbInflectedForms -> Map(sentenceId -> Map(verbInstanceIndex -> selectedDist)))
                // shouldn't count anything twice so it should be fine
            }
        }
      }
      FrameInductionResults(
        frames = allFramesets,
        assignments = allAssignments
      )
    }
  }

  def saveForQA(
    dataset: Dataset,
    frameInductionResults: FrameInductionResults,
    savePath: NIOPath,
    clauseInclusionThreshold: Double = 0.01,
    clauseInclusionMinimum: Int = 2
  ) = {
    import io.circe.Json
    import io.circe.syntax._
    val sentenceJsons = dataset.sentences.iterator.map { case (sentenceId, sentence) =>
      Json.obj(
        "sentenceId" -> sentenceId.asJson,
        "sentenceTokens" -> sentence.sentenceTokens.asJson,
        "verbs" -> sentence.verbEntries.values.toList.flatMap { verb =>
          frameInductionResults.assignments.get(verb.verbInflectedForms)
            .flatMap(_.get(sentenceId)).flatMap(_.get(verb.verbIndex)).flatMap { frameDist =>
              val bestFrames = frameInductionResults
                .frames(verb.verbInflectedForms).frames
                .zip(frameDist).maximaBy(_._2).map(_._1)
              bestFrames.headOption.filter(_ => bestFrames.size == 1).map { bestFrame =>
                val sortedClauseTemplates = bestFrame.clauseTemplates.sortBy(-_.probability)
                val numClauseTemplates = math.max(
                  sortedClauseTemplates .takeWhile(_.probability > clauseInclusionThreshold).size, clauseInclusionMinimum
                )
                val clauses = sortedClauseTemplates.take(numClauseTemplates).map(_.args).flatMap { clauseTemplate =>
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
              }
            }
        }.asJson
      )
    }.toList
    FileUtil.writeJsonLines(savePath, io.circe.Printer.noSpaces)(sentenceJsons)
  }

  def program(
    qasrlBankPath: NIOPath, predDir: NIOPath,
    relativeOutDirOpt: Option[String],
    trainOnDev: Boolean,
    testOnTest: Boolean
  ): IO[ExitCode] = {
    val trainSetName = if(trainOnDev) "dev" else "train"
    val trainSetFilename = s"$trainSetName.jsonl.gz"
    val evalSetName = if(testOnTest) "test" else "dev"
    val evalSetFilename = s"$evalSetName.jsonl.gz"
    val evalSetPath = qasrlBankPath.resolve(s"dense/$evalSetFilename")
    val paraphraseGoldPath = predDir.resolve("gold-paraphrases.json")
    // val predFilename = if(testOnTest) predDir.resolve("predictions-test.jsonl") else predDir.resolve("predictions.jsonl")
    val outDir = relativeOutDirOpt.map(predDir.resolve).getOrElse {
      scala.collection.immutable.Stream.from(0)
        .map(i => predDir.resolve(s"trial-$i"))
        .filter(p => !Files.exists(p))
        .head
    }
    val evaluationItemsPath = predDir.resolve(s"eval-sample-$evalSetName.jsonl")
    val resultsFilename = if(testOnTest) "results-test.json" else "results.json"
    val resultsPath = outDir.resolve(resultsFilename)
    val outForQAPath = if(testOnTest) outDir.resolve("results-test-qa-input.jsonl.gz") else outDir.resolve("results-qa-input.jsonl.gz")
    implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
    for {
      _ <- if(Files.exists(outDir)) IO.unit else IO {
        println(s"Creating output directory $outDir")
        Files.createDirectories(outDir)
      }
      _ <- if(!trainOnDev) IO.unit else {
        import sys.process._
        IO(s"touch ${outDir.resolve("dev")}".!)
      }
      trainSet <- logOp("Reading training set", readDataset(qasrlBankPath.resolve("expanded").resolve(trainSetFilename)))
      trainInstances <- logOp("Constructing training instances", getGoldInstances(trainSet))
      evalSet <- logOp("Reading full eval set", readDataset(qasrlBankPath.resolve("orig").resolve(evalSetFilename)))
      evalInstances <- logOp("Constructing eval instances", getGoldInstances(evalSet))
      // filter <- {
      //   import qasrl.data.JsonCodecs._
      //   import io.circe.generic.auto._
      //   FileUtil.readJson[SimpleQAs.Filter](predDir.resolve("filter.json"))
      // }
      // predInstances <- logOp(
      //   "Loading predictions",
      //   {
      //     import qasrl.data.JsonCodecs._
      //     import io.circe.generic.auto._
      //     getPredictedInstances(FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename), filter)
      //   }
      // )
      trainElmoVecs <- getGoldELMoInstances(trainSet, s"qasrl-v2-elmo/$trainSetName")
      evalElmoVecs <- getGoldELMoInstances(evalSet, s"qasrl-v2-elmo/$evalSetName")
      results <- runVerbWiseSoftEMWithComposite(
        instances = trainInstances |+| evalInstances,
        elmoVecs = trainElmoVecs |+| evalElmoVecs,
        rand = new scala.util.Random(3266435L)
      )
      _ <- logOp("Writing learned frames", FileUtil.writeJson(resultsPath, io.circe.Printer.noSpaces)(results))
      paraphraseGold <- {
        if(!Files.exists(paraphraseGoldPath)) {
          IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
            IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
        } else FileUtil.readJson[EvalApp.ParaphraseAnnotations](paraphraseGoldPath)
      }
      // predictionsStream = {
      //   import qasrl.data.JsonCodecs._
      //   import io.circe.generic.auto._
      //   FileUtil.readJsonLines[SentencePrediction[QABeam]](predFilename)
      // }
      // evalSet <- logOp("Reading eval set", readDataset(evalSetPath))
      evaluationItems <- EvalApp.getEvaluationItems(evalSet, evaluationItemsPath)
      _ <- EvalApp.runEvaluation(evalSet, evaluationItems.toSet, results, paraphraseGold)
      _ <- saveForQA(trainSet |+| evalSet, results, outForQAPath)
    } yield ExitCode.Success

    // for {
    //   dataset <- logOp("Reading mini dev set", qasrl.bank.Data.readDataset(Paths.get("dev-mini.jsonl.gz")))
    //   elmoVecs <- getGoldELMoInstances(dataset, "dev-mini-elmo")
    // } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
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
