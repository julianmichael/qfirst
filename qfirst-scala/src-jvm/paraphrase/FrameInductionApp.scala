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

// case class FrameInductionResults(
//   frames: Map[InflectedForms, VerbFrameset],
//   assignments: Map[InflectedForms, Map[String, Map[Int, Vector[Double]]]] // verb type -> sentence id -> verb token -> dist. over frames for verb type
// )
// object FrameInductionResults {
//   def fromLists(
//     framesList: List[(InflectedForms, VerbFrameset)],
//     assignmentsList: List[(InflectedForms, Map[String, Map[Int, Vector[Double]]])],
//     ) = FrameInductionResults(framesList.toMap, assignmentsList.toMap)
//   import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
//   implicit val frameInductionResultsDecoder: Decoder[FrameInductionResults] =
//     Decoder.forProduct2("frames", "assignments")(fromLists)
//   implicit val frameInductionResultsEncoder: Encoder[FrameInductionResults] =
//     Encoder.forProduct2("frames", "assignments")(d =>
//       (d.frames.toList, d.assignments.toList)
//     )
// }

class Vocab[A] private (
  indexToItem: Vector[A],
  itemToIndex: Map[A, Int]
) {
  def items = indexToItem
  def indices = indexToItem.indices.toVector
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

  import qfirst.paraphrase.models._
  import breeze.stats.distributions.Multinomial

  def runVerbWiseAgglomerativeWithComposite(
    instances: Instances,
    elmoVecs: ClusteringInstances[DenseVector[Float]],
    interpolation: Double = 0.5,
    rand: Random
  ): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
    val algorithm = new CompositeClusteringAlgorithm {
      val _1 = MinEntropyClustering
      val _2 = VectorMeanClustering
      val lambda = interpolation
    }
    ???
  }

  def runVerbWiseAgglomerative(
    algorithm: ClusteringAlgorithm)(
    verbs: Map[InflectedForms, Vector[VerbId]],
    makeInstance: (InflectedForms, VerbId) => algorithm.Instance,
    hyperparams: algorithm.Hyperparams,
    // numFrames: (InflectedForms => Int),
    getFrame: algorithm.ClusterParam => List[FrameClause],
    // rand: Random,
    // shouldLog: Boolean = false
  ): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
    verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
      val instances = verbIds.map(makeInstance(verbInflectedForms, _))
      IO(println(s"Clustering verb: ${verbInflectedForms.stem}")).as(
        verbInflectedForms -> algorithm.runAgglomerativeClustering(instances, hyperparams)._1
          .map(verbs(verbInflectedForms))
      )
    }.map(_.toMap)
  }

  def runVerbWiseSoftEMWithComposite(
    instances: Instances,
    elmoVecs: ClusteringInstances[DenseVector[Float]],
    rand: Random
  ): IO[Map[InflectedForms, VerbFrameset]] = {
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
  ): IO[Map[InflectedForms, VerbFrameset]] = {
    verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
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
        // println(prior)
        // println(assignments)
        val frames = model.map(getFrame).zipWithIndex.map { case (frameClauses, clusterIndex) =>
          VerbFrame(frameClauses, null /* TODO: replace with dummy merge tree */, prior.probabilityOf(clusterIndex))
        }
        val instanceProbs = verbIds.zip(assignments).foldMap { case (verbId, assignmentProbs) =>
          Map(verbId -> assignmentProbs.params.toScalaVector)
        }
        verbInflectedForms -> VerbFrameset(verbInflectedForms, frames.toList, instanceProbs)
      }
    }.map(_.toMap)
  }

  type ClausalQ = (ArgStructure, ArgumentSlot)
  import breeze.linalg._

  def runCoindexing(
    predicateSenseResults: Map[InflectedForms, VerbFrameset],
    fuzzyArgEquivalences: Map[InflectedForms, Map[Int, Map[(ClausalQ, ClausalQ), Double]]]
  ): IO[Map[InflectedForms, VerbFrameset]] = {
    predicateSenseResults.map { case (verbForms, verbFrameset) =>
      val frameRels = fuzzyArgEquivalences(verbForms)
      val coindexedFrames = verbFrameset.frames.zipWithIndex.map { case (frame, frameIndex) =>
        val frameRel = frameRels(frameIndex)
        val clausalQVocab = Vocab.make(frameRel.keySet.flatMap(x => Set(x._1, x._2)))
        val indices = clausalQVocab.indices
        val fuzzyEquivMatrix = DenseMatrix.zeros[Double](clausalQVocab.size, clausalQVocab.size)
        for(i <- indices; j <- indices) {
          val (qi, qj) = clausalQVocab.getItem(i) -> clausalQVocab.getItem(j)
          val score = -math.log(
            if(i == j) 1.0 // reflexive
            else if(qi._1 == qj._1) 0.0 // prohibit coindexing within a clause
            else {
              print(".")
              frameRel.getOrElse(qi -> qj, 0.5) // if they never seemed to appear together? idk
            }
          )
          fuzzyEquivMatrix.update(i, j, score)
        }
        val (mergeTree, _) = CompleteLinkageClustering.runAgglomerativeClustering(indices, fuzzyEquivMatrix)
        frame.copy(coindexingTree = mergeTree.map(clausalQVocab.getItem))
      }
      verbFrameset.copy(frames = coindexedFrames)
    }
    IO(predicateSenseResults)
  }

  def runCollapsedCoindexing(
    predicateSenseResults: Map[InflectedForms, VerbFrameset],
    fuzzyArgEquivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  ): IO[Map[InflectedForms, VerbFrameset]] = {
    predicateSenseResults.map { case (verbForms, verbFrameset) =>
      val verbRel = fuzzyArgEquivalences(verbForms)
      val coindexedFrames = verbFrameset.frames.zipWithIndex.map { case (frame, frameIndex) =>
        val clausalQVocab = Vocab.make(
          frame.clauseTemplates.map(_.args).flatMap(clauseTemplate =>
            (clauseTemplate.args.keys.toList: List[ArgumentSlot]).map(slot =>
              clauseTemplate -> slot
            )
          ).toSet
        )
        val indices = clausalQVocab.indices
        val fuzzyEquivMatrix = DenseMatrix.zeros[Double](clausalQVocab.size, clausalQVocab.size)
        for(i <- indices; j <- indices) {
          val (qi, qj) = clausalQVocab.getItem(i) -> clausalQVocab.getItem(j)
          val score = -math.log(
            if(i == j) 1.0 // reflexive
            else if(qi._1 == qj._1) 0.0 // prohibit coindexing within a clause
            else {
              print(".")
              verbRel.getOrElse(qi -> qj, 0.5) // if they never seemed to appear together? idk. shouldn't happen
            }
          )
          fuzzyEquivMatrix.update(i, j, score)
        }
        val (mergeTree, _) = CompleteLinkageClustering.runAgglomerativeClustering(indices, fuzzyEquivMatrix)
        frame.copy(coindexingTree = mergeTree.map(clausalQVocab.getItem))
      }
      verbFrameset.copy(frames = coindexedFrames)
    }
    IO(predicateSenseResults)
  }

  def program(
    experimentName: String,
    trainOnDev: Boolean,
    testOnTest: Boolean
  ): IO[ExitCode] = {
    implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
    for {
      config <- Config.make(experimentName, Some(trainOnDev), testOnTest)
      trainSet <- config.readInputSet
      trainInstances <- logOp("Constructing training instances", getGoldInstances(trainSet))
      evalSet <- config.readEvalSet
      fullSet = trainSet |+| evalSet
      evalInstances <- logOp("Constructing eval instances", getGoldInstances(evalSet))
      trainElmoVecs <- getGoldELMoInstances(trainSet, config.inputElmoPrefix)
      evalElmoVecs <- getGoldELMoInstances(evalSet, config.evalElmoPrefix)
      predicateSenseResults <- runVerbWiseSoftEMWithComposite(
        instances = trainInstances |+| evalInstances,
        elmoVecs = trainElmoVecs |+| evalElmoVecs,
        rand = new scala.util.Random(3266435L)
      )
      collapsedQAOutputs <- config.readCollapsedQAOutputs
      coindexedResults <- runCollapsedCoindexing(
        predicateSenseResults, collapsedQAOutputs
      )
      // fuzzyArgEquivalences <- logOp(
      //   "Counting argument cooccurrences",
      //   QAInputApp.getFuzzyArgumentEquivalences(
      //     fullSet, predicateSenseResults,
      //     config.streamQAOutputs,
      //     hardAssignments = false // TODO this is a hyperparameter
      //   )
      // )
      _ <- config.writeFramesets(coindexedResults)
      paraphraseGold <- config.readGoldParaphrases
      evaluationItems <- config.getEvaluationItems
      _ <- EvalApp.runEvaluation(evalSet, evaluationItems.toSet, coindexedResults, paraphraseGold)
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val experimentNameO = Opts.option[String](
      "name", metavar = "path", help = "Relative path to the output directory."
    )
    val trainOnDevO = Opts.flag(
      "dev", help = "Run on the dev set for fast iteration."
    ).orFalse
    val testOnTestO = Opts.flag(
      "test", help = "Evaluate on the test set instead of dev."
    ).orFalse

    (experimentNameO, trainOnDevO, testOnTestO).mapN(program)
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
