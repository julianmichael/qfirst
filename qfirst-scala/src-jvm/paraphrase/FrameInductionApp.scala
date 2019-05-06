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
import cats.data.Validated
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

  // type QABeam = List[SimpleQAs.BeamItem[SlotBasedLabel[VerbForm]]]
  type Instances = Map[InflectedForms, Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]
  // type ResolvedInstances = Map[InflectedForms, Map[SentenceId, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]]

  def makeVerbVocab(instances: Instances): Vocab[InflectedForms] = {
    Vocab.make(instances.keySet)
  }
  // def makeClauseVocab(instances: Instances): Vocab[ArgStructure] = {
  //   val qSlotsSet = instances.values.iterator.flatMap(sentMap =>
  //     sentMap.values.iterator.flatMap(verbMap =>
  //       verbMap.values.iterator.flatMap(qMap =>
  //         qMap.keySet.iterator
  //       )
  //     )
  //   ).toSet
  //   val clauseTemplateSet = qSlotsSet.flatMap(slots =>
  //     ClauseResolution.getFramesWithAnswerSlots(slots)
  //       .map(_._1).map(ClauseResolution.getClauseTemplate)
  //   )
  //   Vocab.make(clauseTemplateSet)
  // }

  def makeVerbSpecificClauseVocab(instances: Map[String, Map[Int, Map[SlotBasedLabel[VerbForm], Set[AnswerSpan]]]]): Vocab[ArgStructure] = {
    Vocab.make(
      instances.values.toList.foldMap(verbMap =>
        verbMap.values.toList.foldMap(qMap =>
          ClauseResolution.getResolvedStructures(qMap.keys.toList).map(_._1).toSet
        )
      )
    )
  }

  // def getGoldInstances(dataset: Dataset): Instances = {
  //   dataset.sentences
  //     .iterator.flatMap { case (sid, sentence) => sentence.verbEntries.values.map(sid -> _) }.toList
  //     .groupBy(_._2.verbInflectedForms).map { case (verbInflectedForms, pairs) =>
  //       verbInflectedForms -> pairs.groupBy(_._1).map { case (sid, pairs) =>
  //         sid -> pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
  //           verbIndex -> filterGoldNonDense(verb)._2.map { case (qString, qLabel) =>
  //             qLabel.questionSlots -> (
  //               qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
  //             )
  //           }
  //         }
  //       }
  //     }
  // }

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
        // omit elmo vectors for verbs filtered out of the dataset
        dataset.sentences.get(sentenceId)
          .flatMap(_.verbEntries.get(verbIndex))
          .map(_.verbInflectedForms).foldMap(verbForms =>
            Map(verbForms -> Map(sentenceId -> Map(verbIndex -> List(embedding))))
          )
      }
    ).map(_.head)
  }

  import qfirst.paraphrase.models._
  import breeze.stats.distributions.Multinomial

  // def runVerbWiseSoftEMWithComposite(
  //   instances: Instances,
  //   elmoVecs: ClusteringInstances[DenseVector[Float]],
  //   rand: Random
  // ): IO[Map[InflectedForms, VerbFrameset]] = {
  //   val algorithm = new CompositeClusteringAlgorithm {
  //     val _1 = DirichletMAPClustering
  //     val _2 = VectorMeanClustering
  //     val lambda = 0.0
  //   }
  //   val verbCounts = instances.map { case (forms, sentMap) => forms -> sentMap.iterator.map(_._2.size).sum }
  //   val verbs = instances.map { case (forms, sentMap) =>
  //     forms -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
  //       verbIndices.keys.toVector
  //         .filter(vi =>
  //           instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).exists(_.nonEmpty) &&
  //             elmoVecs.instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
  //         ).map(vi => VerbId(sid, vi))
  //     }
  //   }.filter(_._2.nonEmpty)

  //   verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
  //     val verbInstances = instances(verbInflectedForms)
  //     val clauseVocab = makeVerbSpecificClauseVocab(verbInstances)
  //     if(clauseVocab.size == 0) {
  //       println(verbInflectedForms)
  //       println(verbIds)
  //       println(verbInstances)
  //       println(verbCounts(verbInflectedForms))
  //     }

  //     runVerbWiseSoftEM(algorithm)(
  //       verbInflectedForms = verbInflectedForms,
  //       verbIds = verbIds,
  //       makeInstance = (verbId: VerbId) => {
  //         val clauseCounts = ClauseResolution.getResolvedStructures(
  //           verbInstances(verbId.sentenceId)(verbId.verbIndex).keys.toList
  //         ).foldMap(ct => Map(clauseVocab.getIndex(ct._1) -> 1))
  //         val vector = elmoVecs.instances(verbInflectedForms)(verbId.sentenceId)(verbId.verbIndex)
  //         clauseCounts -> vector
  //       },
  //       hyperparams = (DirichletMAPClustering.Hyperparams(clauseVocab.size, 0.1), ()),
  //       numFrames = math.max(2, math.round(math.log(verbCounts(verbInflectedForms).toDouble) / math.log(10)).toInt), // can change, idk
  //       getFrame = {
  //         case (multinomial, _) => (multinomial.params / multinomial.sum).toScalaVector.toList.zipWithIndex
  //             .filter(_._1 > 0.01).map {
  //               case (prob, index) => FrameClause(clauseVocab.getItem(index), prob)
  //             }
  //       },
  //       rand = rand
  //     ).map(verbInflectedForms -> _)
  //   }.map(_.toMap)
  // }

  // def runVerbWiseSoftEM(
  //   algorithm: ClusteringAlgorithm)(
  //   verbInflectedForms: InflectedForms,
  //   verbIds: Vector[VerbId],
  //   makeInstance: VerbId => algorithm.Instance,
  //   hyperparams: algorithm.Hyperparams,
  //   numFrames: Int,
  //   getFrame: algorithm.ClusterParam => List[FrameClause],
  //   rand: Random,
  //   shouldLog: Boolean = false
  // ): IO[VerbFrameset] = {
  //   val instances = verbIds.map(makeInstance)
  //   for {
  //     modelInit <- {
  //       val init = IO(algorithm.initPlusPlus(instances, hyperparams, math.min(numFrames, instances.size)))
  //       if(shouldLog) logOp("Initializing model", init)
  //       else init
  //     }
  //     (model, assignments, _) <- IO(
  //       algorithm.runSoftEM(
  //         initModel = modelInit,
  //         instances = instances,
  //         hyperparams = hyperparams,
  //         stoppingThreshold = 0.001,
  //         shouldLog = shouldLog
  //       )
  //     )
  //   } yield {
  //     val prior = Multinomial(assignments.iterator.map(m => m.params / m.sum).reduce(_ + _))
  //     val frames = model.map(getFrame).zipWithIndex.map { case (frameClauses, clusterIndex) =>
  //       VerbFrame(frameClauses, null /* TODO: replace with dummy merge tree */, prior.probabilityOf(clusterIndex))
  //     }
  //     val instanceProbs = verbIds.zip(assignments).foldMap { case (verbId, assignmentProbs) =>
  //       Map(verbId -> assignmentProbs.params.toScalaVector)
  //     }
  //     VerbFrameset(verbInflectedForms, frames.toList, instanceProbs)
  //   }
  // }

  type ClausalQ = (ArgStructure, ArgumentSlot)
  import breeze.linalg._
  import scala.collection.immutable.Vector

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
    // verbSenseModel: CalibratedVerbSenseModel,
    predicateSenseResults: Map[InflectedForms, VerbFrameset],
    fuzzyArgEquivalences: Map[InflectedForms, Map[(ClausalQ, ClausalQ), Double]]
  ): IO[Map[InflectedForms, VerbFrameset]] = IO {
    // verbSenseModel.clustersByVerb.transform { case (verbForms, tree) =>
    //   val clusterTrees = tree.splitByPredicate(_.loss > verbSenseModel.maxLoss)
    //   val verbRel = fuzzyArgEquivalences(verbForms)
    //   val verbFrames = clusterTrees.map { tree =>
    //     val verbIds = tree.values
    //   }
    // }

    // model: VerbSenseConfig,
    // clustersByVerb: Map[InflectedForms, MergeTree[VerbId]],
    // maxLoss: Double,
    // minClauseProb: Double

    predicateSenseResults.transform { case (verbForms, verbFrameset) =>
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
              val resOpt = verbRel.get(qi -> qj).orElse(verbRel.get(qj -> qi))
              resOpt.getOrElse {
                println(s"XXX: $qi \t $qj")
                0.5 // shouldn't happen
              }
            }
          )
          fuzzyEquivMatrix.update(i, j, score)
        }
        // clausalQVocab.items.zipWithIndex.foreach(println)
        // for(i <- (0 until clausalQVocab.size)) {
        //   print(f"$i%9d ")
        //   for(j <- (0 to i)) { print(f"${fuzzyEquivMatrix(i, j)}%9.2f") }
        //   println
        // }
        // println("          " + (0 until clausalQVocab.size).map(x => f"$x%9d").mkString)
        // println(fuzzyEquivMatrix)
        val (mergeTree, _) = CompleteLinkageClustering.runAgglomerativeClustering(indices, fuzzyEquivMatrix)
        frame.copy(coindexingTree = mergeTree.map(clausalQVocab.getItem))
      }
      verbFrameset.copy(frames = coindexedFrames)
    }
  }

  def runVerbWiseAgglomerative(
    algorithm: ClusteringAlgorithm)(
    verbInflectedForms: InflectedForms,
    verbIds: Vector[VerbId],
    makeInstance: VerbId => algorithm.Instance,
    hyperparams: algorithm.Hyperparams,
  ): IO[MergeTree[VerbId]] = {
    val instances = verbIds.map(makeInstance)
    IO(println(s"Clustering verb: ${verbInflectedForms.stem}")) >> IO(
        algorithm.runAgglomerativeClustering(instances, hyperparams)._1
          .map(verbIds)
    )
  }

  // def runVerbWiseAgglomerativeWithComposite(
  //   instances: Instances,
  //   elmoVecs: ClusteringInstances[DenseVector[Float]],
  //   interpolation: Double = 0.5,
  //   rand: Random
  // ): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
  //   val algorithm = new CompositeClusteringAlgorithm {
  //     val _1 = MinEntropyClustering
  //     val _2 = VectorMeanClustering
  //     val lambda = interpolation
  //   }
  //   val verbs = instances.map { case (forms, sentMap) =>
  //     forms -> sentMap.toVector.flatMap { case (sid, verbIndices) =>
  //       verbIndices.keys.toVector
  //         .filter(vi =>
  //           instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty &&
  //             elmoVecs.instances.get(forms).flatMap(_.get(sid)).flatMap(_.get(vi)).nonEmpty
  //         )
  //         .map(vi => VerbId(sid, vi))
  //     }
  //   }.filter(_._2.nonEmpty)
  //   val clauseVocab = makeClauseVocab(instances)
  //   val makeInstancePair = (forms: InflectedForms, verbId: VerbId) => {
  //     val questions = instances(forms)(verbId.sentenceId)(verbId.verbIndex).keySet.toList
  //     val clauseCounts = ClauseResolution.getResolvedFramePairs(
  //       forms, questions
  //     ).map(_._1).map(ClauseResolution.getClauseTemplate)
  //       .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
  //     val vector = elmoVecs.instances(forms)(verbId.sentenceId)(verbId.verbIndex)
  //     clauseCounts -> vector
  //   }
  //   runVerbWiseAgglomerative(algorithm)(
  //     verbs, makeInstance, (algorithm._1.Hyperparams(clauseVocab.size), ())
  //   )
  // }

  def runVerbSenseAgglomerativeClustering(
    verbSenseConfig: VerbSenseConfig,
    instances: Instances,
    elmoVecs: ClusteringInstances[DenseVector[Float]]
  ): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
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
    verbs.toList.traverse { case (verbInflectedForms, verbIds) =>
      val verbInstances = instances(verbInflectedForms)
      val clauseVocab = makeVerbSpecificClauseVocab(verbInstances)
      val makeInstance = (verbId: VerbId) => {
        val questions = verbInstances(verbId.sentenceId)(verbId.verbIndex).keySet.toList
        val clauseCounts = ClauseResolution.getResolvedFramePairs(
          verbInflectedForms, questions
        ).map(_._1).map(ClauseResolution.getClauseTemplate)
          .foldMap(c => Map(clauseVocab.getIndex(c) -> 1))
        val vector = elmoVecs.instances(verbInflectedForms)(verbId.sentenceId)(verbId.verbIndex)
        clauseCounts -> vector
      }
      val clustering = verbSenseConfig match {
        case VerbSenseConfig.EntropyOnly =>
          runVerbWiseAgglomerative(MinEntropyClustering)(
            verbInflectedForms, verbIds, (v => makeInstance(v)._1), MinEntropyClustering.Hyperparams(clauseVocab.size)
          )
        case VerbSenseConfig.ELMoOnly =>
          runVerbWiseAgglomerative(VectorMeanClustering)(
            verbInflectedForms, verbIds, (v => makeInstance(v)._2), ()
          )
        case VerbSenseConfig.Interpolated(lambda) =>
          val algorithm = new CompositeClusteringAlgorithm {
            val _1 = MinEntropyClustering; val _2 = VectorMeanClustering
          }
          runVerbWiseAgglomerative(algorithm)(
            verbInflectedForms, verbIds, makeInstance,
            algorithm.Hyperparams(MinEntropyClustering.Hyperparams(clauseVocab.size), (), lambda)
          )
      }
      clustering.map(verbInflectedForms -> _)
    }.map(_.toMap)
  }


  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  def getVerbSenseClusters(config: Config, verbSenseConfig: VerbSenseConfig): IO[Map[InflectedForms, MergeTree[VerbId]]] = {
    config.getCachedVerbClusters(verbSenseConfig).flatMap {
      case Some(verbClusters) => IO.pure(verbClusters)
      case None => for {
        fullInstances <- config.fullInstances.get
        trainSet <- config.input.get
        evalSet <- config.eval.get
        trainElmoVecs <- getGoldELMoInstances(trainSet, config.inputElmoPrefix)
        evalElmoVecs <- getGoldELMoInstances(evalSet, config.evalElmoPrefix)
        verbClusters <- runVerbSenseAgglomerativeClustering(
          verbSenseConfig = verbSenseConfig,
          instances = fullInstances,
          elmoVecs = trainElmoVecs |+| evalElmoVecs
        )
        _ <- config.cacheVerbClusters(verbSenseConfig, verbClusters)
      } yield verbClusters
    }
  }

  import qfirst.metrics.BinaryConf

  case class EvaluationPoint(
    model: VerbSenseConfig,
    maxLoss: Double,
    minClauseProb: Double,
    lbConf: BinaryConf.Stats,
    ubConf: BinaryConf.Stats
  )

  case class CalibratedVerbSenseModel(
    model: VerbSenseConfig,
    clustersByVerb: Map[InflectedForms, MergeTree[VerbId]],
    maxLoss: Double,
    minClauseProb: Double
  )

  def evaluateVerbClusters(
    config: Config,
    verbClustersByConfig: Map[VerbSenseConfig, Map[InflectedForms, MergeTree[VerbId]]],
    goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
    evaluationItems: Vector[(InflectedForms, String, Int)]
  ): IO[Map[VerbSenseConfig, CalibratedVerbSenseModel]] = {
    val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
      goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
    }
    if(presentEvaluationItems.isEmpty) IO.pure {
      verbClustersByConfig.transform { case (vsConfig, verbClusters) =>
        CalibratedVerbSenseModel(vsConfig, verbClusters, verbClusters.values.map(_.loss).max, 0.0) // better max than inf i guess
      }
    } else {
      for {
        instances <- config.fullInstances.get
        fullEvaluationResultsByConfig = verbClustersByConfig.transform { case (vsConfig, clustersByVerb) =>
          presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
            val verbClauseSets = instances(verbInflectedForms).flatMap { case (sid, verbMap) =>
              verbMap.map { case (vi, qMap) =>
                val questions = qMap.keySet.toList
                val clauses = ClauseResolution.getResolvedFramePairs(
                  verbInflectedForms, questions
                ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
                VerbId(sid, vi) -> clauses
              }
            }
            val inputClauseSet = verbClauseSets(VerbId(sentenceId, verbIndex))

            val verbClusters = clustersByVerb(verbInflectedForms)
            val instanceClusters = verbClusters.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present
                                                                                                    // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
            val clusterMetricsByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = instanceClusters.map { tree =>
              val clusterSize = tree.size
              val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss
              val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
                verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
              }
              val predictedClausesWithProbsIncreasing = clauseCounts
                .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
                .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
                .sortBy(_._2)
              val confsForClauseThresholds = predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
                val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
                val predictedClauses = clausesWithProbs.map(_._1).toList.toSet
                val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
                val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
                val lbConf = BinaryConf.Stats(
                  tp = (predictedClauses intersect correctClauses).size,
                  tn = 0, // we don't need this for p/r/f
                  fp = (predictedClauses -- correctClauses).size,
                  fn = (correctClauses -- predictedClauses).size
                )
                val ubConf = BinaryConf.Stats(
                  tp = (predictedClauses -- incorrectClauses).size,
                  tn = 0, // we don't need this for p/r/f
                  fp = (predictedClauses intersect incorrectClauses).size,
                  fn = (correctClauses -- predictedClauses).size
                )
                clauseThreshold -> (lbConf -> ubConf)
              }.toList
              lossThreshold -> confsForClauseThresholds
            }
            clusterMetricsByThresholds
          }
        }
        allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
          val lossThresholds = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
          lossThresholds.flatMap { maxLoss =>
            val chosenClusters = evaluationItemResults.map(
              _.find(_._1 <= maxLoss).get._2
            )
            val clauseProbThresholds = chosenClusters.flatMap(_.map(_._1)).toSet
            clauseProbThresholds.map { minClauseProb =>
              val confPairs = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
              val lbConf = confPairs.foldMap(_._1)
              val ubConf = confPairs.foldMap(_._2)
              EvaluationPoint(vsConfig, maxLoss, minClauseProb, lbConf, ubConf)
            }
          }
        }
        bestModels = allPointsByModel.transform { case (vsConfig, allPoints) =>
          val lbBest = allPoints.maxBy(_.lbConf.f1)
          val lbBestModel = CalibratedVerbSenseModel(
            vsConfig, verbClustersByConfig(vsConfig), lbBest.maxLoss, lbBest.minClauseProb
          )
          val ubBest = allPoints.maxBy(_.ubConf.f1)
          val ubBestModel = CalibratedVerbSenseModel(
            vsConfig, verbClustersByConfig(vsConfig), ubBest.maxLoss, ubBest.minClauseProb
          )
          // (lbBestModel, lbBest.lbConf, ubBestModel, ubBest.ubConf)
          println(s"${vsConfig.modelName}: " + SandboxApp.getMetricsString(ubBest.ubConf))
          ubBestModel
        }
        // _ <- {
        //   import com.cibo.evilplot._
        //   import com.cibo.evilplot.numeric._
        //   import com.cibo.evilplot.plot._
        //   import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
        //   import com.cibo.evilplot.plot.renderers.PointRenderer

        //   case class DataPoint(
        //     model: VerbSenseConfig,
        //     recall: Double,
        //     precision: Double) extends Datum2d[DataPoint] {
        //     def x = recall
        //     def y = precision
        //     def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
        //   }

        // TODO
        // construct final graph points
        // create graph and write to file

        //   IO.unit
        // }
      } yield bestModels
    }
  }

  // TODO: elmo loss is ~175x greater; tune around this number
  // _ <- {
  //   import qfirst.metrics._
  //   val dist = Numbers(verbClusters.values.toVector.filterNot(_.loss == 0.0).map(tree => tree.loss / tree.size))
  //   IO(println(SandboxApp.getMetricsString(dist)))
  // }

  def program(config: Config, modelOpt: Option[VerbSenseConfig]): IO[ExitCode] = {
    val verbSenseConfigs = modelOpt.map(List(_)).getOrElse(
      List(VerbSenseConfig.EntropyOnly, VerbSenseConfig.ELMoOnly) ++
        (1 to 9).map(_.toDouble / 10.0).toList.map(VerbSenseConfig.Interpolated(_))
    )
    for {
      verbClustersByConfig <- verbSenseConfigs.traverse(vsConfig =>
        getVerbSenseClusters(config, vsConfig).map(vsConfig -> _)
      ).map(_.toMap)
      goldParaphrases <- config.readGoldParaphrases
      evaluationItems <- config.evaluationItems.get
      bestModels <- evaluateVerbClusters(config, verbClustersByConfig, goldParaphrases, evaluationItems)
      // bestVerbClustering <- evaluateAndOptimizeVerbClusters(config, verbClusters)
      // collapsedQAOutputs <- config.readCollapsedQAOutputs
      // coindexedResults <- runCollapsedCoindexing(bestVerbClustering, collapsedQAOutputs)
      // _ <- config.writeFramesets(coindexedResults)
      // _ <- EvalApp.runEvaluation(evalSet, evaluationItems.toSet, coindexedResults, paraphraseGold)
    } yield ExitCode.Success
  }

  val runFrameInduction = Command(
    name = "mill -i qfirst.jvm.runMain qfirst.paraphrase.FrameInductionApp",
    header = "Induce verb frames."
  ) {
    val modeO = Opts.option[String](
      "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
    ).mapValidated { string =>
      RunMode.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test."))
    }
    val verbSenseConfigOptO = Opts.option[String](
      "model", metavar = "entropy|elmo|<float>", help = "Verb sense model configuration."
    ).mapValidated { string =>
      VerbSenseConfig.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid model $string: must be entropy, elmo, or a float (interpolation param)."))
    }.orNone

    (modeO, verbSenseConfigOptO).mapN { (mode, verbSenseConfigOpt) =>
      program(Config(mode), verbSenseConfigOpt)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    runFrameInduction.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }
}
