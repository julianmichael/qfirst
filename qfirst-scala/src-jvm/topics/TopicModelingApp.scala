package qfirst.topics

import qfirst._
import qfirst.frames.implicits._

import cats.Id
import cats.implicits._

import java.nio.file._

import qasrl._
import qasrl.data._
import qasrl.labeling._
import qasrl.util._
import qasrl.util.implicits._

import qasrl.bank._

import nlpdata.datasets.wiktionary._
import nlpdata.util.LowerCaseStrings._

import io.circe.generic.JsonCodec

object TopicModelingApp extends App {

  @JsonCodec case class ArgStructure(
    args: DependentMap[ArgumentSlot.Aux, Id],
    isPassive: Boolean
  ) {
    def forgetAnimacy = {
      val newArgs = args.keys.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id]) {
        (m, k) => k match {
          case Subj   => m.put(Subj, Noun(false))
          case Obj    => m.put(Obj, Noun(false))
          case Obj2  => m.put(
            Obj2, args.get(Obj2).get match {
              case Noun(_) => Noun(false)
              case Prep(p, Some(Noun(_))) => Prep(p, Some(Noun(false)))
              case x => x
            }
          )
          case Adv(wh) => m.put(Adv(wh), args.get(Adv(wh)).get)
        }
      }
      this.copy(args = newArgs)
    }
  }


  import MixtureOfUnigrams.UnigramMixtureModel
  @JsonCodec case class ModelData(
    model: UnigramMixtureModel,
    indexToClause: Vector[ArgStructure],
    topicAssignments: List[Vector[Double]]
  ) {
    val clauseToIndex: Map[ArgStructure, Int] = indexToClause.zipWithIndex.toMap
    val numClauses = indexToClause.size
  }

  def run(data: Dataset, numFrames: Int = 100) = {
    val verbs = Dataset.verbEntries.getAll(data)
    val unindexedClauseInstances = {
      import ClauseResolution._
        verbs.map { verb =>
        val qLabels = verb.questionLabels.values.toList
        val frameSets = qLabels.map(_.questionString)
          .map(getFramesWithAnswerSlots(verb.verbInflectedForms, _))
        val locallyResolvedFramePairSets = locallyResolve(frameSets)
        val resolvedFramePairs = qLabels.map(_.questionSlots)
          .zip(locallyResolvedFramePairSets)
          .map(Function.tupled(fallbackResolve(_, _)))
        resolvedFramePairs.foldMap(p => Map(ArgStructure(p._1.args, p._1.isPassive).forgetAnimacy -> 1))
      }
    }

    val (numClauses, clauseToIndex, indexToClause) = {
      val i2c = unindexedClauseInstances.foldMap(_.keySet).toVector
      val c2i = i2c.zipWithIndex.toMap
      (i2c.size, c2i, i2c)
    }

    val indexedClauseInstances = unindexedClauseInstances.map(
      _.map { case (clause, count) => clauseToIndex(clause) -> count }
    )

    val (model, assignments, nll) = MixtureOfUnigrams.runSoftEM(
      initModel = UnigramMixtureModel.init(
        numFrames, numClauses, new scala.util.Random(2343652L)
      ),
      instances = indexedClauseInstances,
      stoppingThreshold = 0.001
    )
    ModelData(model, indexToClause, assignments) -> nll
  }

  def printResults(modelData: ModelData) = {
    val frames = modelData.model.clusters.map { dist =>
      dist.zipWithIndex.map { case (prob, index) =>
        modelData.indexToClause(index) -> prob
      }.sortBy(-_._2)
    }
    def getClause(argStructure: ArgStructure) = {
      val genericInflectedForms = InflectedForms(
        "verb".lowerCase,
        "verbs".lowerCase,
        "verbed".lowerCase,
        "verbing".lowerCase,
        "verbed".lowerCase)
      val f = Frame(
        verbInflectedForms = genericInflectedForms,
        args = argStructure.args,
        tense = PresentTense,
        isPerfect = false,
        isProgressive = false,
        isPassive = argStructure.isPassive,
        isNegated = false
      )
      f.clauses.head
    }
    modelData.model.prior.zip(frames).sortBy(-_._1).zipWithIndex.foreach {
      case ((frameProb, frame), frameIndex) =>
        println(f"Frame $frameIndex%s: $frameProb%.6f")
        // TODO include some example verbs
        frame.takeWhile(_._2 > clauseProbThreshold)
          .foreach { case (argStructure, clauseProb) =>
            println(f"$clauseProb%7.4f ${getClause(argStructure)}")
          }
    }
  }

  lazy val train = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz"))
  lazy val dev = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("dev.jsonl.gz"))
  lazy val devTiny = Data.readDataset(Paths.get("dev-mini.jsonl.gz"))
  val dataset = args.lift(0).getOrElse("devTiny") match {
    case "train" => train
    case "dev" => dev
    case "devTiny" => devTiny
  }
  val numFrames = args(1).toInt
  val pathOpt = args.lift(2).map(Paths.get(_))
  val clauseProbThreshold = 0.05
  val modelData = pathOpt.flatMap(p =>
    scala.util.Try(FileUtil.readJson[ModelData](p).unsafeRunSync).toOption
  ).getOrElse {
    val (modelData, nll) = run(dataset, numFrames)
    println(s"Loss: $nll")
    modelData
  }
  printResults(modelData)
  // TODO maybe some example assignments etc.

}
