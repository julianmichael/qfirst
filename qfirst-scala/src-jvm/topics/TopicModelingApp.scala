package qfirst.topics

import qfirst._
import qfirst.paraphrase._
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

object TopicModelingApp extends IOApp {

  import ClauseResolution._

  val genericInflectedForms = InflectedForms(
    "verb".lowerCase,
    "verbs".lowerCase,
    "verbed".lowerCase,
    "verbing".lowerCase,
    "verbed".lowerCase)

  def getUnindexedInstance(verb: VerbEntry) = {
    getResolvedFramePairs(verb.verbInflectedForms, filterGoldNonDense(verb)._2.toList.map(_._2.questionSlots)).foldMap(p =>
      Map(getClauseTemplate(p._1) -> 1)
    )
  }

  case class ClauseVocab(
    indexToClause: Vector[ArgStructure],
    clauseToIndex: Map[ArgStructure, Int]
  ) {
    def size = indexToClause.size
    def makeInstance(verb: VerbEntry) = {
      getUnindexedInstance(verb).map { case (clause, count) =>
        clauseToIndex(clause) -> count
      }
    }
  }

  def makeClauseVocab(dataset: Dataset) = {
    val verbs = Dataset.verbEntries.getAll(dataset)
    val unindexedClauseInstances = verbs.map(getUnindexedInstance)
    val indexToClause = unindexedClauseInstances.foldMap(_.keySet).toVector
    ClauseVocab(indexToClause, indexToClause.zipWithIndex.toMap)
  }

  case class VerbVocab(
    indexToVerb: Vector[InflectedForms],
    verbToIndex: Map[InflectedForms, Int]
  ) {
    def size = indexToVerb.size
  }

  def makeVerbVocab(dataset: Dataset) = {
    val verbs = Dataset.verbEntries.getAll(dataset)
      .map(_.verbInflectedForms).toSet.toVector
    VerbVocab(verbs, verbs.zipWithIndex.toMap)
  }

  object MixtureOfUnigramsApp {
    import MixtureOfUnigrams.UnigramMixtureModel
    def run(vocab: ClauseVocab, data: Dataset, numFrames: Int = 100, rand: Random) = {
      val verbs = Dataset.verbEntries.getAll(data)
      print("Preparing instances... ")
      val instances = verbs.map(vocab.makeInstance)
      println("Done.")

      val (model, assignments, nll) = MixtureOfUnigrams.runSoftEM(
        initModel = UnigramMixtureModel.initClever(
          instances, numFrames, vocab.size, 1.0, rand
        ),
        instances = instances,
        priorConcentrationParameter = 1.0,
        clusterConcentrationParameter = 1.0,
        stoppingThreshold = 0.001,
      )
      (model, verbs.zip(assignments).toVector, nll)
    }

    def printResults(vocab: ClauseVocab, model: UnigramMixtureModel, assignments: Vector[(VerbEntry, Vector[Double])]) = {
      val frames = model.clusters.map { dist =>
        dist.zipWithIndex.map { case (prob, index) =>
          vocab.indexToClause(index) -> prob
        }.sortBy(-_._2)
      }
      def getClause(argStructure: ArgStructure) = {
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
      model.prior.zip(frames).zipWithIndex.sortBy(-_._1._1).foreach {
        case ((frameProb, frame), frameIndex) =>
          println(f"Frame $frameIndex%s: $frameProb%.6f")
          frame.takeWhile(_._2 > clauseInclusionThreshold)
            .foreach { case (argStructure, clauseProb) =>
              println(f"\t$clauseProb%5.4f ${getClause(argStructure)}")
            }
          println("\t##### Example Instances #####")
          assignments
            .groupBy(_._1.verbInflectedForms).toList
            .flatMap { case (forms, pairs) => pairs.maximaBy(_._2(frameIndex)).headOption }
            .sortBy(-_._2(frameIndex)).take(5).foreach {
              case (verb, topicDist) =>
                println(f"\t${topicDist(frameIndex)}%5.4f ${verb.verbInflectedForms.stem}")
                val framePairs = getResolvedFramePairs(verb.verbInflectedForms, filterGoldNonDense(verb)._2.toList.map(_._2.questionSlots))
                val probs = framePairs.map(p =>
                  model.clusters(frameIndex)(
                    vocab.clauseToIndex(getClauseTemplate(p._1))
                  )
                )
                probs.zip(framePairs).foreach { case (prob, (frame, slot)) =>
                  val qString = frame.questionsForSlot(slot).head
                  println(f"\t\t$prob%5.4f $qString%s")
                }
            }
      }
    }
  }

  val frameInclusionMinimum = 1
  val clauseInclusionMinimum = 2
  val frameInclusionThreshold = 0.04
  val clauseInclusionThreshold = 0.01

  object PLSIApp {
    import PLSI.PLSIModel
    def run(verbVocab: VerbVocab, clauseVocab: ClauseVocab, data: Dataset, numFrames: Int = 100, rand: Random) = {
      val sentenceVerbPairs = data.sentences.values.toList.flatMap(sentence =>
        sentence.verbEntries.values.toList.map(sentence.sentenceId -> _)
      )
      val pairsByVerbType = sentenceVerbPairs.groupBy(_._2.verbInflectedForms)
      val verbsByType = pairsByVerbType.map { case (v, ps) => v -> ps.map(_._2) }
      val locationsByVerb = pairsByVerbType.map { case (v, ps) => v -> ps.map(p => p._1 -> p._2.verbIndex) }

      print("Preparing instances... ")
      val documents = (verbVocab.indexToVerb: Vector[InflectedForms]).map { verb =>
        verbsByType(verb)
          .iterator
          .map(clauseVocab.makeInstance)
          .toVector
      }
      println("Done.")

      print("Initializing model... ")
      val initModel = PLSI.PLSIModel.initClever(
        documents, numFrames, clauseVocab.size, rand
      )
      println("Done.")
      val (model, assignments, nll) = PLSI.runSoftEM(
        initModel = initModel,
        documents = documents,
        stoppingThreshold = 0.001
      )
      (model, assignments, locationsByVerb, nll)
    }

    def save(
      verbFreqs: Map[InflectedForms, Int],
      verbVocab: VerbVocab, clauseVocab: ClauseVocab,
      model: PLSIModel,
      assignments: Vector[Vector[Vector[Double]]],
      locationsByVerb: Map[InflectedForms, List[(String, Int)]],
      path: Path
    ) = {
      val allFrames: Map[InflectedForms, VerbFrameset] = model.priors.zipWithIndex
        .iterator.map { case (frameDist, verbIndex) =>
          val verbInflectedForms = verbVocab.indexToVerb(verbIndex)
          val sentencesAndAssignmentsForVerb = locationsByVerb(verbInflectedForms).map(_._1).zip(assignments(verbIndex))
          val framesByProb = frameDist.zipWithIndex.sortBy(-_._1)
          val numFrames = math.max(framesByProb.takeWhile(_._1 > frameInclusionThreshold).size, frameInclusionMinimum)
          val frames = framesByProb.take(numFrames).map {
            case (frameProb, frameIndex) =>
              val sentenceIds = sentencesAndAssignmentsForVerb.flatMap {
                case (sid, frameDist) =>
                  val frameMaxProb = frameDist.max
                  if(frameMaxProb == frameDist(frameIndex) && frameMaxProb > 0.20) Some(SentenceId.fromString(sid))
                  else None
              }
              val clauseDist = model.clusters(frameIndex)
              val clausesByProb = clauseDist.zipWithIndex.sortBy(-_._1)
              val numClauses = math.max(clausesByProb.takeWhile(_._1 > clauseInclusionThreshold).size, clauseInclusionMinimum)
              val clauses = clausesByProb.take(numClauses).map {
                case (clauseProb, clauseIndex) =>
                  FrameClause(clauseVocab.indexToClause(clauseIndex), Map(), clauseProb) // TODO add argument sigils? need other output file.
              }.toList
              VerbFrame(clauses, frameProb)
          }.toList
          val frameset = VerbFrameset(verbInflectedForms, frames)
          verbInflectedForms -> frameset
      }.toMap
      // val data = VerbFrameData(verbFreqs, allFrames)
      // FileUtil.writeJson(path, io.circe.Printer.noSpaces)(data).unsafeRunSync
      // TODO update for new stuff
    }

    def saveForQA(
      dataset: Dataset,
      verbFreqs: Map[InflectedForms, Int],
      verbVocab: VerbVocab, clauseVocab: ClauseVocab,
      model: PLSIModel,
      assignments: Vector[Vector[Vector[Double]]],
      locationsByVerb: Map[InflectedForms, List[(String, Int)]], // verb type -> (sid, verb index)
      savePath: Path
    ) = {
      val sentenceJsons = dataset.sentences.iterator.map { case (sentenceId, sentence) =>
        Json.obj(
          "sentenceId" -> sentenceId.asJson,
          "sentenceTokens" -> sentence.sentenceTokens.asJson,
          "verbs" -> sentence.verbEntries.values.toList.flatMap { verb =>
            val instanceIndex = locationsByVerb(verb.verbInflectedForms).indexOf(sentenceId -> verb.verbIndex)
            val frameDist = assignments(verbVocab.verbToIndex(verb.verbInflectedForms))(instanceIndex)
            val bestFrames = frameDist.zipWithIndex.maximaBy(_._1)
            if(bestFrames.size != 1) None else Some {
              val bestFrameClauseDist = model.clusters(bestFrames.head._2)
              val clausesByProb = bestFrameClauseDist.zipWithIndex.sortBy(-_._1)
              val numClauses = math.max(clausesByProb.takeWhile(_._1 > clauseInclusionThreshold).size, clauseInclusionMinimum)
              val clauses = clausesByProb.take(numClauses).map(_._2).flatMap { clauseIndex =>
                val clauseTemplate = clauseVocab.indexToClause(clauseIndex)
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
          }.asJson
        )
      }.toList
      FileUtil.writeJsonLines(savePath, io.circe.Printer.noSpaces)(sentenceJsons).unsafeRunSync
    }

    def printResults(
      verbFreqs: Map[InflectedForms, Int],
      verbVocab: VerbVocab, clauseVocab: ClauseVocab, model: PLSIModel, assignments: Vector[Vector[Vector[Double]]]
    ) = {
      val frames = model.clusters.map { dist =>
        dist.zipWithIndex.map { case (prob, index) =>
          clauseVocab.indexToClause(index) -> prob
        }.sortBy(-_._2)
      }
      def getClause(forms: InflectedForms, argStructure: ArgStructure) = {
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
      val avgPrior = model.priors.transpose.map(p => mean(p.toVector))
      def framePrintLines(frame: Vector[(ArgStructure, Double)], frameProb: Double, frameIndex: Int) = {
        val numToPrint = math.max(frame.takeWhile(_._2 > clauseInclusionThreshold).size, clauseInclusionMinimum)
        f"Frame $frameIndex%s: $frameProb%.6f" +: frame.take(numToPrint)
          .map { case (argStructure, clauseProb) =>
            f"\t$clauseProb%5.4f ${getClause(genericInflectedForms, argStructure)}"
          }
      }
      avgPrior.zip(frames).zipWithIndex.sortBy(-_._1._1).foreach {
        case ((frameProb, frame), frameIndex) =>
          framePrintLines(frame, frameProb, frameIndex).foreach(println)
          println("\t###### Example Verbs: ######")
          val certainVerbs = model.priors.zipWithIndex.sortBy(-_._1(frameIndex)).take(10)
            .map { case (dist, verbIndex) => f"\t${dist(frameIndex)}%.4f ${verbVocab.indexToVerb(verbIndex).stem}" }
            .foreach(println)
      }
      verbFreqs.toVector.sortBy(-_._2).foreach { case (verbInflectedForms, verbCount) =>
        println(verbInflectedForms.allForms.mkString(", ") + s"($verbCount)")
        val verbPriors = model.priors(verbVocab.verbToIndex(verbInflectedForms))
          .zipWithIndex.sortBy(-_._1)
        val numToPrint = math.max(verbPriors.takeWhile(_._1 > frameInclusionThreshold).size, frameInclusionMinimum)
        verbPriors
          .take(numToPrint)
          .foreach { case (frameProb, frameIndex) =>
            val frame = frames(frameIndex)
            framePrintLines(frame, frameProb, frameIndex).map("\t" + _).foreach(println)
          }
      }
    }
  }

  lazy val trainOrig = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz"))
  lazy val trainExpanded = Data.readDataset(Paths.get("qasrl-v2_1").resolve("expanded").resolve("train.jsonl.gz"))
  lazy val dev = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("dev.jsonl.gz"))
  lazy val devMini = Data.readDataset(Paths.get("dev-mini.jsonl.gz"))

  // actual running stuff

  def run(args: List[String]): IO[ExitCode] = IO {
    val algorithm = args(0)
    val dataset = args.lift(1).getOrElse("dev-mini") match {
      case "train-orig" => trainOrig
      case "train" | "train-expanded" => trainExpanded
      case "dev" => dev
      case "dev-mini" => devMini
    }
    val clauseVocab = makeClauseVocab(dataset)
    val numFrames = args(2).toInt
    val frameSavePath = Paths.get(args(3))
    val qaInputSavePath = Paths.get(args(4))
    val rand = args.lift(5).fold(new Random)(seed => new Random(seed.toLong))
    // val pathOpt = args.lift(3).map(Paths.get(_))
    algorithm match {
      case "mix-unigrams" =>
        val (model, assignments, nll) = MixtureOfUnigramsApp.run(clauseVocab, dataset, numFrames, rand)
        println(s"Loss: $nll")
        MixtureOfUnigramsApp.printResults(clauseVocab, model, assignments)
      case "plsi" =>
        val verbFreqs = Dataset.verbEntries.getAll(dataset).groupBy(_.verbInflectedForms)
          .map { case (forms, instances) => forms -> instances.size }
        val verbVocab = makeVerbVocab(dataset)
        val (model, assignments, locationsByVerb, nll) = PLSIApp.run(verbVocab, clauseVocab, dataset, numFrames, rand)
        println(s"Loss: $nll")
        PLSIApp.printResults(verbFreqs, verbVocab, clauseVocab, model, assignments)
        PLSIApp.save(verbFreqs, verbVocab, clauseVocab, model, assignments, locationsByVerb, frameSavePath)
        PLSIApp.saveForQA(dataset, verbFreqs, verbVocab, clauseVocab, model, assignments, locationsByVerb, qaInputSavePath)
    }
  }.as(ExitCode.Success)
}
