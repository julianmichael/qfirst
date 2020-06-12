package qfirst.frame
import qfirst.model.eval.protocols.SimpleQAs

import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached
import qfirst.frame.util.NonMergingMap
import qfirst.frame.util.VectorFileUtil

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot
import qasrl.labeling.SlotBasedLabel

import qasrl.bank.Data
import qasrl.bank.FullData

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileUtil
import jjm.implicits._

import cats.Order
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

import monocle.function.{all => Optics}

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution

import freelog._
import freelog.implicits._

sealed abstract class Features[VerbType : Encoder : Decoder, Arg](
  val mode: RunMode)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]) {

  // overriden for propbank features
  def getIfPropBank: Option[PropBankFeatures[Arg]] = None

  type VerbFeats[A] = VerbFeatures[VerbType, A]
  def mapVerbFeats[A, B](f: A => B): VerbFeats[A] => VerbFeats[B] = {
    feats => feats.transform { case (_, instances) =>
      NonMergingMap(
        instances.value.transform { case (verbId, feat) =>
          f(feat)
        }
      )
    }
  }
  type ArgFeats[A] = ArgFeatures[VerbType, Arg, A]
  def makeArgFeats[A, B](f: (VerbId, A) => List[(Arg, B)]): VerbFeats[A] => ArgFeats[B] = {
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, feat) =>
        f(verbId, feat).foldMap { case (arg, argFeat) =>
          NonMergingMap(ArgumentId(verbId, arg) -> argFeat)
        }
      }
    }
  }

  type VerbFeatsNew[A] = VerbType => VerbId => A
  type ArgFeatsNew[A] = VerbType => ArgumentId[Arg] => A

  implicit protected val runMode = mode

  def splitName = RunData.strings.get

  // for constructing features
  def getVerbLemma(verbType: VerbType): String

  // for logging. could replace with Show instance?
  def renderVerbType(verbType: VerbType): String

  val sentences: RunDataCell[NonMergingMap[String, Vector[String]]]

  val verbArgSets: RunDataCell[VerbFeats[Set[Arg]]]

  def argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]]

  def argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]]

  // new style arg features
  def argIndices: RunData[ArgFeatsNew[Int]]

  def argSyntacticFunctions: RunDataCell[ArgFeats[String]]

  protected val rootDir: Path

  // TODO move all setup into this/superclass
  def setup: IO[Unit] = IO.unit

  def outDir = IO.pure(rootDir.resolve("out")).flatTap(createDir)

  // for use by frame induction etc.
  def modelDir = IO.pure(rootDir.resolve("models")).flatTap(createDir)

  // for inputs to feature computation
  protected def inputDir = rootDir.resolve("input")

  // for caching features that take a while to compute
  protected def cacheDir = IO.pure(rootDir.resolve("cache")).flatTap(createDir)

  // question and verb IDs

  lazy val args: RunDataCell[Map[VerbType, Set[ArgumentId[Arg]]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, args) =>
        args.map(arg => ArgumentId(verbId, arg))
      }
    }
  ).toCell("Argument IDs")

  lazy val verbs: RunDataCell[Map[VerbType, Set[VerbId]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) => verbs.value.keySet }
  ).toCell("Verb IDs")

  lazy val verbIdToType = verbArgSets.data.map(
    _.toList.foldMap { case (verbType, verbs) =>
      NonMergingMap(
        verbs.value.toList.map { case (verbId, _) =>
          verbId -> verbType
        }.toMap
      )
    }
  ).toCell("Verb ID to verb type mapping")

  val mlmSettings = List("masked", "symm_left", "symm_right", "symm_both")
  def mlmFeatureDir = inputDir.resolve("mlm")
  def makeMLMFeatures[A](f: (String, Path) => A): Map[String, A] = {
    mlmSettings.map(_ -> "").toMap.transform { case (setting, _) =>
      val mlmSettingDir = mlmFeatureDir.resolve(setting)
      f(setting, mlmSettingDir)
    }
  }

  @JsonCodec case class MLMFeatureId(
    sentenceId: String,
    verbLemma: String,
    index: Int)

  private lazy val mlmVocabs: Map[String, Map[String, Cell[NonMergingMap[String, Vector[String]]]]] = {
    List("arg", "verb").map(label =>
      label -> makeMLMFeatures { case (setting, path) =>
        new Cell(
          s"MLM $label vocab ($setting)",
          FileUtil.readJson[Map[String, Vector[String]]](path.resolve(s"${label}_vocabs.json"))
            .map(NonMergingMap(_))
        )
      }
    ).toMap
  }

  import breeze.linalg.DenseVector

  val mlmFeatureDim = 1024
  // type -> mode -> verb lemma -> sentence id -> index -> vector
  private lazy val mlmVectors: Map[String, Map[String, RunDataCell[Map[String, Map[String, NonMergingMap[Int, DenseVector[Float]]]]]]] = {
    List("arg", "verb").map(label =>
      label -> makeMLMFeatures { case (setting, path) =>
        // TODO sentences is temporary
        RunData.strings.zip(sentences.data).flatMap { case (split, sents) =>
          val idsPath = path.resolve(s"${split}_${label}_ids.jsonl.gz")
          val vecPath = path.resolve(s"${split}_${label}_vecs.bin")
          // val embPath = Paths.get(filePrefix + "_emb.bin")
          for {
            // TODO vocab is temporary
            vocabs <- mlmVocabs(label)(setting).get
            ids <- Log.infoBranch("Reading verb IDs")(
              FileUtil.readJsonLines[MLMFeatureId](idsPath).compile.toList
            )
            vecs <- Log.infoBranch(s"Reading $label MLM vectors")(
              VectorFileUtil.readDenseFloatVectorsNIO(vecPath, mlmFeatureDim)
            )
            _ <- Log.info(s"Number of IDs: ${ids.size}; Number of vectors: ${vecs.size}; embedding size: ${vecs.head.size}")
            _ <- {
              val numToCheck = 20
              val propSane = vecs.take(numToCheck)
                .foldMap(
                  _.activeValuesIterator.filter(f => f >= 0.0 && f <= 1.0).size
                ).toDouble / (numToCheck * mlmFeatureDim)
              val sanityCheckText = f"Sanity check: ${propSane * 100.0}%.1f%% of sampled vector components units are between 0 and 1."
              if(propSane < 1.0) {
                Log.warn(sanityCheckText) >>
                  Log.warn("There might be endianness issues with how you're reading the vectors") >>
                  vecs.take(numToCheck).traverse(e => Log.info(e.activeValuesIterator.take(10).mkString("\t")))
              } else Log.info(sanityCheckText)
            }
            norms <- Ref[IO].of(qfirst.metrics.Numbers(Vector[Float]()))
            normedVecs <- ids.zip(vecs).infoTraverse("Normalizing vectors") { case (id, vec) =>
              import breeze.math._
              import breeze.linalg._
              val total = sum(vec)
              // if(total < 0.6) {
              //   val vocab = vocabs(id.verbLemma)
              //   val sentence = sents(id.sentenceId)
              //   val token = sentence(id.index)
              //   val topElements = vec.activeIterator.map { case (i, value) =>
              //     vocab(i) -> value
              //   }.toVector.sortBy(-_._2)
              //   System.err.println(jjm.ling.Text.render(sentence))
              //   System.err.println(s"$id ($token): $total")
              //   topElements.grouped(10).take(5).foreach { pairs =>
              //     System.err.println(pairs.map(_._1).map(x => f"$x%10s").mkString(" "))
              //     System.err.println(pairs.map(_._2).map(x => f"$x%10.5f").mkString(" "))
              //   }
              // }
              // TODO make it a parameter whether to norm vectors or not. perhaps in the clustering alg instead though.
              // or at least do this in-place or something.
              norms.update(_ |+| qfirst.metrics.Numbers(total)) >> IO(vec /:/ total)
            }
            _ <- norms.get >>= (n => Log.info(s"Vector normalizers: ${getMetricsString(n)}"))
          } yield ids.zip(normedVecs).foldMap { case (mlmFeatureId, vec) =>
              Map(mlmFeatureId.verbLemma -> Map(mlmFeatureId.sentenceId -> NonMergingMap(mlmFeatureId.index -> vec)))
          }
        }.toCell(s"$label MLM Vectors")
      }
    ).toMap
  }

  def verbMLMVocab = mlmVocabs("verb")
  def argMLMVocab = mlmVocabs("arg")

  def verbMLMVectors = mlmVectors("verb")
  def argMLMVectors = mlmVectors("arg")

  def getVerbMLMFeatures(mode: String): RunData[VerbFeatsNew[DenseVector[Float]]] =
    verbMLMVectors(mode).data.map { mlmFeats =>
      (verbType: VerbType) => (verbId: VerbId) => {
        mlmFeats(getVerbLemma(verbType))(verbId.sentenceId).value(verbId.verbIndex)
      }
    }

  def getArgMLMFeatures(mode: String): RunData[ArgFeatsNew[DenseVector[Float]]] =
    argMLMVectors(mode).data.zip(argIndices).map { case (mlmFeats, getArgIndex) =>
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
        mlmFeats(getVerbLemma(verbType))(argId.verbId.sentenceId).value(getArgIndex(verbType)(argId))
      }
    }

  // XXXXXXXXX

  // just for clauses
  // def makeVerbSpecificClauseVocab(instances: Map[String, Map[Int, QAPairs]]): Vocab[ArgStructure] = {
  //   Vocab.make(
  //     instances.values.toList.foldMap(verbMap =>
  //       verbMap.values.toList.foldMap(qMap =>
  //         qMap.keys.toList.map { case (frame, slot) =>
  //           ArgStructure(frame.args, frame.isPassive).forgetAnimacy
  //         }.toSet
  //       )
  //     )
  //   )
  // }

  // def readFramesets(vsConfig: VerbSenseConfig)(implicit cs: ContextShift[IO]) = {
  //   framesetsPath(vsConfig).flatMap(path =>
  //     FileUtil.readJsonLines[VerbFrameset](path)
  //       .compile.toList
  //       .map(_.map(f => f.inflectedForms -> f).toMap)
  //   )
  // }
  // def writeFramesets(
  //   vsConfig: VerbSenseConfig,
  //   framesets: Map[InflectedForms, VerbFrameset])(
  //   implicit cs: ContextShift[IO]
  // ): IO[Unit] = framesetsPath(vsConfig).flatMap(path =>
  //   FileUtil.writeJsonLines(path)(
  //     framesets.values.toList
  //   )
  // )
}

// TODO change to raw question string (or slots) for arg id
// and add clausal question as an extra feature for interpretability and/or clustering stuff
class GoldQasrlFeatures(
  mode: RunMode)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[InflectedForms, ClausalQuestion](mode)(implicitly[Encoder[InflectedForms]], implicitly[Decoder[InflectedForms]], cs, Log) {

  override def getVerbLemma(verbType: InflectedForms): String = verbType.stem

  def renderVerbType(verbType: InflectedForms): String = verbType.allForms.mkString(", ")

  override val rootDir = Paths.get("frame-induction/qasrl")

  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

  private[this] def readQasrlDataset(name: String) =
    Log.infoBranch(s"Reading QA-SRL dataset $name")(
      readDataset(qasrlBankPath.resolve(name + ".jsonl.gz"))
    )

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  val qasrlBank = new Cell(
    "QA-SRL Bank",
    Log.infoBranch("Reading QA-SRL Bank")(
      IO(Data.readFromQasrlBank(qasrlBankPath).toEither.right.get)
    )
  )

  val dataset: RunDataCell[Dataset] = RunData(
    train = "expanded/train",
    dev = "expanded/dev",
    test = "orig/test").flatMap(
    spec => readQasrlDataset(spec).map(filterDatasetNonDense)
  ).toCell("QA-SRL dataset")

  val qaPairs: RunDataCell[VerbFeats[QAPairs]] = {
    dataset.data.map(
      _.sentences.iterator.flatMap { case (sid, sentence) =>
        sentence.verbEntries.values.map(sid -> _)
      }.toList.groupBy(_._2.verbInflectedForms).map {
        case (verbInflectedForms, pairs) =>
          verbInflectedForms -> pairs.groupBy(_._1).toList.foldMap { case (sid, pairs) =>
            NonMergingMap(
              pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
                val qLabels = verb.questionLabels.values.toList
                val resolvedQs = ClauseResolution.getResolvedFramePairs(
                  verbInflectedForms, qLabels.map(_.questionSlots)
                ).map(Function.tupled(ClausalQuestion(_, _)))
                VerbId(sid, verbIndex) -> qLabels.zip(resolvedQs).map { case (qLabel, clausalQ) =>
                  clausalQ -> (
                    qLabel.answerJudgments.toList.flatMap(_.judgment.getAnswer).map(_.spans.toList)
                  )
                }.toMap
              }
            )
          }
      }
    )
  }.toCell("QA-SRL All QA Pairs")

  val argIdToSpans: RunDataCell[ArgFeats[List[List[ESpan]]]] = qaPairs.data.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, qaPairs) =>
        NonMergingMap(
          qaPairs.map { case (cq, spanLists) =>
            ArgumentId(verbId, cq) -> spanLists
          }
        )
      }
    }
  ).toCell("QA-SRL ArgumentId to spans")

  override val verbArgSets: RunDataCell[VerbFeats[Set[ClausalQuestion]]] =
      qaPairs.data.map(
        _.transform { case (_, verbs) =>
          NonMergingMap(
            verbs.value.transform { case (verbId, qaPairs) =>
              qaPairs.keySet
            }
          )
        }
      ).toCell("QA-SRL ArgumentIds by verb ID")
  // TODO replace with this
  // qaPairs
  //   .map(_.sentences.map { case (sid, sent) => sid -> sent.sentenceTokens })
  //   .map(NonMergingMap.apply[String, Vector[String]])
  //   .toCell("QA-SRL sentences")

  override val sentences = dataset.data
    .map(_.sentences.map { case (sid, sent) => sid -> sent.sentenceTokens })
    .map(NonMergingMap.apply[String, Vector[String]])
    .toCell("QA-SRL sentences")

  // TODO temp before we have distribution results from the models
  // override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
  //   argIdToSpans.data.map(
  //     _.transform { case (_, args) =>
  //       NonMergingMap(
  //         args.value.transform { case (argId, _) =>
  //           Map(QuestionTemplate.fromClausalQuestion(argId.argument) -> 1.0)
  //         }
  //       )
  //     }
  //   ).toCell("QA-SRL question distributions for questions")
  // }

  // TODO incorporate gold question
  override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
    RunData.strings.zip(verbIdToType.data).zip(qaPairs.data)
      .flatMap { case ((split, vidToType), qaPairs) =>
      val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
      FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
        .map { case QGen.SentencePrediction(sid, sentenceTokens, verbs) =>
          verbs.foldMap { case QGen.VerbPrediction(vi, spanPreds) =>
            val verbId = VerbId(sid, vi)
            // verbId may not be present if all QAs were filtered out (ie it was a bad predicate)
            vidToType.value.get(verbId).foldMap { verbType =>
              val qas = qaPairs(verbType).value(verbId)
              Map(
                verbType -> NonMergingMap(
                  qas.map { case (cq, spanLists) =>
                    // We might miss a few spans, which were judged very low probability by the model
                    // so they weren't decoded.
                    val spanPredLists = spanLists
                      .map(_.flatMap(s => spanPreds.find(_.span == s)))
                      .filter(_.nonEmpty)
                    if(spanPredLists.isEmpty) {
                      // back off to gold question (should be rare) if ALL spans in ALL answers are missed
                      val argId = ArgumentId(verbId, cq)
                      val qDist = Map(QuestionTemplate.fromClausalQuestion(cq) -> 1.0)
                      System.err.println(s"oopsie: $argId")
                      argId -> qDist
                    } else ArgumentId(verbId, cq) -> spanPredLists.foldMap { localPreds =>
                      val denom = localPreds.foldMap(_.spanProb) * spanPredLists.size
                      localPreds.foldMap(
                        _.questions.transform { case (_, prob) =>
                          prob / denom
                        }
                      )
                    }
                  }
                )
              )
            }
          }
        }.infoCompile(s"Processing QG Predictions ($split)")(_.foldMonoid)
    }.toCell("Question distributions for arguments")
  }

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] = qaPairs.data.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, qaPairs) =>
        NonMergingMap(
          qaPairs.map { case (cq, spanLists) =>
            ArgumentId(verbId, cq) -> spanLists.foldMap(spans => spans.map(_ -> (1.0 / spans.size)).toMap)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping")

  override val argIndices: RunData[ArgFeatsNew[Int]] = RunData.strings.map(_ => ???)


  def argSyntacticFunctions: RunDataCell[ArgFeats[String]] = ???
}

case class PropBankRoleLabel(
  framesetId: String,
  role: String
)
object PropBankRoleLabel {


  // the pred itself, discourse markers, negations, and auxiliaries we don't care about
  def roleLabelIsIrrelevant(l: String) = {
    l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD") ||
      l.contains("C-") || l.contains("R-") ||
      l == "rel"// || l == "Support"
  }

  import qfirst.ontonotes.Predicate

  def isArgRelevant(pred: Predicate, roleLabel: String, argSpan: ESpan) =
    !roleLabelIsIrrelevant(roleLabel) &&
      !Auxiliaries.auxiliaryVerbs.contains(pred.lemma.lowerCase) &&
      !argSpan.contains(pred.index)
}

sealed abstract class PropBankFeatures[Arg](
  mode: RunMode,
  val assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[String, Arg](mode)(implicitly[Encoder[String]], implicitly[Decoder[String]], cs, Log) {

  override def getIfPropBank: Option[PropBankFeatures[Arg]] = Some(this)

  override def getVerbLemma(verbType: String): String = {
    if(assumeGoldVerbSense) verbType.takeWhile(_ != '.')
    else verbType
  }

  def renderVerbType(verbType: String): String = verbType

  // don't store the models in the same dir, because they cluster different kinds of things
  override def modelDir = super.modelDir.map(
    _.resolve(if(assumeGoldVerbSense) "by-sense" else "by-lemma")
  ).flatTap(createDir)

  def verbSenseLabels: RunData[VerbFeatsNew[String]]

  def argRoleLabels: RunDataCell[ArgFeats[PropBankRoleLabel]]
}

class CoNLL08GoldDepFeatures(
  mode: RunMode,
  assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[Int](mode, assumeGoldVerbSense)(cs, Log) {

  import qfirst.conll08._

  override val rootDir = Paths.get("frame-induction/conll08")

  val dataService = new CoNLL08FileSystemService(Paths.get("data/conll08st"))

  val splits = RunData[CoNLL08Split](
    train = CoNLL08Split.Train,
    dev = CoNLL08Split.Dev,
    test = CoNLL08Split.TestWSJ
  )

  def keepOnlyVerbalPredicates(sentence: CoNLL08Sentence) = {
    import jjm.ling.en.PTBPosTags
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .filter(pas => PTBPosTags.verbs.contains(sentence.tokens(pas.predicate.index).pos))
    )
  }

  val dataset: RunDataCell[NonMergingMap[String, CoNLL08Sentence]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    splits.flatMap(split =>
      dataService
        .streamSentences[IO](split)
        .map(s => NonMergingMap(Map(s.id.toString -> keepOnlyVerbalPredicates(s))))
        .infoCompile(s"Reading CoNLL 2008 data ($split)")(_.foldMonoid)
    ).toCell("CoNLL 2008 dataset")
  }

  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    dataset.data.map(sents =>
      NonMergingMap(
        sents.value.map { case (sid, sent) =>
          sid -> sent.tokens.map(_.token)
        }
      )
    ).toCell("CoNLL 2008 sentence index")

  val predArgStructures: RunDataCell[VerbFeats[PredicateArgumentStructure]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbId = VerbId(sid, pas.predicate.index)
          Map(verbType -> NonMergingMap(Map(verbId -> pas))) 
        }
      }
    ).toCell("CoNLL 2008 predicate-argument structures")
  }

  override val verbSenseLabels = predArgStructures.data.map { paStructs =>
    (verbType: String) => (verbId: VerbId) => {
      val predicate = paStructs(verbType).value(verbId).predicate
      s"${predicate.lemma}.${predicate.sense}"
    }
  }

  import scala.annotation.tailrec
  @tailrec final def getDependencyPathToRoot(
    dependencies: Vector[(String, Int)],
    index: Int,
    acc: List[(String, Int)] = Nil
  ): List[(String, Int)] = {
    if(index == -1) acc
    else {
      val dep = dependencies(index)
      getDependencyPathToRoot(dependencies, dep._2, dep :: acc)
    }
  }

  val argSyntacticFunctions: RunDataCell[ArgFeats[String]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        val dependencies = sentence.childToParentDependencies
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma

          val verbIndex = pas.predicate.index
          val verbId = VerbId(sid, verbIndex)

          val argSyntacticFs = pas.arguments.map(_._2).map { argIndex =>
            ArgumentId(verbId, argIndex) -> dependencies(argIndex)._1
          }.toMap

          Map(verbType -> NonMergingMap(argSyntacticFs))
        }
      }
    ).toCell("CoNLL 2008 argument-to-head syntactic rels")
  }

  val argDependencyPaths: RunDataCell[ArgFeats[String]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        val dependencies = sentence.childToParentDependencies
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbIndex = pas.predicate.index
          val verbId = VerbId(sid, verbIndex)
          val argDependencyPaths = pas.arguments.map(_._2).map { argIndex =>
            val predPathToRoot = getDependencyPathToRoot(dependencies, verbIndex)
            val argPathToRoot = getDependencyPathToRoot(dependencies, argIndex)
            val predPathToLCA = predPathToRoot.takeWhile { case (_, i) =>
              i != argIndex && !argPathToRoot.exists(_._2 == i)
            }
            val argPathToLCA = argPathToRoot.takeWhile { case (_, i) =>
              i != argIndex && !predPathToRoot.exists(_._2 == i)
            }
            val pathStr = predPathToLCA.mkString("->") + "*" + argPathToLCA.reverse.mkString("<-")
            ArgumentId(verbId, argIndex) -> pathStr
          }.toMap

          Map(verbType -> NonMergingMap(argDependencyPaths))
        }
      }
    ).toCell("CoNLL 2008 predicate-to-argument dependency paths")
  }

  override val verbArgSets: RunDataCell[VerbFeats[Set[Int]]] = predArgStructures.data.map(
    mapVerbFeats(_.arguments.map(_._2).toSet)
  ).toCell("CoNLL 2008 gold arg indices")

  // override val verbArgSets: RunDataCell[VerbFeats[Set[Int]]] = predArgStructures.data.map(
  //   mapVerbFeats(_.arguments.map(_._2).toSet)
  // ).toCell("CoNLL 2008 gold arg indices")

  override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
    RunData.strings.map(_ => ???).toCell(
      "[ERROR] no question distributions for CoNLL 2008 data."
    )
  }

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] =
    RunData.strings.map(_ => ???).toCell(
      "[ERROR] no spans for CoNLL 2008 data."
    )

  override val argIndices: RunData[ArgFeatsNew[Int]] = {
    RunData.strings.map(_ =>
      (verbType: String) => (argId: ArgumentId[Int]) => argId.argument
    )
  }

  override val argRoleLabels: RunDataCell[ArgFeats[PropBankRoleLabel]] =
    predArgStructures.data.map(
      makeArgFeats { case (verbId, pas) =>
        pas.arguments.map { case (roleLabel, index) =>
          index -> PropBankRoleLabel(pas.predicate.sense, roleLabel)
        }
      }
    ).toCell("PropBank arg to role label mapping")

  // TODO refactor this, other features, gen code, and setup function into the main trait,
  // so it's all done smoothly for the cases with none of those features.

  @JsonCodec case class MLMFeatureGenInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbs: Map[String, Set[Int]],
    argsByVerb: Map[String, Set[Int]]
  )

  val mlmFeatureGenInputs = dataset.data.map { sentences =>
    Stream.emits[IO, (String, CoNLL08Sentence)](sentences.value.toList).map { case (sid, sentence) =>
      MLMFeatureGenInput(
        sentenceId = sid,
        sentenceTokens = sentence.tokens.map(_.token),
        verbs = sentence.predicateArgumentStructures.foldMap(pas =>
          Map(pas.predicate.lemma -> Set(pas.predicate.index))
        ),
        argsByVerb = sentence.predicateArgumentStructures.foldMap(pas =>
          Map(pas.predicate.lemma -> pas.arguments.map(_._2).toSet)
        )
      )
    }
  }

  def getMLMFeatureInputOutPath(split: String) = outDir
    .map(_.resolve(s"mlm-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  val writeMLMInputs = RunData.strings.zip(mlmFeatureGenInputs).flatMap { case (split, inputStream) =>
    getMLMFeatureInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"MLM feature inputs already found at $outPath."),
        Log.infoBranch(s"Logging MLM feature inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  override val setup = writeMLMInputs

}


// verb type is either lemma or sense, depending on assumeGoldVerbSense value (false/true resp.).
class Ontonotes5GoldSpanFeatures(
  mode: RunMode,
  assumeGoldVerbSense: Boolean
  // val filterPropBankRoles: Boolean // TODO do this in a reasonable way
)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[ESpan](mode, assumeGoldVerbSense)(cs, Log) {

  import qfirst.ontonotes._

  override val rootDir = Paths.get("frame-induction/ontonotes5")

  val ontonotesPath = Paths.get("data/conll-formatted-ontonotes-5.0")

  val ontonotesService = new CoNLLFileSystemService(ontonotesPath)

  val fullIndex = Cell("OntoNotes Index")(
    Log.infoBranch("Reading OntoNotes file paths")(
      ontonotesService.getAllPaths
    )
  )

  val index: RunDataCell[List[CoNLLPath]] = RunData(
    train = "train",
    dev = "development",
    test = "test").flatMap(
    spec => fullIndex.get.map(_.filter(_.split == spec))
  ).toCell("OntoNotes Index")

  val dataset: RunDataCell[VerbFeats[(String, Map[ESpan, String])]] = RunData.strings.zip(index.data)
    .flatMap { case (split, filePaths) =>
      filePaths.infoBarFoldMapM(s"Reading PropBank files to construct instances ($split)") { path =>
        Log.trace(path.suffix) >> ontonotesService.getFile(path).map { file =>
          file.sentences.foldMap { sentence =>
            val sentenceId = sentence.path.toString
            sentence.predicateArgumentStructures.foldMap { pas =>
              val verbLemma = pas.predicate.lemma
              val verbSense = s"$verbLemma.${pas.predicate.sense}"
              val verbType = if(assumeGoldVerbSense) verbSense else verbLemma
              Map(
                verbType -> NonMergingMap(
                  VerbId(sentenceId, pas.predicate.index) ->
                    (verbSense -> pas.arguments
                       .filter { case (label, span) => PropBankRoleLabel.isArgRelevant(pas.predicate, label, span) }
                       .map(_.swap).toMap)
                )
              )
            }
          }
        }
      }
    }.toCell("PropBank dataset")

  // TODO: fill this in after adding sense information to `dataset`
  override val verbSenseLabels = dataset.data.map { data =>
    ???
  }

  // TODO eliminate redundant traversal of propbank
  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    index.data.flatMap(
      _.infoBarFoldMapM("Constructing sentence index") { path =>
        ontonotesService.getFile(path).map(
          _.sentences.foldMap(s => NonMergingMap(s.path.toString -> s.tokens.map(_.token).toVector))
        )
      }
    ).toCell("Sentence index")

  override val verbArgSets = dataset.data.map(
    _.transform { case (_, verbs) =>
      NonMergingMap(
        verbs.value.map { case (verbId, (_, labels)) =>
          verbId -> labels.keySet
        }
      )
    }
  ).toCell("PropBank gold span instances")

  override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
    RunData.strings.zip(verbIdToType.data).flatMap { case (split, vidToType) =>
      val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
      FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
        .map { case QGen.SentencePrediction(sid, _, verbs) =>
          verbs.foldMap { case QGen.VerbPrediction(vi, spans) =>
            val verbId = VerbId(sid, vi)
            val verbType = vidToType.value(verbId)
            Map(
              verbType -> NonMergingMap(
                spans.map { case QGen.SpanPrediction(span, _, questions) =>
                  ArgumentId(verbId, span) -> questions
                }.toMap
              )
            )
          }
        }
        .infoCompile(s"Reading QG Predictions ($split)")(_.foldMonoid)
    }.toCell("Question distributions for arguments")
  }

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] = dataset.data.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, (framesetId, arguments)) =>
        NonMergingMap(
          arguments.map { case (span, _) =>
            ArgumentId(verbId, span) -> Map(span -> 1.0)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping")

  override val argIndices: RunData[ArgFeatsNew[Int]] = RunData.strings.map(_ => ???)

  def argSyntacticFunctions: RunDataCell[ArgFeats[String]] = ???

  override val argRoleLabels: RunDataCell[ArgFeats[PropBankRoleLabel]] = dataset.data.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, (framesetId, arguments)) =>
        NonMergingMap(
          arguments.map { case (span, label) =>
            ArgumentId(verbId, span) -> PropBankRoleLabel(framesetId, label)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping")


  @JsonCodec case class PropBankQGInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbEntries: Map[Int, PropBankQGVerbInput]
  )
  @JsonCodec case class PropBankQGVerbInput(
    verbIndex: Int,
    argumentSpans: Set[ESpan]
  )

  val qgInputs = index.data.map { paths =>
    Stream.emits[IO, CoNLLPath](paths) >>= { path =>
      Stream.eval(ontonotesService.getFile(path)) >>= { file =>
        Stream.emits[IO, PropBankQGInput](
          file.sentences.map { sentence =>
            PropBankQGInput(
              sentence.path.toString,
              sentence.tokens.map(_.token).toVector,
              sentence.predicateArgumentStructures.map { pas =>
                pas.predicate.index -> PropBankQGVerbInput(pas.predicate.index, pas.arguments.map(_._2).toSet)
              }.toMap
            )
          }
        )
      }
    }
  }

  def getQGInputOutPath(split: String) = outDir
    .map(_.resolve(s"qg-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  val writeQGInputs = RunData.strings.zip(qgInputs).flatMap { case (split, inputStream) =>
    getQGInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"QG Inputs already found at $outPath."),
        Log.infoBranch(s"Logging QG inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  override val setup = writeQGInputs
}
