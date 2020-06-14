package qfirst.frame.features

import qfirst.frame._
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

abstract class Features[VerbType : Encoder : Decoder, Arg](
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

  def argSyntacticFunctionsConverted: RunDataCell[ArgFeats[String]]

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
