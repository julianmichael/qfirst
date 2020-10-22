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

import cats.Applicative
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

  implicit protected val runMode = mode

  type CachedVerbFeats[A] = RunDataCell[Map[VerbType, NonMergingMap[VerbId, A]]]
  type VerbFeats[A] = RunData[VerbType => VerbId => A]

  type CachedArgFeats[A] = RunDataCell[Map[VerbType, NonMergingMap[ArgumentId[Arg], A]]]
  type ArgFeats[A] = RunData[VerbType => ArgumentId[Arg] => A]

  // def makeArgFeats[A, B](f: (VerbId, A) => List[(Arg, B)]): VerbFeats[A] => ArgFeats[B] = {
  //   _.transform { case (_, verbs) =>
  //     verbs.value.toList.foldMap { case (verbId, feat) =>
  //       f(verbId, feat).foldMap { case (arg, argFeat) =>
  //         NonMergingMap(ArgumentId(verbId, arg) -> argFeat)
  //       }
  //     }
  //   }
  // }

  def cacheArgFeats[A](name: String, log: Boolean = false)(feats: ArgFeats[A]): CachedArgFeats[A] =
    if(log) {
      (args.data.zip(feats)).flatMap { case (args, feats) =>
        Log.infoBranch(s"Caching features: $name") {
          args.toList.infoBarFoldMapM("Verb types") { case (verbType, args) =>
            val featsForVerbType = feats(verbType)
            val argsForVerbType = args.toList.infoBarFoldMapM(s"$verbType") { argId =>
              IO(NonMergingMap(argId -> featsForVerbType(argId)))
            }
            argsForVerbType.map(args => Map(verbType -> args))
          }
        }
      }.toCell(name)
    } else {
      (args.data.zip(feats)).map { case (args, feats) =>
        args.transform { case (verbType, args) =>
          val featsForVerbType = feats(verbType)
          NonMergingMap(
            args.iterator.map { argId =>
              argId -> featsForVerbType(argId)
            }.toMap
          )
        }
      }.toCell(name)
    }

  def fileCacheArgFeats[A: Encoder : Decoder](
    name: String, log: Boolean = false)(
    feats: ArgFeats[A])(
    implicit argDecoder: Decoder[Arg], argEncoder: Encoder[Arg]
  ): CachedArgFeats[A] = {
    // for some reason, the compiler needs this up here to be happy
    val _ = implicitly[Encoder[List[(ArgumentId[Arg], A)]]]
    if(log) {
      (args.data.zip(feats)).flatMap { case (args, feats) =>
        Log.infoBranch(s"Computing features: $name") {
          args.toList.infoBarFoldMapM("Verb types") { case (verbType, args) =>
            val featsForVerbType = feats(verbType)
            val argsForVerbType = args.toList.infoBarFoldMapM(s"$verbType") { argId =>
              IO(NonMergingMap(argId -> featsForVerbType(argId)))
            }
            argsForVerbType.map(args => Map(verbType -> args))
          }
        }
      }.toFileCachedCell(name, n => cacheDir.map(_.resolve(s"$name/$n.jsonl.gz")))(
        read = path => FileUtil.readJsonLines[(VerbType,List[(ArgumentId[Arg],A)])](path).compile.toList
          .map(_.map { case (vt, args) => vt -> NonMergingMap(args.toMap) }.toMap),
        write = (path, a) => FileUtil.writeJsonLines(path)(a.iterator.map { case (vt, args) => vt -> args.value.toList }.toList)
      )
    } else {
      (args.data.zip(feats)).map { case (args, feats) =>
        args.transform { case (verbType, args) =>
          val featsForVerbType = feats(verbType)
          NonMergingMap(
            args.iterator.map { argId =>
              argId -> featsForVerbType(argId)
            }.toMap
          )
        }
      }.toFileCachedCell(name, n => cacheDir.map(_.resolve(s"$name/$n.jsonl.gz")))(
        read = path => FileUtil.readJsonLines[(VerbType,List[(ArgumentId[Arg],A)])](path).compile.toList
          .map(_.map { case (vt, args) => vt -> NonMergingMap(args.toMap) }.toMap),
        write = (path, a) => FileUtil.writeJsonLines(path)(a.iterator.map { case (vt, args) => vt -> args.value.toList }.toList)
      )
    }
  }

  def mapArgFeats[A, B](feats: ArgFeats[A])(f: A => B): ArgFeats[B] =
    feats.map(_.andThen(_.andThen(f)))

  def mapVerbFeats[A, B](feats: VerbFeats[A])(f: A => B): VerbFeats[B] =
    feats.map(_.andThen(_.andThen(f)))

  def widen[A](feats: ArgFeats[A]): RunData[VerbType => ArgumentId[Arg] => A] = feats

  // overriden for propbank features
  def getIfPropBank: Option[PropBankFeatures[Arg]] = None

  def splitName = RunData.strings.get

  // indices of important info

  val sentences: RunDataCell[NonMergingMap[String, Vector[String]]]

  val verbArgSets: RunDataCell[Map[VerbType, Map[VerbId, Set[Arg]]]]

  lazy val sentenceInfos: RunDataCell[Map[String, SentenceInfo[VerbType, Arg]]] = {
    (sentences.data, verbArgSets.data).mapN { (sents, argSets) =>
      val sentMaps = argSets.toList.foldMap { case (verbType, allVerbs) =>
        allVerbs.toList.foldMap { case (verbId, args) =>
          Map(verbId.sentenceId -> NonMergingMap(verbId.verbIndex -> VerbInfo(verbId.verbIndex, verbType, args)))
        }
      }
      sentMaps.map { case (sid, sentMap) =>
        sid -> SentenceInfo(sid, sents.value(sid), sentMap.value)
      }
    }.toCell("Sentence infos")
  }

  lazy val sentencesByVerbType: RunDataCell[Map[VerbType, Set[String]]] =
    verbArgSets.data.map(_.mapVals(_.keySet.map(_.sentenceId))).toCell("Sentence IDs by verb type")

  lazy val args: RunDataCell[Map[VerbType, Set[ArgumentId[Arg]]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) =>
      verbs.toList.foldMap { case (verbId, args) =>
        args.map(arg => ArgumentId(verbId, arg))
      }
    }
  ).toCell("Argument IDs")

  lazy val verbs: RunDataCell[Map[VerbType, Set[VerbId]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) => verbs.keySet }
  ).toCell("Verb IDs")

  lazy val verbIdToType = verbArgSets.data.map(
    _.toList.foldMap { case (verbType, verbs) =>
      NonMergingMap(
        verbs.toList.map { case (verbId, _) =>
          verbId -> verbType
        }.toMap
      )
    }
  ).toCell("Verb ID to verb type mapping")

  // Metadata

  // for constructing features
  def getVerbLemma(verbType: VerbType): String

  // for logging. could replace with Show instance?
  def renderVerbType(verbType: VerbType): String

  // Various features that need to be implemented

  def argSpans: CachedArgFeats[Map[ESpan, Double]]

  lazy val globalQuestionPrior = argQuestionDists.data.map { qFeats =>
    val pcounts = qFeats.values.toList.foldMap(
      _.value.values.toList.combineAll
    )
    val total = pcounts.values.sum
    pcounts.mapVals(_ / total)
  }.toCell("Global question prior")

  lazy val argQuestionDists: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    RunData.strings.zip(verbIdToType.data).zip(argSpans.data).zip(verbArgSets.data).zip(argSemanticHeadIndices).flatMap {
      case ((((split, vidToType), allSpans), allVerbs), headIndices) =>
        val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
        FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
          .map { case QGen.SentencePrediction(sid, _, verbs) =>
            verbs.foldMap { case QGen.VerbPrediction(vi, spans) =>
              val verbId = VerbId(sid, vi)
              val verbType = vidToType.value(verbId)
              val argSet = allVerbs(verbType)(verbId)
              val argIds = argSet.map(ArgumentId(verbId, _))
              val allHeadIndices = argIds.map(headIndices(verbType))
              Map(
                verbType -> NonMergingMap(
                  argIds.map { argId =>
                    val headIndex = headIndices(verbType)(argId)
                    val priorArgSpans = allSpans(verbType)(argId)

                    // use head-based span finding to include everything predicted by the model above the threshold
                    // val otherHeads = allHeadIndices - headIndex
                    // val questions = spans
                    //   .filter(pred =>
                    //     priorArgSpans.contains(pred.span) ||
                    //       (pred.span.contains(headIndex) &&
                    //          otherHeads.forall(h => !pred.span.contains(h))))
                    //   .foldMap(pred =>
                    //     pred.questions.mapVals(_ * pred.spanProb)
                    //   )

                    // TODO: experiment and find best setting of spans * weighting, etc.

                    // use only prior arg spans.
                    val questions = priorArgSpans.toList.foldMap { case (span, prob) =>
                      spans.find(_.span == span).foldMap { pred =>
                        pred.questions.mapVals(_ * prob)
                      }
                    }
                    argId -> questions
                  }.toMap
                )
              )
            }
          }
          .infoCompile(s"Reading QG Predictions ($split)")(_.foldMonoid)
    }.toCell("Question distributions for arguments")
  }


  // new style arg features
  def argSemanticHeadIndices: ArgFeats[Int]

  def argSyntacticFunctions: CachedArgFeats[String]

  def argSyntacticFunctionsConverted: CachedArgFeats[String]

  // directories

  protected val rootDir: Path

  def outDir = IO.pure(rootDir.resolve("out")).flatTap(createDir)

  // for use by frame induction etc.
  def modelDir = splitName.map(split => rootDir.resolve(s"models/$split")).flatTap(createDir)

  // for inputs to feature computation
  protected def inputDir = rootDir.resolve("input")

  // for caching features that take a while to compute
  protected def cacheDir = IO.pure(rootDir.resolve("cache")).flatTap(createDir)

  // features and setup code constructed from the other provided features

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
            // for now don't use the normalized vectors.
            finalVecs = vecs // normedVecs
            _ <- Log.info("Not using normalized vectors.") // remove "not" if using.
          } yield ids.zip(finalVecs).foldMap { case (mlmFeatureId, vec) =>
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

  def getVerbMLMFeatures(mode: String): VerbFeats[DenseVector[Float]] =
    verbMLMVectors(mode).data.map { mlmFeats =>
      (verbType: VerbType) => (verbId: VerbId) => {
        mlmFeats(getVerbLemma(verbType))(verbId.sentenceId).value(verbId.verbIndex)
      }
    }

  def getArgMLMFeatures(featureMode: String): ArgFeats[DenseVector[Float]] = {
    argMLMVectors(featureMode).data.zip(argSemanticHeadIndices).map { case (mlmFeats, getArgIndex) =>
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
        mlmFeats(getVerbLemma(verbType))(argId.verbId.sentenceId).value(getArgIndex(verbType)(argId))
      }
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

  // generating input files for feature generation

  @JsonCodec case class MLMFeatureGenInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbs: Map[String, Set[Int]],
    argsByVerb: Map[String, Set[Int]]
  )

  lazy val mlmFeatureGenInputs = {
    (verbs.data, args.data, sentences.data, verbIdToType.data, widen(argSemanticHeadIndices)).mapN {
      (verbs, args, sentences, verbIdToType, argSemanticHeadIndices) =>
        val verbsBySentenceId = verbs.values.reduce(_ union _).groupBy(_.sentenceId)
        val argsBySentenceId = args.values.reduce(_ union _).groupBy(_.verbId.sentenceId)
        val sentenceIds = verbsBySentenceId.keySet union argsBySentenceId.keySet
        Stream.emits[IO, String](sentenceIds.toList).map { sid =>
          MLMFeatureGenInput(
            sentenceId = sid,
            sentenceTokens = sentences(sid),
            verbs = verbsBySentenceId(sid).unorderedFoldMap { verbId =>
              Map(getVerbLemma(verbIdToType(verbId)) -> Set(verbId.verbIndex))
            },
            argsByVerb = argsBySentenceId.get(sid).combineAll.unorderedFoldMap { argId =>
              val verbType = verbIdToType(argId.verbId)
              Map(getVerbLemma(verbType) -> Set(argSemanticHeadIndices(verbType)(argId)))
            }
          )
        }
    }
  }

  def getMLMFeatureInputOutPath(split: String) = outDir
    .map(_.resolve(s"mlm-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  lazy val writeMLMInputs = RunData.strings.zip(mlmFeatureGenInputs).flatMap { case (split, inputStream) =>
    getMLMFeatureInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"MLM feature inputs already found at $outPath."),
        Log.infoBranch(s"Logging MLM feature inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  @JsonCodec case class PropBankQGInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbEntries: Map[Int, PropBankQGVerbInput]
  )
  @JsonCodec case class PropBankQGVerbInput(
    verbIndex: Int,
    argumentSpans: Set[ESpan]
  )

  def qgInputs: RunData[Stream[IO, PropBankQGInput]] = sentences.data.zip(verbArgSets.data).zip(argSpans.data).map { case ((sents, allVerbs), allSpans) =>
    Stream.emits[IO, (String, List[(VerbType, (VerbId, Set[Arg]))])](
      allVerbs.toList.flatMap(p => p._2.toList.map(p._1 -> _)).groupBy(_._2._1.sentenceId).toList
    ).map { case (sid, verbs) =>
        val verbInputs =  verbs.groupBy(_._2._1).map { case (verbId, verbInstances) =>
          val spans = verbInstances.foldMap(t =>
            t._2._2.unorderedFoldMap(arg =>
              allSpans(t._1)(ArgumentId(verbId, arg)).keySet
            )
          )
          verbId.verbIndex -> PropBankQGVerbInput(verbId.verbIndex, spans)
        }
        PropBankQGInput(sid, sents.value(sid), verbInputs)
    }
  }

  def getQGInputOutPath(split: String) = outDir
    .map(_.resolve(s"qg-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  def writeQGInputs = RunData.strings.zip(qgInputs).flatMap { case (split, inputStream) =>
    getQGInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"QG Inputs already found at $outPath."),
        Log.infoBranch(s"Logging QG inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  def setup = writeMLMInputs >> writeQGInputs

  // TODO: probably get rid of this
  lazy val liveDir = IO.pure(rootDir.resolve("live")).flatTap(createDir)

  // TODO refactor into RunData framework. is this done?
  // actually TODO: delete!
  lazy val evaluationItemsPath = (liveDir, splitName).mapN((dir, split) => dir.resolve(s"eval-sample-$split.jsonl"))
  lazy val evaluationItems = {
      new Cell(
        "Evaluation items",
        evaluationItemsPath >>= (evalItemsPath =>
          FileCached.get[Vector[(VerbType, String, Int)]](
            "Evaluation Items")(
            path = evalItemsPath,
            read = path => FileUtil.readJsonLines[(VerbType, String, Int)](path).compile.toVector,
            write = (path, items) => FileUtil.writeJsonLines(path)(items))(
            Log.infoBranch(s"Creating new sample for evaluation at $evalItemsPath")(
              verbs.get.map { verbMap =>
                (new scala.util.Random(86735932569L)).shuffle(
                  verbMap.iterator
                    .flatMap { case (vt, vids) => vids.map(vid => (vt, vid.sentenceId, vid.verbIndex)) }
                    .toVector
                ).take(1000).toVector
              }
            )
          )
        )
    )
  }

  lazy val paraphraseGoldPath = liveDir.map(_.resolve("gold-paraphrases.json"))

  def readGoldParaphrases = {
    paraphraseGoldPath >>= (ppGoldPath =>
      IO(Files.exists(ppGoldPath)).ifM(
        FileUtil.readJson[ParaphraseAnnotations](ppGoldPath),
        Log.warn(s"No gold paraphrase annotations found at $ppGoldPath. Initializing to empty annotations.") >>
          IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]]),
        )
    )
  }
  def saveGoldParaphrases(data: ParaphraseAnnotations) = {
    paraphraseGoldPath >>= (ppGoldPath =>
      Log.infoBranch(s"Saving gold paraphrases to $ppGoldPath")(
        FileUtil.writeJson(ppGoldPath, io.circe.Printer.noSpaces)(data)
      )
    )
  }
}
