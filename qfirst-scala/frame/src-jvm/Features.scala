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

abstract class Features[VerbType : Encoder : Decoder, Arg](
  mode: RunMode)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]) {

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

  def getVerbLemma(verbType: VerbType): String

  val sentences: RunDataCell[NonMergingMap[String, Vector[String]]]

  val verbArgSets: RunDataCell[VerbFeats[Set[Arg]]]

  def argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]]

  def argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]]

  // new style arg features
  def argIndices: RunData[ArgFeatsNew[Int]]

  protected val rootDir: Path

  // TODO move all setup into this/superclass
  def setup: IO[Unit] = IO.unit

  val splitLabels: RunDataCell.Labels = new RunDataCell.Labels(mode)
  object splitDirnames {
    def train = splitLabels.train
    def dev = splitLabels.dev
    def test = splitLabels.test
    def input = splitLabels.input
    def full = splitLabels.full
    def eval = {
      if(splitLabels.eval == splitLabels.full) splitLabels.eval
      else s"${splitLabels.full}-filtered-${splitLabels.eval}"
    }
    def all = splitLabels.all
  }

  val createDir = (path: Path) => IO(!Files.exists(path))
    .ifM(IO(Files.createDirectories(path)), IO.unit)

  def outDir = IO.pure(rootDir.resolve("out")).flatTap(createDir)

  // for use by frame induction etc.
  def modelDir = outDir.map(_.resolve("models")).flatTap(createDir)

  // for inputs to feature computation
  protected def inputDir = rootDir.resolve("input")

  // for caching features that take a while to compute
  protected def cacheDir = IO.pure(rootDir.resolve("cache")).flatTap(createDir)

  // question and verb IDs

  lazy val args: RunDataCell[Map[VerbType, Set[ArgumentId[Arg]]]] = verbArgSets.get.map(
    _.transform { case (_, verbs) =>
      verbs.value.toList.foldMap { case (verbId, args) =>
        args.map(arg => ArgumentId(verbId, arg))
      }
    }
  ).toCell("Argument IDs")

  lazy val verbLemmaToType = verbArgSets.get.map(
    _.keySet.toList.foldMap(verbType =>
      NonMergingMap(getVerbLemma(verbType) -> verbType)
    )
  ).toCell("Verb lemma to verb type mapping")

  lazy val verbIdToType = verbArgSets.get.map(
    _.toList.foldMap { case (verbType, verbs) =>
      NonMergingMap(
        verbs.value.toList.map { case (verbId, _) =>
          verbId -> verbType
        }.toMap
      )
    }
  ).toCell("Verb ID to verb type mapping")


  // ELMo XXX TODO remove this Verb vectors

  import breeze.linalg.DenseVector

  def getVerbVectors(
    verbIdToType: Map[VerbId, VerbType],
    filePrefix: String
  ): IO[VerbFeats[DenseVector[Float]]] = {
    val idsPath = Paths.get(filePrefix + "_ids.jsonl")
    val embPath = Paths.get(filePrefix + "_emb.bin")
    val embDim = 1024
    for {
      ids <- Log.infoBranch("Reading verb IDs")(
        FileUtil.readJsonLines[VerbId](idsPath).compile.toList
      )
      embeddings <- Log.infoBranch("Reading verb embeddings")(
        VectorFileUtil.readDenseFloatVectorsNIO(embPath, embDim)
      )
      _ <- Log.info(s"Number of IDs: ${ids.size}; Number of embeddings: ${embeddings.size}; embedding size: ${embeddings.head.size}")
      _ <- {
        val numToCheck = 5
        val propSane = embeddings.take(numToCheck)
          .foldMap(
            _.activeValuesIterator.map(scala.math.abs).filter(f => f > 1e-2 && f < 1e2).size
          ).toDouble / (numToCheck * embDim)
        val sanityCheckText = f"Sanity check: ${propSane}%.3f of ELMo embedding units have absolute value between ${1e-2}%s and ${1e2}%s."
        if(propSane < 0.8) {
          Log.warn(sanityCheckText) >>
            Log.warn("There might be endianness issues with how you're reading the ELMo embeddings") >>
            embeddings.take(numToCheck).traverse(e => Log.info(e.activeValuesIterator.take(10).mkString("\t")))
        } else Log.info(sanityCheckText)
      }
    } yield ids.zip(embeddings)
      .foldMap { case (verbId, embedding) =>
        verbIdToType.get(verbId).foldMap(verbType => // don't include vectors for verbs not in the provided instances
          Map(verbType -> NonMergingMap(verbId -> embedding))
        )
      }
  }

  // ELMo XXX TODO remove these

  lazy val elmoVecs = verbIdToType.get.zip(RunData.strings).flatMap {
    case (vIdToVType, split) =>
      getVerbVectors(
        vIdToVType.value,
        inputDir.resolve(s"elmo/$split").toString
      )
  }.toCell("Verb ELMo vectors")

  // end elmo vecs

  val mlmSettings = List("masked", "symm_left", "symm_right", "symm_both")
  def mlmFeatureDir = inputDir.resolve("mlm")
  def makeMLMFeatures[A](f: (String, Path) => A): Map[String, A] = {
    mlmSettings.map(_ -> "").toMap.transform { case (setting, _) =>
      val mlmSettingDir = mlmFeatureDir.resolve(setting)
      f(setting, mlmSettingDir)
    }
  }

  // setting to lemma to vocab
  lazy val argMLMVocabs: Map[String, Cell[NonMergingMap[String, Vector[String]]]] = {
    makeMLMFeatures { case (setting, path) =>
      new Cell(
        s"MLM argument vocab ($setting)",
        FileUtil.readJson[Map[String, Vector[String]]](path.resolve("arg_vocabs.json"))
          .map(NonMergingMap(_))
      )
    }
  }

  @JsonCodec case class MLMFeatureId(
    sentenceId: String,
    verbLemma: String,
    index: Int)

  // XXX TODO remove verb lemma to type map situation
  // mode -> verb lemma -> sentence id -> arg index -> vector
  val argMLMFeatureDim = 1024
  lazy val argMLMVectors: Map[String, RunDataCell[Map[String, Map[String, NonMergingMap[Int, DenseVector[Float]]]]]] = {
    makeMLMFeatures { case (setting, path) =>
      // TODO sentences is temporary
      RunData.strings.zip(sentences.get).flatMap { case (split, sents) =>
        val idsPath = path.resolve(s"${split}_arg_ids.jsonl.gz")
        val vecPath = path.resolve(s"${split}_arg_vecs.bin")
        // val embPath = Paths.get(filePrefix + "_emb.bin")
        for {
          // TODO vocab is temporary
          vocabs <- argMLMVocabs(setting).get
          ids <- Log.infoBranch("Reading verb IDs")(
            FileUtil.readJsonLines[MLMFeatureId](idsPath).compile.toList
          )
          vecs <- Log.infoBranch("Reading argument MLM vectors")(
            VectorFileUtil.readDenseFloatVectorsNIO(vecPath, argMLMFeatureDim)
          )
          _ <- Log.info(s"Number of IDs: ${ids.size}; Number of vectors: ${vecs.size}; embedding size: ${vecs.head.size}")
          _ <- {
            val numToCheck = 20
            val propSane = vecs.take(numToCheck)
              .foldMap(
                _.activeValuesIterator.filter(f => f >= 0.0 && f <= 1.0).size
              ).toDouble / (numToCheck * argMLMFeatureDim)
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
      }.toCell("Argument MLM Vectors")
    }
  }

  def getArgMLMFeatures(mode: String): RunData[ArgFeatsNew[DenseVector[Float]]] =
    argMLMVectors(mode).get.zip(argIndices).map { case (mlmFeats, getArgIndex) =>
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
    dataset.get.map(
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

  val argIdToSpans: RunDataCell[ArgFeats[List[List[ESpan]]]] = qaPairs.get
    .map(
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
      qaPairs.get.map(
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

  override val sentences = dataset.get
    .map(_.sentences.map { case (sid, sent) => sid -> sent.sentenceTokens })
    .map(NonMergingMap.apply[String, Vector[String]])
    .toCell("QA-SRL sentences")

  // TODO temp before we have distribution results from the models
  // override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
  //   argIdToSpans.get.map(
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
    RunData.strings.zip(verbIdToType.get).zip(qaPairs.get)
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

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] = qaPairs.get.map(
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

  // lazy val instancesByQuestionId = instances.get.map(
  //   _.map { case (verbType, sentences) =>
  //     verbType -> sentences.toList.foldMap { case (sid, verbs) =>
  //       verbs.value.toList.foldMap { case (vi, questions) =>
  //         val verbId = VerbId(sid, vi)
  //         questions.map { case (question, spanLists) =>
  //           QuestionId(verbId, question) -> spanLists
  //         }.toMap
  //       }
  //     }
  //   }
  // ).toCell("Instances by question ID")


  // lazy val questionTemplateVocabsCachePath = cacheDir.map(_.resolve("question-vocabs.jsonl.gz"))
  // lazy val questionTemplateVocabsByVerb = new Cell(
  //   "Question template vocabularies",
  //   questionTemplateVocabsCachePath >>= (path =>
  //     FileCached.get[Map[InflectedForms, Vocab[QuestionTemplate]]]("Question template vocabularies")(
  //       path = path,
  //       read = path => (
  //         FileUtil.readJsonLines[(InflectedForms, Vocab[QuestionTemplate])](path)
  //           .infoCompile("Reading lines")(_.toList).map(_.toMap)
  //       ),
  //       write = (path, clausalQVocabs) => FileUtil.writeJsonLines(
  //         path, io.circe.Printer.noSpaces)(
  //         clausalQVocabs.toList))(
  //       instances.all.get >>= (
  //         _.toList.infoBarTraverse("Constructing clausal question vocabularies") {
  //           case (verbType, sentences) =>
  //             Log.trace(verbType.toString) >> IO {
  //               val qTemplateSet = sentences.unorderedFoldMap(verbs =>
  //                 verbs.value.unorderedFoldMap(qaPairs =>
  //                   qaPairs.keySet.map(QuestionTemplate.fromClausalQuestion)
  //                 )
  //               )
  //               verbType -> Vocab.make(qTemplateSet)
  //             }
  //         }.map(_.toMap)
  //       )
  //     )
  //   )
  // )

  // // inputs sent to a QA model
  // // def qaInputPath = outDir.map(_.resolve(s"qa-input-${mode.eval}.jsonl.gz"))

  // // outputs of a QA model, read back in to construct features
  // def qaOutputPath(split: String) =
  //   IO.pure(inputDir.resolve("qa"))
  //     .flatTap(createDir)
  //     .map(_.resolve(s"qa-output-$split.jsonl.gz"))
  // def qaFeaturesPath(split: String) =
  //   cacheDir.map(_.resolve("qa"))
  //     .flatTap(createDir)
  //     .map(_.resolve(s"qa-features-$split.jsonl.gz"))

  // lazy val answerNLLs = RunData.splits.flatMap { split =>
  //   import QAInputApp.{SentenceQAOutput, ClauseQAOutput}
  //   for {
  //     insts <- instances(split)
  //     vidToType <- verbIdToType(split)
  //     splitName <- RunData.strings(split)
  //     qaOutPath <- qaOutputPath(splitName)
  //     res <- FileUtil.readJsonLines[SentenceQAOutput](
  //       qaOutPath
  //     ).map { case SentenceQAOutput(sid, verbs) =>
  //       verbs.toList.foldMap { case (verbIndexStr, clausalQAs) =>
  //         val verbIndex = verbIndexStr.toInt
  //         val verbId = VerbId(sid, verbIndex)
  //         val verbTypeOpt = vidToType.value.get(verbId)
  //         verbTypeOpt.foldMap { verbType =>
  //           val verbInstances = insts(verbType)(sid).value(verbIndex)
  //           Map(
  //             verbType -> verbInstances.map { case (cq, spans) =>
  //               val goldSpans = spans.flatten.toSet
  //               QuestionId(verbId, cq) -> clausalQAs.map {
  //                 case ClauseQAOutput(question, spans) =>
  //                   val marginalAnswerProb = spans
  //                     .filter(p => goldSpans.contains(p._1))
  //                     .foldMap(_._2)
  //                   // smoothed prob chosen here bc it's 1/2 of the bottom decoding threshold for the QA model
  //                   val smoothedMarginalAnswerProb =
  //                     scala.math.max(marginalAnswerProb, 0.0025)
  //                   question.clausalQ -> -scala.math.log(smoothedMarginalAnswerProb)
  //               }
  //             }
  //           )
  //         }
  //       }
  //     }.infoCompile("Compiling QA NLL features from sentence predictions")(_.foldMonoid)
  //   } yield res
  // }.toFileCachedCell("QA NLL features", qaFeaturesPath)(
  //   read = path => (
  //     FileUtil.readJsonLines[(InflectedForms, List[(QuestionId, List[(TemplateQ, Double)])])](path)
  //       .map { case (k, v) => k -> v.toMap }
  //       .infoCompile("Reading lines")(_.toList)
  //       .map(_.toMap)
  //   ),
  //   write = (path, qaFeatures) => FileUtil.writeJsonLines(
  //     path, io.circe.Printer.noSpaces)(
  //     qaFeatures.toList.map { case (k, v) => k -> v.toList })
  // )


  val liveDir = IO.pure(rootDir.resolve("live")).flatTap(createDir)

  // TODO refactor into RunData framework
  val evaluationItemsPath = liveDir.map(_.resolve(s"eval-sample-${splitLabels.eval}.jsonl"))
  val evaluationItems = {
      new Cell(
        "Evaluation items",
        evaluationItemsPath >>= (evalItemsPath =>
          FileCached.get[Vector[(InflectedForms, String, Int)]](
            "Evaluation Items")(
            path = evalItemsPath,
            read = path => FileUtil.readJsonLines[(InflectedForms, String, Int)](path).compile.toVector,
            write = (path, items) => FileUtil.writeJsonLines(path)(items))(
            Log.infoBranch(s"Creating new sample for evaluation at $evalItemsPath")(
              dataset.eval.get.map { evalSet =>
                (new scala.util.Random(86735932569L)).shuffle(
                  evalSet.sentences.values.iterator.flatMap(sentence =>
                    sentence.verbEntries.values.toList.map(verb =>
                      (verb.verbInflectedForms, sentence.sentenceId, verb.verbIndex)
                    )
                  )
                ).take(1000).toVector
              }
            )
          )
        )
    )
  }

  val paraphraseGoldPath = liveDir.map(_.resolve("gold-paraphrases.json"))

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

abstract class PropBankFeatures[Arg](
  mode: RunMode,
  val assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[String, Arg](mode)(implicitly[Encoder[String]], implicitly[Decoder[String]], cs, Log) {

  override def getVerbLemma(verbType: String): String = {
    if(assumeGoldVerbSense) verbType.takeWhile(_ != '.')
    else verbType
  }

  // don't store the models in the same dir, because they cluster different kinds of things
  override def modelDir = super.modelDir.map(
    _.resolve(if(assumeGoldVerbSense) "sense" else "lemma")
  ).flatTap(createDir)

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
    dataset.get.map(sents =>
      NonMergingMap(
        sents.value.map { case (sid, sent) =>
          sid -> sent.tokens.map(_.token)
        }
      )
    ).toCell("CoNLL 2008 sentence index")

  val predArgStructures: RunDataCell[VerbFeats[PredicateArgumentStructure]] = {
    dataset.get.map(
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

  val argDependencyRels: RunDataCell[ArgFeats[String]] = {
    dataset.get.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        val dependencies = sentence.childToParentDependencies
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma

          val verbIndex = pas.predicate.index
          val verbId = VerbId(sid, verbIndex)

          val argDependencyRels = pas.arguments.map(_._2).map { argIndex =>
            ArgumentId(verbId, argIndex) -> dependencies(argIndex)._1
          }.toMap

          Map(verbType -> NonMergingMap(argDependencyRels))
        }
      }
    ).toCell("CoNLL 2008 argument-to-head dependency rels")
  }

  val argDependencyPaths: RunDataCell[ArgFeats[String]] = {
    dataset.get.map(
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

  override val verbArgSets: RunDataCell[VerbFeats[Set[Int]]] = predArgStructures.get.map(
    mapVerbFeats(_.arguments.map(_._2).toSet)
  ).toCell("CoNLL 2008 gold arg indices")

  // override val verbArgSets: RunDataCell[VerbFeats[Set[Int]]] = predArgStructures.get.map(
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
    predArgStructures.get.map(
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

  val mlmFeatureGenInputs = dataset.get.map { sentences =>
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
  }.run

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

  val dataset: RunDataCell[VerbFeats[(String, Map[ESpan, String])]] = RunData.strings.zip(index.get)
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

  // TODO eliminate redundant traversal of propbank
  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    index.get.flatMap(
      _.infoBarFoldMapM("Constructing sentence index") { path =>
        ontonotesService.getFile(path).map(
          _.sentences.foldMap(s => NonMergingMap(s.path.toString -> s.tokens.map(_.token).toVector))
        )
      }
    ).toCell("Sentence index")

  override val verbArgSets = dataset.get.map(
    _.transform { case (_, verbs) =>
      NonMergingMap(
        verbs.value.map { case (verbId, (_, labels)) =>
          verbId -> labels.keySet
        }
      )
    }
  ).toCell("PropBank gold span instances")

  override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
    RunData.strings.zip(verbIdToType.get).flatMap { case (split, vidToType) =>
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

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] = dataset.get.map(
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

  override val argRoleLabels: RunDataCell[ArgFeats[PropBankRoleLabel]] = dataset.get.map(
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

  val qgInputs = index.get.map { paths =>
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
  }.run

  override val setup = writeQGInputs
}
