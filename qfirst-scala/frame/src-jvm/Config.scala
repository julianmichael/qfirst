package qfirst.frame
import qfirst.model.eval.protocols.SimpleQAs

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot
import qasrl.labeling.SlotBasedLabel

import qasrl.bank.Data
import qasrl.bank.FullData


import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileUtil

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec

import qfirst.clause.ArgStructure

import freelog._
import freelog.implicits._

@JsonCodec sealed trait VerbSenseConfig {
  import VerbSenseConfig._
  def modelName = this match {
    case SingleCluster => "single"
    case EntropyOnly => "entropy"
    case ELMoOnly => "elmo"
    case i @ Interpolated(entropyLambda) =>
      f"entropy$entropyLambda%.2f-elmo${i.elmoLambda}%.2f"
  }
}
object VerbSenseConfig {
  case object SingleCluster extends VerbSenseConfig
  case object EntropyOnly extends VerbSenseConfig
  case object ELMoOnly extends VerbSenseConfig
  // case object SetOnly extends VerbSenseConfig
  case class Interpolated(
    entropyLambda: Double
  ) extends VerbSenseConfig {
    val elmoLambda: Double = 1.0 - entropyLambda
    def lambdas = List(entropyLambda, elmoLambda)
    assert(lambdas.forall(l => 0.0 < l && l < 1.0))
  }
  object DoubleMatch {
    def unapply(s: String) = scala.util.Try(s.toDouble).toOption
  }
  def fromString(s: String) = s match {
    case "single" => Some(SingleCluster)
    case "entropy" => Some(EntropyOnly)
    case "elmo" => Some(ELMoOnly)
    case DoubleMatch(d) => Some(Interpolated(d))
    case _ => None
  }
}

// circumvent side-effect of ref creation
class Cell[A](name: String, create: IO[A])(implicit Log: TreeLogger[IO, String]) {
  private[this] var value: Ref[IO, Option[A]] = Ref[IO].of[Option[A]](None).unsafeRunSync
  val get: IO[A] = value.get.flatMap(innerValue =>
    innerValue.map(a => Log.debug(s"Retrieving in-memory cached value: $name").as(a))
      .getOrElse(Log.debugBranch(s"Computing and caching value: $name")(create.flatTap(a => value.set(Some(a)))))
  )
  val isPresent = value.get.map(_.nonEmpty)
}

case class Config(mode: RunMode)(
  implicit cs: ContextShift[IO], Log: EphemeralTreeLogger[IO, String]
) {

  val outputDir = Paths.get("frame-induction")
  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")
  val qasrlElmoPath = Paths.get("qasrl-v2-elmo")
  val trainElmoPrefix = qasrlElmoPath.resolve("train").toString
  val devElmoPrefix = qasrlElmoPath.resolve("dev").toString
  val testElmoPrefix = qasrlElmoPath.resolve("test").toString
  // val inputElmoPrefix = if(mode.sanity) devElmoPrefix else trainElmoPrefix
  // val evalElmoPrefix = if(mode.test) testElmoPrefix else devElmoPrefix
  val propBankPredictionsPath = Paths.get("propbank-data/predictions")
  val propBankElmoPath = Paths.get("propbank-data/elmo")
  val propBankTrainElmoPrefix = propBankElmoPath.resolve("train").toString
  val propBankDevElmoPrefix = propBankElmoPath.resolve("dev").toString
  val propBankTestElmoPrefix = propBankElmoPath.resolve("test").toString
  val qaInputPath = outputDir.resolve(s"qa-input-${mode.devOrTest}.jsonl.gz")
  val qaOutputPath = outputDir.resolve(s"qa-output-${mode.devOrTest}.jsonl.gz")
  val collapsedQAOutputPath = outputDir.resolve(s"qa-output-${mode.devOrTest}-collapsed.jsonl.gz")
  val evaluationItemsPath = outputDir.resolve(s"eval-sample-${mode.devOrTest}.jsonl")
  val paraphraseGoldPath = outputDir.resolve("gold-paraphrases.json")

  private[this] val createDir = (path: Path) => IO(!Files.exists(path))
    .ifM(IO(Files.createDirectories(path)), IO.unit)

  val configDir = IO.pure(
    outputDir.resolve(mode.toString)
  ).flatTap(createDir)

  val propBankConfigDir = IO.pure(
    outputDir.resolve("propbank").resolve(mode.toString)
  ).flatTap(createDir)

  val globalResultsDir = configDir
    .map(_.resolve("all-results"))
    .flatTap(createDir)

  val globalPropBankResultsDir = propBankConfigDir
    .map(_.resolve("all-results"))
    .flatTap(createDir)

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  val qasrlBank = new Cell(
    "QA-SRL Bank",
    Log.infoBranch("Reading QA-SRL Bank")(
      IO(Data.readFromQasrlBank(qasrlBankPath).toEither.right.get)
    )
  )

  private[this] def readQasrlDataset(name: String) =
    Log.infoBranch(s"Reading QA-SRL dataset $name")(
      readDataset(qasrlBankPath.resolve(name + ".jsonl.gz"))
    )

  def getGoldInstances(dataset: Dataset): Instances.Qasrl = {
    Instances(
      dataset.sentences
        .iterator.flatMap { case (sid, sentence) => sentence.verbEntries.values.map(sid -> _) }.toList
        .groupBy(_._2.verbInflectedForms).map { case (verbInflectedForms, pairs) =>
          verbInflectedForms -> pairs.groupBy(_._1).map { case (sid, pairs) =>
            sid -> pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
              verbIndex -> verb.questionLabels.map { case (qString, qLabel) =>
                qLabel.questionSlots -> (
                  qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList).toSet
                )
              }
            }
          }
        }
    )
  }

  val train = new Cell(
    "QA-SRL train",
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.trainExpanded),
      readQasrlDataset("expanded/train")
    ).map(filterDatasetNonDense)
  )
  val trainInstances = new Cell(
    "QA-SRL train instances",
    train.get.map(getGoldInstances)
  )

  val dev = new Cell(
    "QA-SRL dev",
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.devOrig),
      readQasrlDataset("orig/dev")
    ).map(filterDatasetNonDense)
  )
  val devInstances = new Cell(
    "QA-SRL dev instances",
    dev.get.map(getGoldInstances)
  )

  val test = new Cell(
    "QA-SRL test",
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.testOrig),
      readQasrlDataset("orig/test")
    ).map(filterDatasetNonDense)
  )
  val testInstances = new Cell(
    "QA-SRL test instances",
    test.get.map(getGoldInstances)
  )

  val input = if(mode.sanity) dev else train
  val inputInstances = if(mode.sanity) devInstances else trainInstances

  val eval = if(mode.test) test else dev
  val evalInstances = if(mode.test) testInstances else devInstances

  val full = new Cell("QA-SRL full", for(i <- input.get; e <- eval.get) yield i |+| e)
  val fullInstances = new Cell("QA-SRL full instances", for(i <- inputInstances.get; e <- evalInstances.get) yield i |+| e)

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
  //           )
  //         )
  //       )
  //     )
  //   }.compile.foldMonoid
  // }

  type QABeam = List[SimpleQAs.BeamItem[SlotBasedLabel[VerbForm]]]

  def getPropBankPredictedInstances(
    predictions: Stream[IO, PropBankSentencePrediction[QABeam]],
    filter: SimpleQAs.Filter
  ): IO[Instances.PropBank] = {
    val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)
    predictions.map { sentencePred => // remove "be" since out of scope of QA-SRL
      sentencePred.verbs.filter(v => v.verbLemma != "be" && v.beam.nonEmpty).foldMap(
        verbPred => Map(
          verbPred.verbLemma -> Map(
            sentencePred.sentenceId -> Map(
              verbPred.verbIndex ->
                protocol.filterBeam(filter, verbPred.toGenericVerbPrediction).map {
                  case (qString, (slots, spans)) => slots -> spans
                }
            ).filter(_._2.nonEmpty)
          ).filter(_._2.nonEmpty)
        ).filter(_._2.nonEmpty)
      )
    }.compile.foldMonoid.map(Instances(_))
  }

  def getPropBankSenseLabels(
    predictions: Stream[IO, PropBankSentencePrediction[QABeam]]
  ): IO[Instances.PropBankLabels] = {
    val resIO = predictions.map { sentencePred =>
      sentencePred.verbs.filter(_.beam.nonEmpty).foldMap(
        verbPred => Map(
          verbPred.verbLemma -> Map(
            sentencePred.sentenceId -> Map(
              verbPred.verbIndex -> Vector(verbPred.verbSense)
            )
          )
        )
      )
    }.compile.foldMonoid
    resIO.map(
      _.transform { case (_, verbTypeMap) =>
        verbTypeMap.transform { case (_, sentenceMap) =>
          sentenceMap.transform { case (_, verbSenseVec) =>
            assert(verbSenseVec.size == 1)
            verbSenseVec.head
          }
        }
      }
    ).map(Instances(_))
  }

  val propBankQasrlFilter = {
    import io.circe.generic.auto._
    new Cell(
      "PropBank QA-SRL Filter",
      FileUtil.readJson[SimpleQAs.Filter](
        propBankPredictionsPath.resolve(s"filter.json")
      )
    )
  }

  def readPropBankInstances(name: String) = {
    import io.circe.generic.auto._
    Log.infoBranch(s"Reading QA-SRL on PropBank $name set")(
      propBankQasrlFilter.get.flatMap(filter =>
        for {
          bad <- Ref[IO].of(0)
          total <- Ref[IO].of(0)
          res <- getPropBankPredictedInstances(
            FileUtil.readJsonLines[PropBankSentencePrediction[QABeam]](
              propBankPredictionsPath.resolve(s"propbank-$name-qasrl.jsonl.gz")
            ).flatMap { x =>
              import io.circe.syntax._
              if(x.verbs.exists(_.beam.isEmpty)) {
                Stream.eval_(bad.update(_ + 1) >> total.update(_ + 1))
              } else Stream.eval(total.update(_ + 1).as(x))
            }, filter
          )
          _ <- (bad.get, total.get).mapN((b, t) =>
            Log.info(s"Ignored $b/$t sentences due to apparently bad verbs.")
          ).flatten
        } yield res
      )
    )
  }

  val propBankTrainInstances = new Cell("PropBank train instances", readPropBankInstances("train"))
  val propBankDevInstances = new Cell("PropBank dev instances", readPropBankInstances("dev"))
  val propBankTestInstances = new Cell("PropBank test instances", readPropBankInstances("test"))

  val propBankInputInstances = if(mode.sanity) propBankDevInstances else propBankTrainInstances
  val propBankEvalInstances = if(mode.test) propBankTestInstances else propBankDevInstances
  val propBankFullInstances = new Cell("PropBank full instances", for(i <- propBankInputInstances.get; e <- propBankEvalInstances.get) yield i |+| e)

  def readPropBankLabels(name: String) = {
    import io.circe.generic.auto._
    Log.infoBranch(s"Reading verb sense labels on PropBank $name set")(
      getPropBankSenseLabels(
        FileUtil.readJsonLines[PropBankSentencePrediction[QABeam]](
          propBankPredictionsPath.resolve(s"propbank-$name-qasrl.jsonl.gz")
        )
      )
    )
  }

  val propBankTrainLabels = new Cell("PropBank train labels", readPropBankLabels("train"))
  val propBankDevLabels = new Cell("PropBank dev labels", readPropBankLabels("dev"))
  val propBankTestLabels = new Cell("PropBank test labels", readPropBankLabels("test"))
  val propBankFullLabels = if(mode.sanity) propBankDevLabels else propBankTrainLabels
  val propBankEvalLabels = if(mode.test) propBankTestLabels else propBankDevLabels

  import breeze.linalg.DenseVector

  def getGoldELMoInstances(
    dataset: Dataset,
    filePrefix: String
  ): IO[Instances.QasrlElmo] = {
    val idsPath = Paths.get(filePrefix + "_ids.jsonl")
    val embPath = Paths.get(filePrefix + "_emb.bin")
    val embDim = 1024
    for {
      ids <- Log.infoBranch("Reading verb IDs")(
        FileUtil.readJsonLines[VerbId](idsPath).compile.toList
      )
      embeddings <- Log.infoBranch("Reading verb embeddings")(
        VectorFileUtil.readDenseFloatVectorsNIO(embPath, embDim).flatTap { embeddings =>
          val numToCheck = 5
          val propSane = embeddings.take(numToCheck)
            .foldMap(
              _.activeValuesIterator.map(scala.math.abs).filter(f => f > 1e-2 && f < 1e2).size
            ).toDouble / (numToCheck * embDim)
          val sanityCheckText = f"Sanity check: ${propSane}%.3f of ELMo embedding units have absolute value between ${1e-2}%s and ${1e2}%s."
          Log.info(s"Number of IDs: ${ids.size}; Number of embeddings: ${embeddings.size}; embedding size: ${embeddings.head.size}") >> {
            if(propSane < 0.8) {
              Log.warn(sanityCheckText) >>
                Log.warn("There might be endianness issues with how you're reading the ELMo embeddings") >>
                embeddings.take(numToCheck).traverse(e => Log.info(e.activeValuesIterator.take(10).mkString("\t")))
            } else Log.info(sanityCheckText)
          }
        }
      )
    } yield Instances(
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

  val trainElmo = new Cell("Train ELMo", train.get.flatMap(dataset => getGoldELMoInstances(dataset, trainElmoPrefix)))
  val devElmo = new Cell("Dev ELMo", dev.get.flatMap(dataset => getGoldELMoInstances(dataset, devElmoPrefix)))
  val testElmo = new Cell("Test ELMo", test.get.flatMap(dataset => getGoldELMoInstances(dataset, testElmoPrefix)))
  val inputElmo = if(mode.sanity) devElmo else trainElmo
  val evalElmo = if(mode.test) testElmo else devElmo
  val fullElmo = new Cell("Full ELMo", for(input <- inputElmo.get; eval <- evalElmo.get) yield input |+| eval)

  def getPropBankELMoInstances(
    verbIdToLemma: Map[VerbId, String],
    filePrefix: String
  ): IO[Instances.PropBankElmo] = {
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
    } yield Instances(
      ids.zip(embeddings).foldMap { case (vid @ VerbId(sentenceId, verbIndex), embedding) =>
        verbIdToLemma.get(vid).foldMap(verbLemma => // don't include vectors for verbs not in the provided instances
          Map(verbLemma -> Map(sentenceId -> Map(verbIndex -> List(embedding))))
        )
      }
    ).map(_.head)
  }

  private[this] def instancesToVerbIdMap(instances: Instances.PropBank) = {
    instances.values.toList.foldMap { case (verbLemma, verbTypeMap) =>
      verbTypeMap.toList.foldMap { case (sentenceId, sentenceMap) =>
        sentenceMap.toList.foldMap { case (verbIndex, _) =>
          Map(VerbId(sentenceId, verbIndex) -> verbLemma)
        }
      }
    }
  }

  val propBankTrainElmo = new Cell(
    "PropBank train ELMo",
    propBankTrainInstances.get.map(instancesToVerbIdMap).flatMap(
      getPropBankELMoInstances(_, propBankTrainElmoPrefix)
    )
  )
  val propBankDevElmo = new Cell(
    "PropBank dev ELMo",
    propBankDevInstances.get.map(instancesToVerbIdMap).flatMap(
      getPropBankELMoInstances(_, propBankDevElmoPrefix)
    )
  )
  val propBankTestElmo = new Cell(
    "PropBank test ELMo",
    propBankTestInstances.get.map(instancesToVerbIdMap).flatMap(
      getPropBankELMoInstances(_, propBankTestElmoPrefix)
    )
  )
  val propBankInputElmo = if(mode.sanity) propBankDevElmo else propBankTrainElmo
  val propBankEvalElmo = if(mode.test) propBankTestElmo else propBankDevElmo
  val propBankFullElmo = new Cell("PropBank full ELMo", for(i <- propBankInputElmo.get; e <- propBankEvalElmo.get) yield i |+| e)

  val collapsedQAOutputs = {
    new Cell(
      "Collapsed QA outputs",
      fileCached[Map[InflectedForms, Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double]]](
        "Collapsed QA outputs")(
        path = collapsedQAOutputPath,
        read = path => (
          FileUtil.readJsonLines[(InflectedForms, List[(((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double)])](path)
            .map { case (k, v) => k -> v.toMap }.compile.toList.map(_.toMap)
        ),
        write = (path, collapsedQAOutputs) => FileUtil.writeJsonLines(
          collapsedQAOutputPath, io.circe.Printer.noSpaces)(
          collapsedQAOutputs.toList.map { case (k, v) => k -> v.toList })
      )( // compute
        for {
          trainSet <- train.get
          evalSet <- eval.get
          collapsedQAOutputs <- QAInputApp.getCollapsedFuzzyArgumentEquivalences(
            trainSet |+| evalSet,
            FileUtil.readJsonLines[QAInputApp.SentenceQAOutput](qaOutputPath)
          )
        } yield collapsedQAOutputs
      )
    )
  }

  val evaluationItems = {
    new Cell(
      "Evaluation items",
      fileCached[Vector[(InflectedForms, String, Int)]](
        "Evaluation Items")(
        path = evaluationItemsPath,
        read = path => FileUtil.readJsonLines[(InflectedForms, String, Int)](path).compile.toVector,
        write = (path, items) => FileUtil.writeJsonLines(path)(items))(
        Log.infoBranch(s"Creating new sample for evaluation at $evaluationItemsPath")(
          eval.get.map { evalSet =>
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
  }

  def readGoldParaphrases(implicit cs: ContextShift[IO]) = {
    if(!Files.exists(paraphraseGoldPath)) {
      Log.warn(s"No gold paraphrase annotations found at $paraphraseGoldPath. Initializing to empty annotations.") >>
        IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
    } else Log.infoBranch(s"Reading gold paraphrases from $paraphraseGoldPath")(
      FileUtil.readJson[ParaphraseAnnotations](paraphraseGoldPath)
    )
  }
  def saveGoldParaphrases(data: ParaphraseAnnotations)(implicit cs: ContextShift[IO]) = {
    Log.infoBranch(s"Saving gold paraphrases to $paraphraseGoldPath")(
      FileUtil.writeJson(paraphraseGoldPath, io.circe.Printer.noSpaces)(data)
    )
  }

  def modelDir(verbSenseConfig: VerbSenseConfig) = {
    configDir.map(_.resolve(s"${verbSenseConfig.modelName}")).flatTap(createDir)
  }
  def propBankModelDir(verbSenseConfig: VerbSenseConfig) = {
    propBankConfigDir.map(_.resolve(s"${verbSenseConfig.modelName}")).flatTap(createDir)
  }

  def verbClustersPath(verbSenseConfig: VerbSenseConfig) = {
    modelDir(verbSenseConfig).map(_.resolve(s"clusters.jsonl.gz"))
  }
  def propBankVerbClustersPath(verbSenseConfig: VerbSenseConfig) = {
    propBankModelDir(verbSenseConfig).map(_.resolve(s"clusters.jsonl.gz"))
  }
  def resultsPath(verbSenseConfig: VerbSenseConfig) = {
    modelDir(verbSenseConfig).map(_.resolve(s"results")).flatTap(createDir)
  }
  def propBankResultsPath(verbSenseConfig: VerbSenseConfig) = {
    propBankModelDir(verbSenseConfig).map(_.resolve(s"results")).flatTap(createDir)
  }

  def cacheVerbModelComputation(
    verbSenseConfig: VerbSenseConfig)(
    computeVerbModels: IO[Map[InflectedForms, VerbClusterModel]]
  ): IO[Map[InflectedForms, VerbClusterModel]] = {
    verbClustersPath(verbSenseConfig).flatMap(path =>
      fileCached[Map[InflectedForms, VerbClusterModel]](
        "Verb Models")(
        path, read = path => FileUtil.readJsonLines[(InflectedForms, VerbClusterModel)](path)
          .compile.toList.map(_.toMap),
        write = (path, clusters) => FileUtil.writeJsonLines(path)(clusters.toList)
      )(compute = computeVerbModels)
    )
  }

  def cachePropBankVerbModelComputation(
    verbSenseConfig: VerbSenseConfig)(
    computeVerbModels: IO[Map[String, PropBankVerbClusterModel]]
  ): IO[Map[String, PropBankVerbClusterModel]] = {
    propBankVerbClustersPath(verbSenseConfig).flatMap(path =>
      fileCached[Map[String, PropBankVerbClusterModel]](
        "PropBank Verb Models")(
        path, read = path => FileUtil.readJsonLines[PropBankVerbClusterModel](path)
          .compile.toList.map(_.map(m => m.verbLemma -> m).toMap),
        write = (path, clusters) =>
        Log.info("Max cluster tree depth: " + clusters.toList.map(_._2.clusterTree.depth).max.toString) >>
          FileUtil.writeLines[PropBankVerbClusterModel](path, _.toJsonStringSafe)(clusters.toList.map(_._2))
      )(compute = computeVerbModels)
    )
  }

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
