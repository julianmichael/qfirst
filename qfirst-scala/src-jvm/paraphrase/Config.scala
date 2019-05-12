package qfirst.paraphrase
import qfirst._
import qfirst.protocols.SimpleQAs

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot
import qasrl.labeling.SlotBasedLabel

import qasrl.bank.Data
import qasrl.bank.FullData


import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._

import fs2.Stream

import EvalApp.ParaphraseAnnotations

import io.circe.generic.JsonCodec

import qfirst.ClauseResolution.ArgStructure

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
class Cell[A](create: IO[A]) {
  private[this] var value: Option[A] = None
  def get: IO[A] = value
    .map(IO.pure)
    .getOrElse(create.flatTap(a => IO { value = Some(a) }))
  def isPresent = IO(value.nonEmpty)
}

case class Config(mode: RunMode)(implicit cs: ContextShift[IO]) {
  val outputDir = Paths.get("frame-induction")
  val qasrlBankPath = Paths.get("qasrl-v2_1")
  val qasrlElmoPath = Paths.get("qasrl-v2-elmo")
  val propBankPredictionsPath = Paths.get("propbank-data/predictions")
  val propBankElmoPath = Paths.get("propbank-data/elmo")
  val trainElmoPrefix = qasrlElmoPath.resolve("train").toString
  val devElmoPrefix = qasrlElmoPath.resolve("dev").toString
  val testElmoPrefix = qasrlElmoPath.resolve("test").toString
  val inputElmoPrefix = if(mode.sanity) devElmoPrefix else trainElmoPrefix
  val evalElmoPrefix = if(mode.test) testElmoPrefix else devElmoPrefix
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

  val globalResultsDir = configDir
    .map(_.resolve("all-results"))
    .flatTap(createDir)

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  val qasrlBank = new Cell(
    logOp(
      "Reading QA-SRL Bank",
      Data.readFromQasrlBank(qasrlBankPath).toEither.right.get
    )
  )

  private[this] def readQasrlDataset(name: String) = logOp(
    s"Reading QA-SRL dataset $name",
    readDataset(qasrlBankPath.resolve(name + ".jsonl.gz"))
  )

  def getGoldInstances(dataset: Dataset): FrameInductionApp.Instances = {
    dataset.sentences
      .iterator.flatMap { case (sid, sentence) => sentence.verbEntries.values.map(sid -> _) }.toList
      .groupBy(_._2.verbInflectedForms).map { case (verbInflectedForms, pairs) =>
        verbInflectedForms -> pairs.groupBy(_._1).map { case (sid, pairs) =>
          sid -> pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
            verbIndex -> verb.questionLabels.map { case (qString, qLabel) =>
              qLabel.questionSlots -> (
                qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
              )
            }
          }
        }
      }
  }

  val train = new Cell(
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.trainExpanded),
      readQasrlDataset("expanded/train")
    ).map(filterDatasetNonDense)
  )
  val trainInstances = new Cell(
    train.get.map(getGoldInstances)
  )

  val dev = new Cell(
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.devOrig),
      readQasrlDataset("orig/dev")
    ).map(filterDatasetNonDense)
  )
  val devInstances = new Cell(
    dev.get.map(getGoldInstances)
  )

  val test = new Cell(
    qasrlBank.isPresent.ifM(
      qasrlBank.get.map(_.testOrig),
      readQasrlDataset("orig/test")
    ).map(filterDatasetNonDense)
  )
  val testInstances = new Cell(
    test.get.map(getGoldInstances)
  )

  val input = if(mode.sanity) dev else train
  val inputInstances = if(mode.sanity) devInstances else trainInstances

  val eval = if(mode.test) test else dev
  val evalInstances = if(mode.test) testInstances else devInstances

  val full = new Cell(for(i <- input.get; e <- eval.get) yield i |+| e)
  val fullInstances = new Cell(for(i <- inputInstances.get; e <- evalInstances.get) yield i |+| e)

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
  ): IO[FrameInductionApp.PropBankInstances] = {
    val protocol = SimpleQAs.protocol[SlotBasedLabel[VerbForm]](useMaxQuestionDecoding = false)
    predictions.map { sentencePred =>
      sentencePred.verbs.filter(_.beam.nonEmpty).foldMap(
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
    }.compile.foldMonoid
  }

  def getPropBankSenseLabels(
    predictions: Stream[IO, PropBankSentencePrediction[QABeam]]
  ): IO[FrameInductionApp.PropBankLabels] = {
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
    )
  }

  val propBankQasrlFilter = {
    import io.circe.generic.auto._
    new Cell(
      FileUtil.readJson[SimpleQAs.Filter](
        propBankPredictionsPath.resolve(s"filter.json")
      )
    )
  }

  def readPropBankInstances(name: String) = {
    import io.circe.generic.auto._
    import qasrl.data.JsonCodecs._
    logOp(
      s"Reading QA-SRL on PropBank $name set",
      propBankQasrlFilter.get.flatMap(filter =>
        getPropBankPredictedInstances(
          FileUtil.readJsonLines[PropBankSentencePrediction[QABeam]](
            propBankPredictionsPath.resolve(s"propbank-$name-qasrl.jsonl.gz")
          ), filter
        )
      )
    )
  }

  val propBankTrainInstances = new Cell(readPropBankInstances("train"))
  val propBankDevInstances = new Cell(readPropBankInstances("dev"))
  val propBankTestInstances = new Cell(readPropBankInstances("test"))

  val propBankInputInstances = if(mode.sanity) propBankDevInstances else propBankTrainInstances
  val propBankEvalInstances = if(mode.test) propBankTestInstances else propBankDevInstances
  val propBankFullInstances = new Cell(for(i <- propBankInputInstances.get; e <- propBankEvalInstances.get) yield i |+| e)

  def readPropBankLabels(name: String) = {
    import io.circe.generic.auto._
    import qasrl.data.JsonCodecs._
    logOp(
      s"Reading verb sense labels on PropBank $name set",
      getPropBankSenseLabels(
        FileUtil.readJsonLines[PropBankSentencePrediction[QABeam]](
          propBankPredictionsPath.resolve(s"propbank-$name-qasrl.jsonl.gz")
        )
      )
    )
  }

  val propBankTrainLabels = new Cell(readPropBankLabels("train"))
  val propBankDevLabels = new Cell(readPropBankLabels("dev"))
  val propBankTestLabels = new Cell(readPropBankLabels("test"))

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
      _ <- IO(println(s"Number of IDs: ${ids.size}; Number of embeddings: ${embeddings.size}; embedding size: ${embeddings.head.size}"))
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

  val trainElmo = new Cell(train.get.flatMap(dataset => getGoldELMoInstances(dataset, trainElmoPrefix)))
  val devElmo = new Cell(dev.get.flatMap(dataset => getGoldELMoInstances(dataset, devElmoPrefix)))
  val testElmo = new Cell(test.get.flatMap(dataset => getGoldELMoInstances(dataset, testElmoPrefix)))
  val inputElmo = if(mode.sanity) devElmo else trainElmo
  val evalElmo = if(mode.test) testElmo else devElmo
  val fullElmo = new Cell(for(input <- inputElmo.get; eval <- evalElmo.get) yield input |+| eval)

  val collapsedQAOutputs = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    new Cell(
      fileCached[Map[InflectedForms, Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double]]](
        path = collapsedQAOutputPath,
        read = path => (
          FileUtil.readJsonLines[(InflectedForms, List[(((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double)])](path)
            .map { case (k, v) => k -> v.toMap }.compile.toList.map(_.toMap)
        ),
        write = (path, collapsedQAOutputs) => logOp(
          "Writing collapsed QA outputs",
          FileUtil.writeJsonLines(collapsedQAOutputPath, io.circe.Printer.noSpaces)(collapsedQAOutputs.toList.map { case (k, v) => k -> v.toList })
        )
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
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    new Cell(
      fileCached[Vector[(InflectedForms, String, Int)]](
        path = evaluationItemsPath,
        read = path => FileUtil.readJsonLines[(InflectedForms, String, Int)](path).compile.toVector,
        write = (path, items) => FileUtil.writeJsonLines(path)(items))(
        logOp(
          s"Creating new sample for evaluation at $evaluationItemsPath", eval.get.map { evalSet =>
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
      IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
        IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
    } else FileUtil.readJson[ParaphraseAnnotations](paraphraseGoldPath)
  }
  def saveGoldParaphrases(data: ParaphraseAnnotations)(implicit cs: ContextShift[IO]) = {
    FileUtil.writeJson(paraphraseGoldPath, io.circe.Printer.noSpaces)(data)
  }

  def modelDir(verbSenseConfig: VerbSenseConfig) = {
    configDir.map(_.resolve(s"${verbSenseConfig.modelName}")).flatTap(createDir)
  }
  def verbClustersPath(verbSenseConfig: VerbSenseConfig) = {
    modelDir(verbSenseConfig).map(_.resolve(s"clusters.jsonl.gz"))
  }
  // def framesetsPath(verbSenseConfig: VerbSenseConfig) = {
  //   modelDir(verbSenseConfig).map(_.resolve(s"framesets.jsonl.gz"))
  // }
  def resultsPath(verbSenseConfig: VerbSenseConfig) = {
    modelDir(verbSenseConfig).map(_.resolve(s"results")).flatTap(createDir)
  }

  def getCachedVerbModels(verbSenseConfig: VerbSenseConfig): IO[Option[Map[InflectedForms, VerbClusterModel]]] = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    verbClustersPath(verbSenseConfig).flatMap(path =>
      fileCached[Option[Map[InflectedForms, VerbClusterModel]]](
        path, read = path => FileUtil.readJsonLines[(InflectedForms, VerbClusterModel)](path)
          .compile.toList.map(l => Some(l.toMap)),
        write = (_, _) => IO.unit
      )(compute = IO(None))
    )
  }
  def cacheVerbModels(verbSenseConfig: VerbSenseConfig, clusters: Map[InflectedForms, VerbClusterModel]): IO[Unit] = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    verbClustersPath(verbSenseConfig).flatMap(path =>
      FileUtil.writeJsonLines(path)(clusters.toList)
    )
  }
  def cacheVerbModelComputation(
    verbSenseConfig: VerbSenseConfig)(
    computeVerbModels: IO[Map[InflectedForms, VerbClusterModel]]
  ): IO[Map[InflectedForms, VerbClusterModel]] = {
    getCachedVerbModels(verbSenseConfig).flatMap {
      case Some(verbModels) => IO.pure(verbModels)
      case None => for {
        models <- computeVerbModels
        _ <- cacheVerbModels(verbSenseConfig, models)
      } yield models
    }
  }

  def getCachedPropBankVerbModels(verbSenseConfig: VerbSenseConfig): IO[Option[Map[String, PropBankVerbClusterModel]]] = {
    verbClustersPath(verbSenseConfig).flatMap(path =>
      fileCached[Option[Map[String, PropBankVerbClusterModel]]](
        path, read = path => FileUtil.readJsonLines[(String, PropBankVerbClusterModel)](path)
          .compile.toList.map(l => Some(l.toMap)),
        write = (_, _) => IO.unit
      )(compute = IO(None))
    )
  }
  def cachePropBankVerbModels(verbSenseConfig: VerbSenseConfig, clusters: Map[String, PropBankVerbClusterModel]): IO[Unit] = {
    verbClustersPath(verbSenseConfig).flatMap(path =>
      FileUtil.writeJsonLines(path)(clusters.toList)
    )
  }
  def cachePropBankVerbModelComputation(
    verbSenseConfig: VerbSenseConfig)(
    computeVerbModels: IO[Map[String, PropBankVerbClusterModel]]
  ): IO[Map[String, PropBankVerbClusterModel]] = {
    getCachedPropBankVerbModels(verbSenseConfig).flatMap {
      case Some(verbModels) => IO.pure(verbModels)
      case None => for {
        models <- computeVerbModels
        _ <- cachePropBankVerbModels(verbSenseConfig, models)
      } yield models
    }
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
