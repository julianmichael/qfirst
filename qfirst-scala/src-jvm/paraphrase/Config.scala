package qfirst.paraphrase
import qfirst.MergeTree
import qfirst.FileUtil

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot

import qasrl.bank.Data
import qasrl.bank.FullData


import nlpdata.datasets.wiktionary.InflectedForms

import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._

import EvalApp.ParaphraseAnnotations

import qfirst.ClauseResolution.ArgStructure

sealed trait RunMode {
  import RunMode._
  override def toString = this match {
    case Sanity => "sanity"
    case Dev => "dev"
    case Test => "test"
  }
  def devOrTest: String = this match {
    case Sanity => "dev"
    case Dev => "dev"
    case Test => "test"
  }
  def sanity = this match {
    case Sanity => true
    case _ => false
  }
  def test = this match {
    case Test => true
    case _ => false
  }
}
object RunMode {
  case object Sanity extends RunMode
  case object Dev extends RunMode
  case object Test extends RunMode

  def fromString(s: String) = s match {
    case "sanity" => Some(Sanity)
    case "dev" => Some(Dev)
    case "test" => Some(Test)
    case _ => None
   }
}

sealed trait VerbSenseConfig {
  import VerbSenseConfig._
  def modelName = this match {
    case EntropyOnly => "entropy"
    case ELMoOnly => "elmo"
    case i @ Interpolated(entropyLambda) =>
      f"entropy$entropyLambda%.2f-elmo${i.elmoLambda}%.2f"
  }
}
object VerbSenseConfig {
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
  val inputElmoPrefix = qasrlElmoPath.resolve(if(mode.sanity) "dev" else "train").toString
  val evalElmoPrefix = qasrlElmoPath.resolve(if(mode.test) "test" else "dev").toString
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
  val modelsDir = configDir.map(_.resolve("models")).flatTap(createDir)
  val resultsDir = configDir.map(_.resolve("results")).flatTap(createDir)

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
        read = path => FileUtil.readJsonLines[(InflectedForms, String, Int)](evaluationItemsPath).compile.toVector,
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


  def verbClustersPath(verbSenseConfig: VerbSenseConfig) = {
    modelsDir.map(_.resolve(s"${verbSenseConfig.modelName}.jsonl.gz"))
  }

  def getCachedVerbClusters(verbSenseConfig: VerbSenseConfig): IO[Option[Map[InflectedForms, MergeTree[VerbId]]]] = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    // optionFromFile[Map[InflectedForms, MergeTree[VerbId]]](
    //   verbClustersPath,
    //   path => FileUtil.readJsonLines[(InflectedForms, MergeTree[VerbId])](path)
    //     .compile.toList.map(l => Some(l.toMap)),
    // )
    verbClustersPath(verbSenseConfig).flatMap(path =>
      fileCached[Option[Map[InflectedForms, MergeTree[VerbId]]]](
        path, read = path => FileUtil.readJsonLines[(InflectedForms, MergeTree[VerbId])](path)
          .compile.toList.map(l => Some(l.toMap)),
        write = (_, _) => IO.unit
      )(compute = IO(None))
    )
  }
  def cacheVerbClusters(verbSenseConfig: VerbSenseConfig, clusters: Map[InflectedForms, MergeTree[VerbId]]): IO[Unit] = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    verbClustersPath(verbSenseConfig).flatMap(path =>
      FileUtil.writeJsonLines(path)(clusters.toList)
    )
  }


  // TODO below

  // val inducedFramesetsPath = experimentDir.resolve(
  //   if(testOnTest) "results-test.jsonl.gz" else "results-dev.jsonl.gz"
  // )
  // def readFramesets(implicit cs: ContextShift[IO]) = logOp(
  //   "Reading framesets",
  //   FileUtil.readJsonLines[VerbFrameset](inducedFramesetsPath)
  //     .compile.toList
  //     .map(_.map(f => f.inflectedForms -> f).toMap)
  // )
  // def writeFramesets(
  //   framesets: Map[InflectedForms, VerbFrameset])(
  //   implicit cs: ContextShift[IO]
  // ) = logOp(
  //   "Writing framesets",
  //   FileUtil.writeJsonLines(inducedFramesetsPath, io.circe.Printer.noSpaces)(
  //     framesets.values.toList
  //   )
  // )
}
