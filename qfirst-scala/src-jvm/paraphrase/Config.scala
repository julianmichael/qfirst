package qfirst.paraphrase
import qfirst.FileUtil

import java.nio.file._

import qasrl.bank.Data
import qasrl.bank.FullData

import nlpdata.datasets.wiktionary.InflectedForms

import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._

import EvalApp.ParaphraseAnnotations

case class Config(
  experimentName: String,
  trainOnDev: Boolean,
  testOnTest: Boolean
) {
  import Config.outputDir
  import Config.qasrlBankPath
  import Config.qasrlElmoPath
  val qaInputPath = Config.getQAInputPath(outputDir)
  val qaOutputPath = Config.getQAOutputPath(outputDir)

  def readWholeQasrlBank = logOp(
    "Reading QA-SRL Bank",
    Data.readFromQasrlBank(qasrlBankPath).toEither.right.get
  )
  def readQasrlDataset(name: String) = logOp(
    s"Reading QA-SRL dataset $name",
    readDataset(qasrlBankPath.resolve(name + ".jsonl.gz"))
  )
  def getInputSet(data: FullData) = {
    if(trainOnDev) data.devExpanded else data.trainExpanded
  }
  def readInputSet = readQasrlDataset(
    if(trainOnDev) "expanded/dev" else "expanded/train"
  )
  def getEvalSet(data: FullData) = {
    if(testOnTest) data.testOrig else data.devExpanded
  }
  def readEvalSet = readQasrlDataset(
    if(testOnTest) "orig/test" else "expanded/dev"
  )

  def inputElmoPrefix = qasrlElmoPath.resolve(
    if(trainOnDev) "dev" else "train"
  ).toString
  def evalElmoPrefix = qasrlElmoPath.resolve(
    if(testOnTest) "test" else "dev"
  ).toString

  val evaluationItemsPath = outputDir.resolve(
    if(testOnTest) "eval-sample-test.jsonl" else "eval-sample-dev.jsonl"
  )
  def getEvaluationItems(implicit cs: ContextShift[IO]) = {
    import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
    if(Files.exists(evaluationItemsPath)) FileUtil.readJsonLines[(InflectedForms, String, Int)](evaluationItemsPath).compile.toList
    else logOp(
      s"Creating new sample for evaluation at $evaluationItemsPath", readEvalSet.flatMap { evalSet =>
        val rand = new scala.util.Random(86735932569L)
        val allItems = rand.shuffle(
          filterDatasetNonDense(evalSet).sentences.values.iterator.flatMap(sentence =>
            sentence.verbEntries.values.toList.map(verb =>
              (verb.verbInflectedForms, sentence.sentenceId, verb.verbIndex)
            )
          )
        ).take(1000).toVector
        FileUtil.writeJsonLines(evaluationItemsPath, io.circe.Printer.noSpaces)(allItems).as(allItems)
      }
    )
  }

  val paraphraseGoldPath = outputDir.resolve("gold-paraphrases.json")
  def readGoldParaphrases(implicit cs: ContextShift[IO]) = {
    if(!Files.exists(paraphraseGoldPath)) {
      IO(println("No gold paraphrase annotations found at the given path. Initializing to empty annotations.")) >>
        IO.pure(Map.empty[String, Map[Int, VerbParaphraseLabels]])
    } else FileUtil.readJson[ParaphraseAnnotations](paraphraseGoldPath)
  }
  def saveGoldParaphrases(data: ParaphraseAnnotations)(implicit cs: ContextShift[IO]) = {
    FileUtil.writeJson(paraphraseGoldPath, io.circe.Printer.noSpaces)(data)
  }

  val experimentDir = outputDir.resolve(experimentName)

  val inducedFramesetsPath = experimentDir.resolve(
    if(testOnTest) "results-test.jsonl.gz" else "results-dev.jsonl.gz"
  )
  def readFramesets(implicit cs: ContextShift[IO]) = logOp(
    "Reading framesets",
    FileUtil.readJsonLines[VerbFrameset](inducedFramesetsPath)
      .compile.toList
      .map(_.map(f => f.inflectedForms -> f).toMap)
  )
  def writeFramesets(
    framesets: Map[InflectedForms, VerbFrameset])(
    implicit cs: ContextShift[IO]
  ) = logOp(
    "Writing framesets",
    FileUtil.writeJsonLines(inducedFramesetsPath, io.circe.Printer.noSpaces)(
      framesets.values.toList
    )
  )


}
object Config {
  val outputDir = Paths.get("frame-induction")
  val qasrlBankPath = Paths.get("qasrl-v2_1")
  val qasrlElmoPath = Paths.get("qasrl-v2-elmo")
  def getQAInputPath(outputDir: Path) = outputDir.resolve("qa-input.jsonl.gz")
  def getQAOutputPath(outputDir: Path) = outputDir.resolve("qa-output.jsonl.gz")

  def make(
    experimentName: String,
    trainOnDevOpt: Option[Boolean],
    testOnTest: Boolean)(
    implicit cs: ContextShift[IO]
  ): IO[Config] = {
    val experimentDir = outputDir.resolve(experimentName)
    for {
      trainOnDev <- IO {
        trainOnDevOpt match {
          case None if !Files.exists(experimentDir) =>
            throw new RuntimeException(s"Experiment directory not found: $experimentDir")
          case Some(trainOnDevFlag) if Files.exists(experimentDir) =>
            val trainOnDevSource = Files.exists(experimentDir.resolve("dev"))
            if(trainOnDevFlag != trainOnDevSource) {
              val modeString = if(trainOnDevSource) "dev" else "train"
              throw new RuntimeException(s"Must use $modeString configuration for experiment: $experimentDir")
            } else trainOnDevFlag // doesn't matter which bc equal
          case Some(trainOnDevFlag) if !Files.exists(experimentDir) =>
            println(s"Creating experiment directory $experimentDir")
            Files.createDirectories(experimentDir)
            if(trainOnDevFlag) {
              import sys.process._
              IO(s"touch ${experimentDir.resolve("dev")}".!)
            }
            trainOnDevFlag
        }
      }
      _ <- {
        if(trainOnDev && testOnTest) {
          throw new RuntimeException("Cannot train on dev and test on test")
        } else IO.unit
      }
    } yield Config(experimentName, trainOnDev, testOnTest)
  }
}
