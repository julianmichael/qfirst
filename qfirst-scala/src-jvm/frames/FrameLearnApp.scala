package qfirst.frames
import qfirst._
import qfirst.metrics._
import qfirst.probability._

import cats.Applicative
import cats.Id
import cats.Functor
import cats.Monoid
import cats.Monad

import cats.data.NonEmptyList
import cats.data.Writer
import cats.implicits._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files
import java.nio.file.Paths

import nlpdata.datasets.wiktionary.InflectedForms

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.VerbEntry
import qasrl.data.Sentence

import HasMetrics.ops._

object Logging {
  sealed trait Log[F[_]] extends (String => F[Unit]) {
    def apply(s: String): F[Unit]
    final def any(a: Any): F[Unit] = apply(a.toString)
  }
  object Log {
    val writer: Log[Writer[Vector[String], ?]] = new Log[Writer[Vector[String], ?]] {
      override def apply(s: String) = Writer.tell[Vector[String]](Vector(s))
    }
    val nop: Log[Id] = new Log[Id] {
      override def apply(s: String) = ()
    }
    val console: Log[IO] = new Log[IO] {
      override def apply(s: String) = IO { println(s) }
    }
  }
  def log[F[_]](s: String)(implicit l: Log[F]) = l(s)
}

object SimpleFrameInduction {

  import Logging._

  case class Instance(
    verbInflectedForms: InflectedForms,
    allFramesWithAnswer: Map[String, Set[(Frame, ArgumentSlot)]]
  )

  case class Model(
    verbCounts: Map[InflectedForms, Map[ArgStructure, Double]],
    globalCounts: Map[ArgStructure, Double]
  ) {
    val numGlobalCounts = globalCounts.values.sum
    def getVerbDistribution(verbInflectedForms: InflectedForms) = {
      CategoricalDistribution(
        verbCounts.get(verbInflectedForms).combineAll |+|
          globalCounts.mapValues(_ / numGlobalCounts)
      )
    }

    def chooseBestFrames(
      input: Instance
    ): Instance = {
      val verbDistribution = getVerbDistribution(input.verbInflectedForms)
      val argStructurePseudoCounts = input.allFramesWithAnswer.values.toList.foldMap { case qFramesWithAnswer =>
        val totalMass = qFramesWithAnswer.map(fa => verbDistribution.probability(fa._1.structure)).sum
        if(totalMass == 0.0) {
          qFramesWithAnswer.toList.foldMap(fa => Map(fa._1.structure -> (1.0 / qFramesWithAnswer.size)))
        } else  {
          qFramesWithAnswer.toList.map(_._1.structure).foldMap(s => Map(s -> (verbDistribution.probability(s) / totalMass)))
        }
      }
      val bestFramesWithAnswer = input.allFramesWithAnswer.map { case (q, qFramesWithAnswer) =>
        q -> qFramesWithAnswer.groupBy(fa => argStructurePseudoCounts(fa._1.structure)).toList.maxBy(_._1)._2
      }
      input.copy(allFramesWithAnswer = bestFramesWithAnswer)
    }

    def predictBestFrames(
      verb: VerbEntry
    ): Map[String, Set[(Frame, ArgumentSlot)]] = {
      val questions = verb.questionLabels.keys.toList
      val allFramesWithAnswer = questions.flatMap(q =>
        Model.getAllPossibleFramesWithAnswer(verb.verbInflectedForms, q).map(q -> _)
      ).toMap
      chooseBestFrames(Instance(verb.verbInflectedForms, allFramesWithAnswer)).allFramesWithAnswer
    }

    def emStep(instances: List[Instance]): Model = {
      val verbCounts = instances.foldMap { input =>
        val output = chooseBestFrames(input)
        val framePseudoCounts = output.allFramesWithAnswer.values.toList.foldMap { setOfFramesWithAnswer =>
          setOfFramesWithAnswer.toList.foldMap(fa => Map(fa._1.structure -> (1.0 / setOfFramesWithAnswer.size)))
        }
        Map(output.verbInflectedForms -> framePseudoCounts)
      }
      val globalCounts = verbCounts.values.toList.combineAll
      Model(verbCounts, globalCounts)
    }
  }
  object Model {
    def getAllPossibleFramesWithAnswer(
      verbInflectedForms: InflectedForms,
      question: String
    ): Option[Set[(Frame, ArgumentSlot)]] = {
      val questionTokensIsh = question.init.split(" ").toVector
      val stateMachine = new TemplateStateMachine(questionTokensIsh, verbInflectedForms)
      val template = new QuestionProcessor(stateMachine)
      val result = template.processStringFully(question).right.toOption.map { goodStates =>
        val completeStates = goodStates.map(s =>
          QuestionProcessor.ValidState.eitherIso.get(s).right.get
        )
        val framesWithAnswer = completeStates.map(s =>
          s.frame -> s.answerSlot
        ).toList.toSet
        framesWithAnswer
      }
      // if(result.isEmpty) { println(s"WAHH! $question") }
      result
    }

    def init(instances: List[Instance]): Model = {
      val allPossibleArgStructures = instances.foldMap(_.allFramesWithAnswer.values.toList.foldMap(_.map(_._1.structure)))
      Model(Map(), allPossibleArgStructures.map(_ -> 1.0).toMap)
    }

    implicit val modelFramePredictionModel = FramePredictionModel.make[Model](
      (model: Model, verb: VerbEntry) => {
        val results = model.predictBestFrames(verb)
        results.map {
          case (question, resultSet) =>
            // if(resultSet.size > 1) {
            //   println("\nWoo!")
            //   println(question)
            //   resultSet.foreach { case (frame, answerSlot) =>
            //     println(s"${frame.structure.args}: $answerSlot")
            //   }
            // }
            question -> resultSet.head
        }
      }
    )
  }

  // import scala.annotation.tailrec
  def runEMAux[F[_]: Applicative](
    doSomethingWithModel: Model => F[Unit])(
    model: Model, data: List[Instance], stepsRemaining: Int
  ): F[Model] = {
    doSomethingWithModel(model).productR {
      if(stepsRemaining <= 0) Applicative[F].pure(model)
      else runEMAux(doSomethingWithModel)(model.emStep(data), data, stepsRemaining - 1)
    }
  }

  def runEM[F[_]: Applicative](
    doSomethingWithModel: Model => F[Unit])(
    data: List[Instance], steps: Int
  ) = runEMAux(doSomethingWithModel)(Model.init(data), data, steps)

  import shapeless._
  import shapeless.syntax.singleton._
  import shapeless.record._

  def computeMetrics(model: Model) = (i: Instance) => {
    import qfirst.metrics.{Transformers => M}
    val output = model.chooseBestFrames(i)
    "frames per question" ->> output.allFramesWithAnswer.toList.map(_._2.size).foldMap(Counts(_)) ::
      "questions per verb instance" ->> Counts(output.allFramesWithAnswer.size) ::
      "frames per verb instance" ->> Counts(output.allFramesWithAnswer.toList.foldMap(_._2.map(_._1)).size) ::
      "arg structures per verb instance" ->> Counts(output.allFramesWithAnswer.toList.foldMap(_._2.map(_._1.structure)).size) ::
      "arg structures (no animacy) per verb instance" ->> Counts(output.allFramesWithAnswer.toList.foldMap(_._2.map(_._1.structure.forgetAnimacy)).size) ::
      "frames" ->> Count(output.allFramesWithAnswer.values.toList.foldMap(_.toVector.map(p => p._1.structure -> p._1.tan))) ::
      "argument structures" ->> Count(output.allFramesWithAnswer.values.toList.foldMap(_.toVector.map(_._1.structure))) ::
      "argument structures (no animacy)" ->> Count(output.allFramesWithAnswer.values.toList.foldMap(_.toVector.map(_._1.structure.forgetAnimacy))) ::
      "frames with answer slot" ->> Count(output.allFramesWithAnswer.values.toList.foldMap(_.toVector.map(p => p._1.structure -> p._1.tan -> p._2))) ::
      "argument structures with answer slot" ->> Count(output.allFramesWithAnswer.values.toList.foldMap(_.toVector.map(p => p._1.structure -> p._2))) ::
      HNil
  }

  def evaluate(model: Model, instances: List[Instance]) = {
    instances.foldMap(computeMetrics(model))
  }

  def getNiceVerbEntryString(verb: VerbEntry) = {
    verb.verbInflectedForms.toString + "\n" +
      verb.questionLabels.map { case (qString, qLabel) =>
        val numJudgments = qLabel.answerJudgments.size
        val propValid = qLabel.answerJudgments.filter(_.judgment.isAnswer).size.toDouble / numJudgments
        f"\t$propValid%5.2f ($numJudgments) $qString%s"
      }.mkString("\n") + "\n"
  }

  def getInstance(verb: VerbEntry): Instance = {
    val structuresForQuestions = qfirst.filterGoldNonDense(verb)._2.toList
      .map(_._1).flatMap(question =>
        Model.getAllPossibleFramesWithAnswer(verb.verbInflectedForms, question).map(question -> _)
      ).toMap
    Instance(
      verb.verbInflectedForms,
      structuresForQuestions
    )
  }

  def getInstances(data: Dataset): List[Instance] = {
    Dataset.verbEntries.getAll(data).map(getInstance)
  }

  def run[F[_]: Monad : Log](
    trainData: Dataset, devData: Dataset
  ): F[Model] = {
    val train = getInstances(trainData)
    val dev = getInstances(devData)
    for {
      model <- runEM(m => log(evaluate(m, train).get("frames per question").histogramString(100) + "\n"))(train, 1)
      trainResults = evaluate(model, train)
      _ <- log("=== TRAIN RESULTS ===")
      _ <- log("questions per verb instance:\n" + trainResults.get("questions per verb instance").histogramString(100) + "\n")
      _ <- log("arg structures per verb instance:\n" + trainResults.get("arg structures per verb instance").histogramString(100) + "\n")
      _ <- log(getMetricsString(trainResults))
      devResults = evaluate(model, dev)
      _ <- log("=== DEV RESULTS ===")
      _ <- log("questions per verb instance:\n" + devResults.get("questions per verb instance").histogramString(100) + "\n")
      _ <- log("arg structures per verb instance:\n" + devResults.get("arg structures per verb instance").histogramString(100) + "\n")
      _ <- log(getMetricsString(devResults))
    } yield model
  }
}

object FrameLearnApp extends IOApp {

  import Logging._

  def printFrameStats[M: FramePredictionModel](
    data: Dataset,
    model: M
  ): IO[Unit] = {
    import FrameDataWriter._
    import FramePredictionModel.ops._

    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._

    lazy val stats = data.sentences.toList.foldMap { case (sentenceId, sentence) =>
      sentence.verbEntries.toList.foldMap { case (verbIndex, verb) =>
        val predictions = model.predictFramesWithAnswers(verb)
        val clauseSet = predictions.map(_._2._1.args.keys.toSet).toSet
        val qslotSet = predictions.map(_._2._2).toSet
        "abstracted clauses" ->> Counts(clauseSet.size) ::
          "question answer slots" ->> Counts(qslotSet.size) :: HNil
      }
    }

    for {
      // _ <- IO(println("Clause stats: " + qfirst.SandboxApp.getMetricsString(stats)))
      _ <- IO(println("Clause hist:\n" + stats("abstracted clauses").histogramString(75)))
      _ <- IO(println("Qslot hist:\n" + stats("question answer slots").histogramString(75)))
    } yield ()
  }

  def writeFrameData[M: FramePredictionModel](
    data: Dataset,
    model: M,
    outPath: NIOPath
  ): IO[Unit] = {
    import FrameDataWriter._
    import FramePredictionModel.ops._
    import io.circe.syntax._

    val printer = io.circe.Printer.noSpaces

    def jsonItemsIter = for {
      (sentenceId, sentence) <- data.sentences.iterator
      (verbIndex, verb) <- sentence.verbEntries.iterator
      (questionString, (frame, answerSlot)) <- model.predictFramesWithAnswers(verb)
    } yield {
      val info = FrameInfo(sentenceId, verbIndex, questionString, frame, answerSlot)
      printer.pretty(info.asJson)
    }

    val fileString = jsonItemsIter.mkString("\n")

    IO(Files.write(outPath, fileString.getBytes("UTF-8")))
  }

  def program(qasrlBankPath: NIOPath, outPath: NIOPath): IO[ExitCode] = {
    implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
    implicit val _log = Log.console
    for {
      train <- IO(Data.readDataset(qasrlBankPath.resolve("expanded").resolve("train.jsonl.gz")))
      dev <- IO(Data.readDataset(qasrlBankPath.resolve("expanded").resolve("dev.jsonl.gz")))
      devDense <- IO(Data.readDataset(qasrlBankPath.resolve("dense").resolve("dev.jsonl.gz")))
      model <- SimpleFrameInduction.run[IO](train, dev)
      // _ <- writeFrameData(train |+| dev |+| devDense, model, outPath)
      _ <- printFrameStats(train |+| dev |+| devDense, model)
    } yield ExitCode.Success
  }

  val runFrameLearn = Command(
    name = "mill qfirst.runClauseLearn",
    header = "Learn the mapping from QA-SRL questions to clauses."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val outPath = Opts.option[NIOPath](
      "out", metavar = "path", help = "Path where to write the output file."
    )

    (goldPath, outPath).mapN(program)
  }

   def run(args: List[String]): IO[ExitCode] = {
    runFrameLearn.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(run) => run
    }
  }
}
