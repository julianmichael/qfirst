package qfirst.learn
import qfirst._
import qfirst.metrics._
import qfirst.probability._

import cats.Id
import cats.Functor
import cats.Monoid

import cats.data.NonEmptyList
import cats.data.Writer
import cats.implicits._
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

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

object LearnApp {

  val genericInflectedForms = InflectedForms(
    stem = "stem".lowerCase,
    present = "present".lowerCase,
    presentParticiple = "presentParticiple".lowerCase,
    past = "past".lowerCase,
    pastParticiple = "pastParticiple".lowerCase
  )

  val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc
    )
  }
  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  def evalBucketBounds(unsortedBounds: NonEmptyList[Int])(value: Int) = {
    val bounds = unsortedBounds.sorted
    if(value <= bounds.head) s"<=${bounds.head}"
    else bounds.toList.sliding(2).find(g => value > g(0) && value <= g(1)) match {
      case Some(g) => s"${g(0) + 1}-${g(1)}"
      case None => s">${bounds.last}"
    }
  }

  def verbFreq(getFreq: InflectedForms => Int, bounds: NonEmptyList[Int]) = (verb: VerbEntry) => {
    val freq = getFreq(verb.verbInflectedForms)
    evalBucketBounds(bounds)(freq)
  }

  def verbBucketers(getFreq: (InflectedForms => Int)) = Map(
    "verb-freq" -> verbFreq(
      getFreq,
      NonEmptyList.of(0, 10, 50, 150, 250, 500, 750, 1000))
  )

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
  import Logging._

  object SimpleQuestionDistributions {

    case class Model(
      verbDistributions: Map[InflectedForms, FunctionalDistribution[String]],
      verbCounts: Map[InflectedForms, Int],
      fallback: FunctionalDistribution[String]
    ) {
      def seenVerbs = verbDistributions.keySet
      def get(verb: InflectedForms) = verbDistributions.getOrElse(verb, fallback)
    }

    def induce(data: Dataset): Model = {
      val questionCountsForVerbs = Dataset.verbEntries.getAll(data)
        .groupBy(_.verbInflectedForms)
        .map { case (verbForms, verbEntries) =>
          val (invalidLabelMaps, validLabelMaps) = verbEntries.map(qfirst.filterGoldNonDense).unzip
          val questionCounts = counts(
            validLabelMaps.flatMap { valids =>
              valids.values.map(_.questionSlots.renderQuestionString(genericInflectedForms))
            }
          )
          verbForms -> questionCounts
      }
      val totalDist = FunctionalDistribution.interpolate(
        200.0 -> new FunctionalDistribution(
          CategoricalDistribution(questionCountsForVerbs.toList.foldMap(_._2)).probability
        ),
        20.0 -> new FunctionalDistribution((x: String) => 1.0 / 46000000)
      )
      Model(
        questionCountsForVerbs.map { case (v, qc) =>
          val sum = qc.values.sum
          v -> FunctionalDistribution.interpolate(
            sum.toDouble -> new FunctionalDistribution(CategoricalDistribution(qc).probability),
            200.0 -> totalDist
          )
        },
        questionCountsForVerbs.map { case (v, qc) =>
          v -> qc.values.sum
        },
        totalDist
      )
    }

    def computeMetrics(model: Model) = (s: Sentence) => {
      import qfirst.metrics.{Transformers => M}
      s.verbEntries.values.toList.foldMap {
        /* M.bucket(verbBucketers(model.verbCounts.withDefaultValue(0))) */ { verb =>
          val validQuestions = qfirst.filterGoldNonDense(verb)._2
          validQuestions.values
            .map(_.questionSlots.renderQuestionString(genericInflectedForms)).toList
            .foldMap { questionString =>
            val likelihood = model.get(verb.verbInflectedForms).probability(questionString)
            if(likelihood == 0.0) {
              println(s"verb: ${verb.verbInflectedForms}")
              println(s"question: $questionString")
            }
            Perplexity(-math.log(likelihood))
          }
        }
      }
    }

    def evaluate(model: Model, data: Dataset) = {
      data.sentences.values.toList.foldMap(computeMetrics(model))
    }

    def run[F[_]: Functor : Log](
      train: Dataset, dev: Dataset
    ): F[Model] = {
      val model = induce(train)
      val metrics = evaluate(model, dev)
      log(getMetricsString(metrics)).as(model)
    }
  }

  def getTemplate(ql: qasrl.data.QuestionLabel) =
    TemplateSlots.fromQuestionSlots(ql.questionSlots).toTemplateString

  object SimpleQuestionTemplateDistributions {

    case class Model(
      verbDistributions: Map[InflectedForms, FunctionalDistribution[String]],
      verbCounts: Map[InflectedForms, Int],
      fallback: FunctionalDistribution[String]
    ) {
      def seenVerbs = verbDistributions.keySet
      def get(verb: InflectedForms) = verbDistributions.getOrElse(verb, fallback)
    }

    def induce(data: Dataset): Model = {
      val questionCountsForVerbs = Dataset.verbEntries.getAll(data)
        .groupBy(_.verbInflectedForms)
        .map { case (verbForms, verbEntries) =>
          val (invalidLabelMaps, validLabelMaps) = verbEntries.map(qfirst.filterGoldNonDense).unzip
          val questionCounts = counts(
            validLabelMaps.flatMap { valids =>
              valids.values.map(getTemplate)
            }
          )
          verbForms -> questionCounts
      }
      val totalDist = FunctionalDistribution.interpolate(
        10.0 -> new FunctionalDistribution(
          CategoricalDistribution(questionCountsForVerbs.toList.foldMap(_._2)).probability
        ),
        1.0 -> new FunctionalDistribution((x: String) => 1.0 / 218000)
      )
      Model(
        questionCountsForVerbs.map { case (v, qc) =>
          val sum = qc.values.sum
          v -> FunctionalDistribution.interpolate(
            sum.toDouble -> new FunctionalDistribution(CategoricalDistribution(qc).probability),
            44.0 -> totalDist
          )
        },
        questionCountsForVerbs.map { case (v, qc) =>
          v -> qc.values.sum
        },
        totalDist
      )
    }

    def computeMetrics(model: Model) = (s: Sentence) => {
      import qfirst.metrics.{Transformers => M}
      s.verbEntries.values.toList.foldMap {
        M.bucket(verbBucketers(model.verbCounts.withDefaultValue(0))) { verb =>
          val validQuestions = qfirst.filterGoldNonDense(verb)._2
          validQuestions.values
            .map(getTemplate).toList
            .foldMap { questionString =>
            val likelihood = model.get(verb.verbInflectedForms).probability(questionString)
            Perplexity(-math.log(likelihood))
          }
        }
      }
    }

    def evaluate(model: Model, data: Dataset) = {
      data.sentences.values.toList.foldMap(computeMetrics(model))
    }

    def run[F[_]: Functor : Log](
      train: Dataset, dev: Dataset
    ): F[Model] = {
      val model = induce(train)
      val metrics = evaluate(model, dev)
      log(getMetricsString(metrics)).as(model)
    }
  }

  def getGeneralStats[F[_]](data: Dataset) = {
    import qfirst.metrics.{Transformers => M}
    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._

    // TODO switch to record
    Dataset.verbEntries.getAll(data)
      .groupBy(_.verbInflectedForms).toList
      .foldMap { case (verbForms, verbEntries) =>
        val (invalidLabelMaps, validLabelMaps) = verbEntries.map(qfirst.filterGoldNonDense).unzip
        Map(
          "verbs" -> Counts(verbEntries.size),
          "question strings" -> Counts(validLabelMaps.map(_.keySet).reduce(_ union _).size),
          "question templates" -> Counts(
            validLabelMaps.map(_.values.map(getTemplate).toSet).reduce(_ union _).size),
          "question sets" -> validLabelMaps.foldMap(valids => Counts(valids.keySet.size)),
        )
    } + (
      "global question strings" ->
        counts(
          Dataset.questionLabels.getAll(data).map(_.questionSlots.renderQuestionString(genericInflectedForms))
        ).values.toList.foldMap(Counts(_))
    ) + (
      "global question templates" ->
        counts(
          Dataset.questionLabels.getAll(data).map(getTemplate)
        ).values.toList.foldMap(Counts(_))
    )
  }


  def program(qasrlBankPath: NIOPath): IO[Unit] = {
    // TODO validate using opts stuff from decline?
    val train = Data.readDataset(qasrlBankPath.resolve("orig").resolve("train.jsonl.gz"))
    val dev = Data.readDataset(qasrlBankPath.resolve("orig").resolve("dev.jsonl.gz"))
    implicit val _log = Log.console

    IO.unit >>
      log(getMetricsString(getGeneralStats(train))) >>
      // SimpleQuestionDistributions.run[IO](train, dev) >>
      // SimpleQuestionTemplateDistributions.run[IO](train, dev) >>
      IO.unit
  }

  val runLearn = Command(
    name = "mill qfirst.runLearn",
    header = "Do simple structure learning on QA-SRL."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )

    (goldPath).map(program)
  }

  def main(args: Array[String]): Unit = {
    val result = runLearn.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(run) => run
    }
    result.unsafeRunSync
  }
}
