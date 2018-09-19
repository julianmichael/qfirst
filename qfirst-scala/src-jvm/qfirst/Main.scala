package qfirst
import qfirst.metrics._

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.VerbEntry
import qasrl.bank.Data

import HasMetrics.ops._

object Main {

  def filterGold(minNumAnswers: Int, maxNumInvalid: Int) = (verb: VerbEntry) => {
    val (invalids, valids) = verb.questionLabels.toList.flatMap {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        val numAnswers = judgments.size
        if(numAnswers >= minNumAnswers) {
          if(numInvalid <= maxNumInvalid) Some(Right(questionString -> qLabel))
          else Some(Left(questionString -> qLabel))
        } else None
    }.separate
    invalids.toMap -> valids.toMap
  }

  val filterGoldNonDense = filterGold(3, 0)
  val filterGoldDense = filterGold(6, 1)

  import Instances.Bucketers

  val questionBucketers = Map(
    "wh" -> Bucketers.wh,
    "prep" -> Bucketers.prep
  )

  val templateBucketers = Map(
    "wh" -> Bucketers.Templated.wh,
    "prep" -> Bucketers.Templated.prep
  )

  val alignedSpanBucketers = Map(
    "gold-dep-len" -> Bucketers.goldDepLength(NonEmptyList.of(1, 2, 3, 5, 8, 12, 18, 27)),
    // "pred-dep-len" -> Bucketers.predDepLength(NonEmptyList.of(1, 2, 3, 5, 8, 12, 18, 27))
  )

  val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
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
    // (0 to 9).map(_ / 1000.0).toList ++
  val allQFirstFilters = for {
    qThresh <- (0 to 26 by 2).map(_ / 100.0).toList
    sThresh <- (10 to 90 by 10).map(_ / 100.0)
    iThresh <- (0 to 100 by 10).map(_ / 100.0)
    remBelowInv <- List(false, true)
  } yield BeamFilter(qThresh, sThresh, iThresh, remBelowInv)

  val allAFirstFilters = (0 to 100 by 5)
    .map(_ / 100.0)
    .toList.map(spanThreshold =>
    BeamFilter(
      questionThreshold = 0.1,
      spanThreshold = spanThreshold,
      invalidThreshold = 0.9,
      shouldRemoveSpansBelowInvalidProb = false)
  )

  val bestAFirstFilter = BeamFilter(0.10, 0.6, 0.9, false)
  val bestQFirstFilter = BeamFilter(0.10, 0.5, 0.9, true)
  val qFirstRecall2Filter = BeamFilter(0.05, 0.2, 0.5, false)

  // TODO make this choice a command-line argument?
  val allFilters = allAFirstFilters
  // val allFilters = List(qFirstRecall2Filter)

  def nullBucketer[I] = Map.empty[String, I => String]

  def verbBucketers(verbFreq: (LowerCaseString => Int)) = Map(
    "verb-freq" -> Bucketers.verbFreq(
      verbFreq,
      NonEmptyList.of(0, 10, 50, 150, 250, 500, 750, 1000))
  )

  def renderQuestionExample(question: Instances.QuestionInstance, renderInvalidGold: Boolean = false): String = {
    val qas = question.qas
    val verb = qas.verb
    val sid = verb.sentence.gold.sentenceId
    val sentenceTokens = verb.sentence.gold.sentenceTokens
    val verbString = verb.gold.verbInflectedForms.stem.toString + " (" + verb.gold.verbIndex + ")"
    val allQAStrings = qas.allQuestionStrings.toList
      .filter(qString => renderInvalidGold || qas.goldValid.contains(qString) || qas.pred.contains(qString))
      .sortBy { qString =>
      if(qas.goldValid.contains(qString) && qas.pred.contains(qString)) -1
      else if(qas.goldValid.contains(qString)) 0
      else if(qas.goldInvalid.contains(qString) && qas.pred.contains(qString)) 1
      else if(qas.pred.contains(qString)) 2
      else 3 }
      .map { qString =>
      val isGoldInvalid = qas.goldInvalid.contains(qString)
      val renderedQ = (if(qString == question.string) "*" else "") + (if(isGoldInvalid) "#" else "") + qString
      val goldString = qas.goldValid.get(qString).orElse(
        qas.goldInvalid.get(qString).filter(_ => renderInvalidGold)
      ).fold("\t\t") { qLabel =>
        val spans = qLabel.answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet[AnswerSpan].toList.sortBy(_.begin)
        val renderedSpans = spans.map(s =>
          Text.renderSpan(sentenceTokens, (s.begin until s.end).toSet)
        ).mkString(" / ")
        renderedQ + "\t" + renderedSpans + "\t"
      }
      val predString = qas.pred.get(qString).fold("\t") { case (_, spans) =>
        val renderedSpans = spans.map(s =>
          Text.renderSpan(sentenceTokens, (s.begin until s.end).toSet)
        ).mkString(" / ")
        renderedQ + "\t" + renderedSpans
      }
      goldString + predString
    }
    "\t" + Text.render(sentenceTokens) + "\n" +
      "\t" + verbString + "\n" +
      allQAStrings.map("\t" + _).mkString("\n")
  }

  def runDenseMetrics(
    getVerbFrequency: => (LowerCaseString => Int),
    gold: Dataset,
    pred: Map[String, SentencePrediction]
  ) = {
    import qfirst.{Instances => I}
    import qfirst.metrics.{Transformers => M}
    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._
    import monocle.function.{all => Optics}

    val computeMetrics = M.split(I.sentenceToVerbs) {
      M.hchoose(
        "num verbs" ->> ((vi: I.VerbInstance) => 1),
        "predictions" ->> M.choose(allFilters) { filterPred =>
          I.verbToQASet(filterGoldDense, filterPred) andThen
          M.hchoose(
            "spans" ->> (
              I.qaSetToSpanSet andThen
                I.getSpanSetConf
            ),
            "full questions" ->> M.split(I.qaSetToQuestions) {
              M.bucket(questionBucketers) {
                M.hchoose(
                  "question" ->> I.getQuestionBoundedAcc,
                  "question with answer" ->> I.getQuestionWithAnswerBoundedAcc,
                  "answer span" ->> M.split(I.questionToQAs) {
                    I.getQABoundedAcc
                  }
                )
              }
            },
            "templated questions" ->> (
              I.qaSetToQATemplateSet andThen
                M.split(I.qaTemplateSetToQuestionTemplates) {
                  // M.bucket(templateBucketers) {
                  M.hchoose(
                    "question" ->> I.getQuestionTemplateAcc,
                    "question with answer" ->> I.getQuestionTemplateWithAnswerAcc,
                    "answer span" ->> M.split(I.questionTemplateToQATemplates) {
                      I.getQATemplateAcc
                    }
                  )
                  // }
                }
            )
          )
        }
      )
    }

    val raw = Instances.foldMapInstances(gold, pred)(computeMetrics)

    // postprocess

    // compute number of questions and spans per verb
    val results = raw.updateWith("predictions")(
      _.map { stats => // stats for given filter
        val questionsPerVerb = "questions per verb" ->> stats.get("full questions").collapsed.get("question").boundedAcc.predicted.toDouble / raw("num verbs")
        val spansPerVerb = "spans per verb" ->> stats.get("full questions").collapsed.get("answer span").predicted.toDouble / raw("num verbs")
        stats + questionsPerVerb + spansPerVerb
      }
    )

    val questionRecallThresholds = List(0.1, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
    // val spanRecallThresholds

    // filter by recall thresholds, take max (lb) performing setting under each threshold
    val questionTunedResults = results.updateWith("predictions")(
      filters => Chosen(
        questionRecallThresholds.map(thresh =>
          thresh -> filters.filter(_.get("questions per verb") >= thresh)
            .keepMaxBy(_.get("full questions").collapsed.get("question with answer").accuracyLowerBound)
        ).filter(_._2.nonEmpty).toMap
      )
    )

    val spanRecallThresholds = List(0.1, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)

    val spanTunedResults = results.updateWith("predictions")(
      filters => Chosen(
        spanRecallThresholds.map(thresh =>
          thresh -> filters.filter(_.get("spans per verb") >= thresh)
            .keepMaxBy(_.get("full questions").collapsed.get("answer span").accuracyLowerBound)
        ).filter(_._2.nonEmpty).toMap
      )
    )

    val prep = questionTunedResults.get("predictions").data(2.0).data.head._2
      .get("full questions")
      .collapseBuckets("wh")
      .map(_.get("question"))
      .filter { bucket => bucket.boundedAcc.predicted >= 50 }

    val rand = new util.Random(28127L)
    val prepExamples = prep.data.map { case (buckets, bacc) =>
      val correct = rand.shuffle(bacc.correct).take(5)
      val correctPct = bacc.boundedAcc.correct.toDouble / bacc.boundedAcc.predicted * 100
      val incorrect = rand.shuffle(bacc.incorrect).take(5)
      val incorrectPct = bacc.boundedAcc.incorrect.toDouble / bacc.boundedAcc.predicted * 100
      val uncertain = rand.shuffle(bacc.uncertain).take(5)
      val uncertainPct = bacc.boundedAcc.uncertain.toDouble / bacc.boundedAcc.predicted * 100

      def renderExes(name: String, group: Vector[Instances.QuestionInstance], pct: Double) = {
        f"$name%s: $pct%.1f%%\n" +
          group.map(q => renderQuestionExample(q, true)).mkString("\n\n") + "\n"
      }

      buckets("prep") + ":\n" +
        renderExes("Correct", correct, correctPct) +
        renderExes("Incorrect", incorrect, incorrectPct) +
        renderExes("Uncertain", uncertain, uncertainPct)
    }.mkString("\n")

    println(questionTunedResults.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(spanTunedResults.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(prepExamples)

    {
      import ammonite.ops._
      write.over(pwd / "prep-errors-dense.tsv", prepExamples)
    }
  }

  def runNonDenseMetrics(
    getVerbFrequency: => (LowerCaseString => Int),
    gold: Dataset,
    pred: Map[String, SentencePrediction]
  ) = {
    import qfirst.{Instances => I}
    import qfirst.metrics.{Transformers => M}

    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._

    val computeMetrics = M.split(I.sentenceToVerbs) {
      // M.bucket(verbBucketers(getVerbFrequency)) {
      M.choose(allFilters) { filterPred =>
        I.verbToQASet(filterGoldNonDense, filterPred) andThen
        M.hchoose(
          "spans" ->> (
            I.qaSetToSpanSet andThen
              I.getSpanSetConf
          ),
          "full questions" ->> (
            M.split(I.qaSetToQuestions) {
              M.bucket(questionBucketers) {
                M.hchoose(
                  "question" ->> I.getQuestionConf,
                  "question with answer" ->> I.getQuestionWithAnswerConf
                )
              }
            }
          ),
          "templated questions" ->> (
            I.qaSetToQATemplateSet andThen
              M.split(I.qaTemplateSetToQuestionTemplates) {
                // M.bucket(templateBucketers) {
                M.hchoose(
                  "question" ->> I.getQuestionTemplateConf,
                  "question with answer" ->> I.getQuestionTemplateWithAnswerConf
                )
                // }
              }
          )
        )
      }
      // }
    }

    val raw = Instances.foldMapInstances(gold, pred)(computeMetrics)

    val best = raw.keepMaxBy(_.get("full questions").collapsed.get("question with answer").stats.f1)

    val bestCollapsed = best.map(_.updateWith("full questions")(_.collapsed))

    val prep = best.data.head._2.get("full questions")
      .collapseBuckets("wh")
      .map(_.get("question"))
      .filter { bucket =>
      val enoughGold = bucket.stats.numGold >= 50
      val enoughPred = bucket.stats.numPredicted >= 50
      enoughGold || enoughPred
    }

    val rand = new util.Random(28127L)
    val prepExamples = prep.data.map { case (buckets, conf) =>
      val tp = rand.shuffle(conf.tp).take(5)
      val tpPct = conf.stats.tp.toDouble / conf.stats.numPositiveInstances * 100
      val fp = rand.shuffle(conf.fp).take(5)
      val fpPct = conf.stats.fp.toDouble / conf.stats.numPositiveInstances * 100
      val fn = rand.shuffle(conf.fn).take(5)
      val fnPct = conf.stats.fn.toDouble / conf.stats.numPositiveInstances * 100

      def renderExes(name: String, group: Vector[Instances.QuestionInstance], pct: Double) = {
        f"$name%s: $pct%.1f%%\n" +
          group.map(q => renderQuestionExample(q)).mkString("\n\n") + "\n"
      }

      buckets("prep") + ":\n" +
        renderExes("True positives", tp, tpPct) +
        renderExes("False positives", fp, fpPct) +
        renderExes("False negatives", fn, fnPct)
    }.mkString("\n")

    // println(raw.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(prep.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(bestCollapsed.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(prepExamples)

    {
      import ammonite.ops._
      write.over(pwd / "prep-errors.tsv", prepExamples)
    }
  }

  def readPredictions(path: NIOPath) = {
    import ammonite.ops._
    import io.circe.jawn
    read.lines(Path(path, pwd)).toList
      .traverse(jawn.decode[SentencePrediction])
      .map(_.map(pred => pred.sentenceId -> pred).toMap)
  }

  def readVerbFrequencies(trainFile: NIOPath) = {
    val data = Data.readDataset(trainFile)
    data.sentences.iterator
      .flatMap(s => s._2.verbEntries.values.map(_.verbInflectedForms.stem).iterator)
      .foldLeft(Map.empty[LowerCaseString, Int].withDefaultValue(0)) {
      (counts, stem) => counts + (stem -> (counts(stem) + 1))
    }
  }

  def program(qasrlBankPath: NIOPath, predFile: NIOPath, dense: Boolean) = {
    lazy val verbFrequencies = readVerbFrequencies(qasrlBankPath.resolve("orig").resolve("train.jsonl.gz"))
    val gold = if(dense) {
      Data.readDataset(qasrlBankPath.resolve("dense").resolve("dev.jsonl.gz"))
    } else Data.readDataset(qasrlBankPath.resolve("orig").resolve("dev.jsonl.gz"))
    val predEither = readPredictions(predFile)
    predEither match {
      case Left(error) => System.err.println(error)
      case Right(pred) =>
        if(dense) runDenseMetrics(verbFrequencies, gold, pred)
        else runNonDenseMetrics(verbFrequencies, gold, pred)
    }
  }

  val runMetrics = Command(
    name = "mill qfirst.run",
    header = "Calculate metrics."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val predPath = Opts.option[NIOPath](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val dense = Opts.flag(
      "dense", help = "Whether to run the dense eval."
    ).orFalse

    (goldPath, predPath, dense).mapN(program)
  }

  def main(args: Array[String]): Unit = {
    val result = runMetrics.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(main) => IO { main }
    }
    result.unsafeRunSync
  }
}
