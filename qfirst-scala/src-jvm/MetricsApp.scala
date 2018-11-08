package qfirst
import qfirst.metrics._

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.VerbEntry

import HasMetrics.ops._

object MetricsApp {

  sealed trait PredClass
  object PredClass {
    case object NotPredicted extends PredClass
    case object Correct extends PredClass
    case object WrongAnswer extends PredClass
    case object CorrectTemplate extends PredClass
    case class WrongWh(pred: LowerCaseString, gold: LowerCaseString) extends PredClass
    case class SwappedPrep(pred: LowerCaseString, gold: LowerCaseString) extends PredClass
    case class MissingPrep(gold: LowerCaseString) extends PredClass
    case class ExtraPrep(pred: LowerCaseString) extends PredClass
    case object Other extends PredClass

    val notPredicted: PredClass = NotPredicted
    val correct: PredClass = Correct
    val wrongAnswer: PredClass = WrongAnswer
    val correctTemplate: PredClass = CorrectTemplate
    def wrongWh(pred: LowerCaseString, gold: LowerCaseString): PredClass = WrongWh(pred, gold)
    def swappedPrep(pred: LowerCaseString, gold: LowerCaseString): PredClass = SwappedPrep(pred, gold)
    def missingPrep(gold: LowerCaseString): PredClass = MissingPrep(gold)
    def extraPrep(pred: LowerCaseString): PredClass = ExtraPrep(pred)
    val other: PredClass = Other
  }

  val computePredClass = (question: Instances.QuestionInstance) => {
    val P = PredClass
    question.qas.pred.get(question.string).fold(P.notPredicted) { case (predSlots, predSpans) =>
      question.qas.goldValid.get(question.string) match {
        case Some(qLabel) =>
          val answerSpans = qLabel.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet
          if(answerSpans.exists(predSpans.contains)) P.correct else P.wrongAnswer
        case None =>
          val qaTemplates = Instances.qaSetToQATemplateSet(question.qas)
          val predTemplateSlots = TemplateSlots.fromQuestionSlots(predSlots)
          qaTemplates.gold.get(predTemplateSlots.toTemplateString).as(P.correctTemplate).getOrElse {
            def abstractWh(slots: TemplateSlots) = slots.copy(wh = (if(slots.wh.toString == "what") "what" else "adv").lowerCase)
            val whAbstractedPredTemplateSlots = abstractWh(predTemplateSlots)
            val whError = qaTemplates.gold.values.toList.map(_._1).filter { goldSlots =>
              abstractWh(goldSlots) == whAbstractedPredTemplateSlots
            }.map(goldSlots => P.wrongWh(predTemplateSlots.wh, goldSlots.wh)).headOption
            whError.getOrElse {
              val prepError = predTemplateSlots.prep match {
                case None => // missing prep
                  qaTemplates.gold.values.toList.map(_._1).filter { goldSlots =>
                    abstractWh(goldSlots).copy(prep = None) == whAbstractedPredTemplateSlots ||
                      abstractWh(goldSlots).copy(prep = None, obj2 = None) == whAbstractedPredTemplateSlots
                  }.flatMap(_.prep.map(P.missingPrep(_))).headOption
                case Some(predPrep) =>
                  qaTemplates.gold.values.toList.map(_._1).filter { goldSlots =>
                    abstractWh(goldSlots).copy(prep = Some(predPrep)) == whAbstractedPredTemplateSlots
                  }.flatMap(_.prep.map(P.swappedPrep(predPrep, _))).headOption.orElse {
                    qaTemplates.gold.values.toList.map(_._1).filter { goldSlots =>
                      whAbstractedPredTemplateSlots.copy(prep = None) == abstractWh(goldSlots) ||
                        whAbstractedPredTemplateSlots.copy(prep = None, obj2 = None) == abstractWh(goldSlots)
                    }.headOption.as(P.extraPrep(predPrep))
                  }
              }
              prepError.getOrElse(P.other)
            }
          }
      }
    }
  }

  def runPrepositionAnalysis(predClasses: List[(PredClass, Instances.QuestionInstance)]) = {
    val prepConf = {
      import PredClass._
      predClasses.collect {
        case (SwappedPrep(pred, gold), q) => Confusion.instance(gold, pred, q)
        case (Correct | WrongAnswer | CorrectTemplate | WrongWh(_, _), q) =>
          val prep = q.slots.prep.getOrElse("_".lowerCase)
          Confusion.instance(prep, prep, q)
        case (MissingPrep(gold), q) => Confusion.instance(gold, "_".lowerCase, q)
        case (ExtraPrep(pred), q) => Confusion.instance("_".lowerCase, pred, q)
      }.combineAll
    }

    println(prepConf.stats.prettyString(10))

    val allConfusionPairs = prepConf.matrix.toList.flatMap {
      case (gold, predMap) => predMap.toList.map {
        case (pred, questions) => (gold, pred, questions)
      }
    }
    allConfusionPairs
      .filter(t => t._1 != t._2)
      .sortBy(-_._3.size).takeWhile(_._3.size >= 10).foreach {
      case (gold, pred, questions) =>
        val verbHist = questions.groupBy(_.qas.verb.gold.verbInflectedForms.stem).map {
          case (verb, qs) => verb -> qs.size
        }
        val numInstances = questions.size
        println(s"Gold: $gold; Pred: $pred; num confusions: $numInstances")
        verbHist.toList
          .sortBy(-_._2)
          .takeWhile(_._2 > 1)
          .takeWhile(_._2.toDouble / numInstances >= 0.04)
          .foreach { case (verb, num) =>
            println(f"$verb%12s $num%3d (${num * 100.0 / numInstances}%4.1f%%)")
        }
    }

    val prepPresenceInstances = for {
      (gold, pred, questions) <- allConfusionPairs
      if gold != pred && gold.toString != "_" && pred.toString != "_"
      question <- questions
    } yield {
      val tokens = question.qas.verb.sentence.gold.sentenceTokens
      val goldInSentence = tokens.contains(gold.toString)
      val predInSentence = tokens.contains(pred.toString)
      val label = (goldInSentence, predInSentence) match {
        case (true,   true) => "both in sentence"
        case (false,  true) => "pred in sentence"
        case (true,  false) => "gold in sentence"
        case (false, false) => "neither in sentence"
      }
      label -> (gold, pred)
    }

    val prepPresenceCounts = prepPresenceInstances
      .groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }

    prepPresenceCounts.foreach { case (k, goldPredPairs) =>
      println(f"$k%20s: ${goldPredPairs.size}%d (${goldPredPairs.size * 100.0 / prepPresenceInstances.size}%3.1f%%)")
      goldPredPairs.groupBy(identity).map { case (k, vs) => k -> vs.size }.toList.sortBy(-_._2).take(5).foreach {
        case ((gold, pred), num) => println(f"Gold: $gold%10s | Pred: $pred%10s $num%d")
      }
    }
  }

  case class FilterSpace(
    oneThreshold: Option[OneThresholdFilterSpace] = None,
    twoThreshold: Option[TwoThresholdFilterSpace] = None,
    threeThreshold: Option[ThreeThresholdFilterSpace] = None,
    best: Option[BeamFilter]
  ) {
    def withBest(filter: BeamFilter): FilterSpace = this.copy(best = Some(filter))
    def allFilters = best.map(List(_)).getOrElse(
      oneThreshold.foldMap(_.allFilters) ++
        twoThreshold.foldMap(_.allFilters) ++
        threeThreshold.foldMap(_.allFilters)
    )
  }

  case class OneThresholdFilterSpace(thresholds: List[Double] = Nil) {
    def allFilters = thresholds.map(BeamFilter.oneThreshold)
  }
  case class TwoThresholdFilterSpace(
    qaThresholds: List[Double] = Nil,
    invalidThresholds: List[Double] = Nil
  ) {
    def allFilters = for {
      qa <- qaThresholds
      i <- invalidThresholds
    } yield BeamFilter.twoThreshold(qa, i)
  }

  case class ThreeThresholdFilterSpace(
    questionThresholds: List[Double] = Nil,
    spanThresholds: List[Double] = Nil,
    invalidThresholds: List[Double] = Nil
  ) {
    def allFilters = for {
      q <- questionThresholds
      s <- spanThresholds
      i <- invalidThresholds
      b <- List(true, false)
    } yield BeamFilter.threeThreshold(q, s, i, b)
  }

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

  // TODO move these all into metadata files

  val allQFirstFilters = for {
    qThresh <- (0 to 48 by 8).map(_ / 100.0).toList
    sThresh <- (10 to 90 by 40).map(_ / 100.0)
    iThresh <- (0 to 100 by 25).map(_ / 100.0)
    remBelowInv <- List(false, true)
  } yield BeamFilter.threeThreshold(qThresh, sThresh, iThresh, remBelowInv)

  val allAFirstFilters = (
    (0 to 20) ++ (20 to 50 by 10) ++ (100 to 950 by 50) ++ (960 to 980 by 10) ++ (980 to 1000)
  ).map(_ / 1000.0).toList.map(spanThreshold => BeamFilter.oneThreshold(spanThreshold))

  // val bestAFirstFilter = BeamFilter(0.10, 0.6, 0.9, false)
  // val bestQFirstFilter = BeamFilter.threeThreshold(0.10, 0.5, 0.9, true)
  val qFirstRecall2Filter = BeamFilter.threeThreshold(0.05, 0.2, 0.5, false)

  // TODO make this choice a command-line argument?
  // val allFilters = allAFirstFilters
  // val allFilters = allQFirstFilters
  val allFilters = List(qFirstRecall2Filter)
  // val allFilters = List()

  // val recall2Filter = BeamFilter.threeThreshold(0.05, 0.2, 0.5, false)

  def nullBucketer[I] = Map.empty[String, I => String]

  def verbBucketers(verbFreq: (LowerCaseString => Int)) = Map(
    "verb-freq" -> Bucketers.verbFreq(
      verbFreq,
      NonEmptyList.of(0, 10, 50, 150, 250, 500, 750, 1000))
  )

  def domainBucketers = Map(
    "domain" -> ((verb: Instances.VerbInstance) => SentenceId.fromString(verb.sentence.gold.sentenceId).documentId.domain.toString)
  )

  def renderVerbPrediction(sentenceTokens: Vector[String], verb: VerbPrediction) = {
    val sentenceStr = Text.render(sentenceTokens)
    val verbStr = s"${sentenceTokens(verb.verbIndex)} (${verb.verbIndex})"
    val qasString = verb.questions.sortBy(-_.questionProb).map {
      case QuestionPrediction(questionSlots, questionProb, invalidProb, answerSpans) =>
        val qString = questionSlots.renderQuestionString(verb.verbInflectedForms)
        val aStrings = answerSpans.sortBy(-_._2).map { case (span, spanProb) =>
          f"${Text.renderSpan(sentenceTokens, (span.begin until span.end).toSet)}%s ($spanProb%.2f)"
        }.mkString(" / ")
        f"$questionProb%4.2f / $invalidProb%4.2f\t$qString%s\t$aStrings%s"
    }.mkString("\n")
    s"$sentenceStr\n$verbStr\n$qasString"
  }

  import qasrl.labeling.SlotBasedLabel
  import nlpdata.datasets.wiktionary.VerbForm

  def renderFilteredPrediction(
    sentenceTokens: Vector[String],
    verbIndex: Int,
    preds: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
  ) = {
    val sentenceStr = Text.render(sentenceTokens)
    val verbStr = s"${sentenceTokens(verbIndex)} (${verbIndex})"
    val qasString = preds.toList.map {
      case (qString, (_, answerSpans)) =>
        val aStrings = answerSpans.map { span =>
          Text.renderSpan(sentenceTokens, (span.begin until span.end).toSet)
        }.mkString(" / ")
        s"$qString\t$aStrings"
    }.mkString("\n")
    s"$sentenceStr\n$verbStr\n$qasString"
  }

  def renderPredStuff(qas: Instances.QASetInstance) = {
    val tokens = qas.verb.sentence.gold.sentenceTokens
    val unfilteredStr = renderVerbPrediction(tokens, qas.verb.pred)
    val filteredStr = renderFilteredPrediction(tokens, qas.verb.pred.verbIndex, qas.pred)
    unfilteredStr + "\n" + filteredStr
  }

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

  def runDenseCurveMetrics(
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
              M.hchoose(
                "question" ->> I.getQuestionBoundedAcc,
                "question with answer" ->> I.getQuestionWithAnswerBoundedAcc,
                "answer span" ->> M.split(I.questionToQAs) {
                  I.getQABoundedAcc
                }
              )
            },
            "templated questions" ->> (
              I.qaSetToQATemplateSet andThen
                M.split(I.qaTemplateSetToQuestionTemplates) {
                  M.hchoose(
                    "question" ->> I.getQuestionTemplateAcc,
                    "question with answer" ->> I.getQuestionTemplateWithAnswerAcc,
                    "answer span" ->> M.split(I.questionTemplateToQATemplates) {
                      I.getQATemplateAcc
                    }
                  )
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
        val questionsPerVerb = "questions per verb" ->> stats.get("full questions").get("question").stats.predicted.toDouble / raw("num verbs")
        val spansPerVerb = "spans per verb" ->> stats.get("full questions").get("answer span").stats.predicted.toDouble / raw("num verbs")
        stats + questionsPerVerb + spansPerVerb
      }
    )

    val questionRecallThresholds = (1 to 48).map(_.toDouble / 4).toList

    // filter by recall thresholds, take max (lb) performing setting under each threshold
    val questionTunedResults = results.updateWith("predictions")(
      filters => Chosen(
        questionRecallThresholds.map(thresh =>
          thresh -> filters.filter(_.get("questions per verb") >= thresh)
            .keepMaxBy(_.get("full questions").get("question with answer").stats.accuracyLowerBound)
        ).filter(_._2.nonEmpty).toMap
      )
    )

    val spanRecallThresholds = (1 to 12).map(_.toDouble / 4).toList

    val spanTunedResults = results.updateWith("predictions")(
      filters => Chosen(
        spanRecallThresholds.map(thresh =>
          thresh -> filters.filter(_.get("spans per verb") >= thresh)
            .keepMaxBy(_.get("full questions").get("answer span").stats.accuracyLowerBound)
        ).filter(_._2.nonEmpty).toMap
      )
    )

    val questionAccBounds = questionTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("question" ->> results.get("full questions").get("question")) ::
          ("questions per verb" ->> results.get("questions per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }
    val questionTemplateAcc = questionTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("question template" ->> results.get("templated questions").get("question")) ::
          ("questions per verb" ->> results.get("questions per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }

    val questionAnswerAccBounds = questionTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("question" ->> results.get("full questions").get("question with answer")) ::
          ("questions per verb" ->> results.get("questions per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }
    val questionAnswerTemplateAcc = questionTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("question template" ->> results.get("templated questions").get("question with answer")) ::
          ("questions per verb" ->> results.get("questions per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }

    val spanAccBounds = spanTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("span" ->> results.get("full questions").get("answer span")) ::
          ("spans per verb" ->> results.get("spans per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }

    val templatedSpanAcc = spanTunedResults.get("predictions")
      .map(_.data.head)
      .map { case (filter, results) =>
        ("span" ->> results.get("templated questions").get("answer span")) ::
          ("spans per verb" ->> results.get("spans per verb")) ::
          ("beam filter" ->> filter) :: HNil
    }

    import ammonite.ops._
    val metricsFilename = "dense-metrics.txt"
    write.over(pwd / metricsFilename, "")

    def printMetrics[M: HasMetrics](label: String, metrics: M) = {
      def pr(s: String) = {
        println(s)
        write.append(pwd / metricsFilename, s + "\n")
      }
      pr(label + ":")
      pr(metrics.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))

    }

    printMetrics("Question", questionAccBounds)
    printMetrics("Question template", questionTemplateAcc)
    printMetrics("Question with answer", questionAnswerAccBounds)
    printMetrics("Question template with answer", questionAnswerTemplateAcc)
    printMetrics("Span", spanAccBounds)
    printMetrics("Spans with templates", templatedSpanAcc)

    questionTunedResults.get("predictions").data.get(2.0).foreach(overall =>
      printMetrics("Overall at 2.0", overall.data.head._2)
    )
  }

  def runDenseMetrics(
    getVerbFrequency: => (LowerCaseString => Int),
    gold: Dataset,
    pred: Map[String, SentencePrediction],
    metadataDir: NIOPath
  ) = {
    import qfirst.{Instances => I}
    import qfirst.metrics.{Transformers => M}
    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._
    import monocle.function.{all => Optics}

    val filtersPath = metadataDir.resolve("filters.json")
    val filterSpaceEither = {
      import io.circe.jawn
      import io.circe.generic.auto._
      jawn.decodeFile[FilterSpace](new java.io.File(filtersPath.toString))
    }

    filterSpaceEither.map { filterSpace =>
      val computeMetrics = M.split(I.sentenceToVerbs) {
        M.bucket(/*verbBucketers(getVerbFrequency) ++ */ domainBucketers) {
          M.hchoose(
            "verbs" ->> ((vi: I.VerbInstance) => Count(vi.gold.verbInflectedForms.stem)),
            "predictions" ->> (
              M.choose(filterSpace.allFilters) { filter =>
                I.verbToQASet(filterGoldDense, filter) andThen
                M.split(I.qaSetToQuestions) {
                  M.hchoose(
                    "question" ->> I.getQuestionBoundedAcc,
                    "question with answer" ->> I.getQuestionWithAnswerBoundedAcc,
                    "answer span" ->> M.split(I.questionToQAs) {
                      I.getQABoundedAcc
                    }
                  )
                }
              }
            )
          )
        }
      }

      val raw = Instances.foldMapInstances(gold, pred)(computeMetrics)

      val rawCollapsed = raw.collapsed

      // compute number of questions and spans per verb
      val results = rawCollapsed.updateWith("predictions")(
        _.map { stats => // stats for given filter
          val questionsPerVerb = "questions per verb" ->> stats.get("question").stats.predicted.toDouble / rawCollapsed("verbs").stats.numInstances
          val spansPerVerb = "spans per verb" ->> stats.get("answer span").stats.predicted.toDouble / rawCollapsed("verbs").stats.numInstances
          stats + questionsPerVerb + spansPerVerb
        }
      )

      // choose the filter with best results that has >= 2 Qs/verb recall
      val questionTunedResults = results.updateWith("predictions")(
        _.filter(_.get("questions per verb") >= 2.0)
          .keepMaxBy(_.get("question with answer").stats.accuracyLowerBound)
      )

      val bestFilter = questionTunedResults.get("predictions").data.head._1

      {
        val printer = io.circe.Printer.spaces2
        import io.circe.generic.auto._
        import io.circe.syntax._
        Files.write(filtersPath, printer.pretty(filterSpace.withBest(bestFilter).asJson).getBytes("UTF-8"))
      }

      def getMetricsString[M: HasMetrics](m: M) =
        m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

      println("Overall at ≥2.0: " + getMetricsString(questionTunedResults))

      // do automated error analysis

      val computePredClassesForSentences = M.split(I.sentenceToVerbs) {
        M.bucket(/* verbBucketers(getVerbFrequency) ++ */ domainBucketers) {
          I.verbToQASet(filterGoldDense, bestFilter) andThen
          M.split(I.qaSetToQuestions) {
            ((q: I.QuestionInstance) => computePredClass(q) -> q) andThen Count[(PredClass, I.QuestionInstance)]
            // (computePredClass *** identity[I.QuestionInstance]) andThen Count[(PredClass, I.QuestionInstance)]
          }
        }
      }

      val predClasses = Instances
        .foldMapInstances(gold, pred)(computePredClassesForSentences)
        .map(_.filter(_._1 != PredClass.NotPredicted))

      val mainErrorClasses = {
        import PredClass._
        predClasses.map(
          _.values.map(_._1).filter(_ != PredClass.Correct).map {
            case WrongWh(_, _) => WrongWh("_".lowerCase, "_".lowerCase)
            case SwappedPrep(_, _) => SwappedPrep("_".lowerCase, "_".lowerCase)
            case MissingPrep(_) => MissingPrep("_".lowerCase)
            case ExtraPrep(_) => ExtraPrep("_".lowerCase)
            case x => x
          }
        )
      }

      val bucketedErrorClasses = mainErrorClasses.map(
        _.foldMap(
          M.bucket(Map("class" -> ((x: PredClass) => x.toString))) {
            ((_: PredClass) => 1)
          }
        )
      )

      val whConf = {
        import PredClass._
        predClasses.collapsed.values.collect {
          case (Correct | WrongAnswer | CorrectTemplate, q) => Confusion.instance(q.slots.wh, q.slots.wh, q)
          case (WrongWh(pred, gold), q) => Confusion.instance(gold, pred, q)
        }.combineAll
      }

      println(whConf.stats.prettyString(0))

      println("Collapsed error classes: " + getMetricsString(bucketedErrorClasses.collapsed))
      println("All bucketed error classes: " + getMetricsString(bucketedErrorClasses))

      def writeExamples(path: NIOPath, examples: Vector[Instances.QuestionInstance], rand: util.Random) = {
        val examplesString = rand.shuffle(examples.map(renderQuestionExample(_))).mkString("\n")
        Files.write(path, examplesString.getBytes("UTF-8"))
      }

      val collapsedPredClasses = predClasses.collapsed

      val examplesDir = metadataDir.resolve("examples")
      if(!Files.exists(examplesDir)) {
        Files.createDirectories(examplesDir)
      }

      {
        val r = new util.Random(235867962L)
        writeExamples(
          examplesDir.resolve("template.txt"),
          collapsedPredClasses.values.collect { case (PredClass.CorrectTemplate, q) => q },
          r
        )
        writeExamples(
          examplesDir.resolve("prep-swap.txt"),
          collapsedPredClasses.values.collect { case (PredClass.SwappedPrep(_, _), q) => q },
          r
        )
        writeExamples(
          examplesDir.resolve("other.txt"),
          collapsedPredClasses.values.collect { case (PredClass.Other, q) => q },
          r
        )
      }

      // performance on verbs by frequency

      val verbBucketedResults = raw.map(
        _.updateWith("predictions")(
          _.data(bestFilter).get("question with answer")
        )
      )
      println("Overall by verb: " + getMetricsString(verbBucketedResults))

      val domainsDir = examplesDir.resolve("domain")
      if(!Files.exists(domainsDir)) {
        Files.createDirectories(domainsDir)
      }
      {
        val r = new util.Random(22646L)
        predClasses.data.foreach { case (buckets, results) =>
          // val instances = results.get("predictions").incorrect ++ results.get("predictions").uncertain
          val bucketDir = domainsDir.resolve(buckets("domain"))
          if(!Files.exists(bucketDir)) {
            Files.createDirectories(bucketDir)
          }
          val instances = results.values.collect { case (PredClass.Other, q) => q }
          writeExamples(bucketDir.resolve("other.txt"), instances, r)
        }
      }
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

    val computeMetrics = {
      // M.bucket(verbBucketers(getVerbFrequency)) {
      M.hchoose(
        "num sentences" ->> ((vi: I.SentenceInstance) => 1),
        "num verbs" ->> M.split(I.sentenceToVerbs)(((vi: I.VerbInstance) => 1)),
        "predictions" ->> M.split(I.sentenceToVerbs) {
          M.choose(allFilters) { filterPred =>
            I.verbToQASet(filterGoldNonDense, filterPred) andThen
            M.hchoose(
              "gold span counts" ->> (
                I.qaSetToSpanSet andThen ((spanSet: I.SpanSetInstance) =>
                  Count(spanSet.gold.flatten.toSet.toList)
                )
              ),
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
        }
      )
      // }
    }

    val raw = Instances.foldMapInstances(gold, pred)(computeMetrics)

    val best = raw.updateWith("predictions")(_.keepMaxBy(_.get("full questions").collapsed.get("question with answer").stats.f1))

    val bestCollapsed = best.updateWith("predictions")(_.map(_.updateWith("full questions")(_.collapsed)))

    val prep = best.get("predictions").data.head._2.get("full questions")
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
    // println(prep.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    println(bestCollapsed.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    // println(prepExamples)

    {
      import ammonite.ops._
      write.over(pwd / "prep-errors.tsv", prepExamples)
    }
  }

  def readPredictions(
    path: NIOPath, inputType: String
  ): Either[io.circe.Error, Map[String, SentencePrediction]] = {
    import qfirst.frames._
    import ammonite.ops._
    import io.circe.jawn
    if(inputType == "question") {
      read.lines(Path(path, pwd)).toList
        .traverse(jawn.decode[SentencePrediction])
        .map(_.map(pred => pred.sentenceId -> pred).toMap)
    } else if(inputType == "clausal") {
      read.lines(Path(path, pwd)).toList
        .traverse(jawn.decode[ClausalSentencePrediction])
        .map(_.map(pred => pred.sentenceId -> pred.toSentencePrediction).toMap)
    } else ??? // shouldn't happen since we validated the input
  }

  def readVerbFrequencies(data: Dataset) = {
    data.sentences.iterator
      .flatMap(s => s._2.verbEntries.values.map(_.verbInflectedForms.stem).iterator)
      .foldLeft(Map.empty[LowerCaseString, Int].withDefaultValue(0)) {
      (counts, stem) => counts + (stem -> (counts(stem) + 1))
    }
  }

  def program(qasrlBankPath: NIOPath, predDir: NIOPath, mode: String, inputType: String) = {
    // TODO validate using opts stuff from decline?
    if(!Set("dense", "non-dense", "dense-curve").contains(mode)) {
      System.err.println("Must specify mode of non-dense, dense-curve, or dense.")
      System.exit(1)
    }
    if(!Set("question", "clausal").contains(inputType)) {
      System.err.println("Must specify input type of of question or clausal.")
      System.exit(1)
    }
    val metadataDir = predDir.resolve(mode)
    lazy val trainData = Data.readDataset(qasrlBankPath.resolve("orig").resolve("train.jsonl.gz"))
    lazy val verbFrequencies = readVerbFrequencies(trainData)
    val gold = if(mode == "dense" || mode == "dense-curve") {
      Data.readDataset(qasrlBankPath.resolve("dense").resolve("dev.jsonl.gz"))
    } else Data.readDataset(qasrlBankPath.resolve("orig").resolve("dev.jsonl.gz"))
    val predFile = if(mode == "dense" || mode == "dense-curve") {
      predDir.resolve("predictions-dense.jsonl")
    } else predDir.resolve("predictions.jsonl")
    val predEither = readPredictions(predFile, inputType)
    predEither match {
      case Left(error) => System.err.println(error)
      case Right(pred) =>
        if(mode == "non-dense") runNonDenseMetrics(verbFrequencies, gold, pred)
        else if(mode == "dense-curve") runDenseCurveMetrics(verbFrequencies, gold, pred)
        else if(mode == "dense") runDenseMetrics(verbFrequencies, gold, pred, metadataDir) match {
          case Right(res) => res
          case Left(err) =>
            System.err.println(err)
            System.exit(1)
            ???
        }
        else ???
    }
  }

  val runMetrics = Command(
    name = "mill qfirst.runMetrics",
    header = "Calculate metrics."
  ) {
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the QA-SRL Bank."
    )
    val predPath = Opts.option[NIOPath](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val mode = Opts.option[String](
      "mode", metavar = "non-dense|dense-curve|dense", help = "Which eval to run."
    ).withDefault("dense")
    val inputType = Opts.option[String](
      "inputType", metavar = "question|clausal", help = "Which input type to handle."
    ).withDefault("question")

    (goldPath, predPath, mode, inputType).mapN(program)
  }

  def main(args: Array[String]): Unit = {
    val result = runMetrics.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(main) => IO { main }
    }
    result.unsafeRunSync
  }
}
