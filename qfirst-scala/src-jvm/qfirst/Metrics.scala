package qfirst

import Consolidate._
import Predictions._

import cats.Functor
import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._

// import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.datasets.wiktionary.PastParticiple
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

// import qasrl.QuestionProcessor
// import qasrl.TemplateStateMachine

import qasrl.bank.Data
import qasrl.bank.AnnotationRound
import qasrl.bank.AnswerSource
import qasrl.bank.QuestionSource
// import qasrl.bank.FullData

import qasrl.data.Answer
import qasrl.data.AnswerLabel
import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

import qasrl.labeling.SlotBasedLabel

import simulacrum._

// import scala.collection.immutable.SortedMap

object Metrics {
  sealed trait MetricValue {
    import MetricValue._
    def render: String = this match {
      case MetricDouble(x) => f"$x%.3f"
      case MetricInt(x) => f"$x%d"
      case MetricIntOfTotal(value, total) => f"$value%d (${value.toDouble / total * 100}%.2f%%)"
    }
  }
  object MetricValue {
    case class MetricDouble(value: Double) extends MetricValue
    case class MetricInt(value: Int) extends MetricValue
    case class MetricIntOfTotal(value: Int, total: Int) extends MetricValue

    def apply(value: Int): MetricValue = MetricInt(value)
    def apply(value: Double): MetricValue = MetricDouble(value)
    def apply(value: Int, total: Int): MetricValue = MetricIntOfTotal(value, total)
  }

  sealed trait MapTree[A, B] {
    import MapTree._
    def toStringPretty(
      renderKey: A => String,
      renderValue: B => String,
      numSpacesPerIndent: Int = 2
    )(implicit o: Ordering[A]): String = toStringPrettyAux(renderKey, renderValue, numSpacesPerIndent, 0)
    private def toStringPrettyAux(
      renderKey: A => String,
      renderValue: B => String,
      numSpacesPerIndent: Int,
      indentLevel: Int
    )(implicit o: Ordering[A]): String = this match {
      case Leaf(value) => renderValue(value)
      case Fork(children) =>
        "{\n" + children.toList.sortBy(_._1).map {
          case (key, child) =>
            (" " * (numSpacesPerIndent * (indentLevel + 1))) +
              renderKey(key) + ": " +
              child.toStringPrettyAux(renderKey, renderValue, numSpacesPerIndent, indentLevel + 1)
        }.mkString(",\n") + "\n" + (" " * (numSpacesPerIndent * indentLevel)) + "}"
    }

    // TODO handle merge conflicts somehow
    def merge(other: MapTree[A, B], mergeFn: (B, B) => B): MapTree[A, B] = (this, other) match {
      case (Leaf(x), Leaf(y)) => Leaf(mergeFn(x, y))
      case (Fork(x), Fork(y)) =>
        val allKeys = x.keySet ++ y.keySet
        Fork(
          allKeys.toList.map(key =>
            key -> x.get(key).fold(y(key))(xChild => y.get(key).fold(xChild)(yChild => xChild.merge(yChild, mergeFn)))
          ).toMap
        )
      case _ => ???
    }
  }
  object MapTree {
    case class Fork[A, B](children: Map[A, MapTree[A, B]]) extends MapTree[A, B]
    case class Leaf[A, B](value: B) extends MapTree[A, B]
    def leaf[A, B](value: B): MapTree[A, B] = Leaf[A, B](value)
    def fork[A, B](children: Map[A, MapTree[A, B]]): MapTree[A, B] = Fork(children)
    def fork[A, B](children: (A, MapTree[A, B])*): MapTree[A, B] = Fork(children.toMap)
    def fromMap[A, B](m: Map[A, B]) = Fork(m.map { case (k, v) => k -> Leaf[A, B](v) })
    def fromPairs[A, B](children: (A, B)*): MapTree[A, B] = fromMap(children.toMap)
  }

  import scala.language.implicitConversions
  @typeclass trait HasMetrics[A] {
    def getMetrics(a: A): MapTree[String, MetricValue]
  }
  import HasMetrics.ops._

  object Metrics {

    case class Bucketed[MetricData](
      data: Map[Map[String, String], MetricData]
    ) {
      def mapBucketValues(key: String, mapper: String => String)(implicit M: Monoid[MetricData]) = Bucketed(
        data.toList.map {
          case (buckets, m) => buckets.get(key).fold(buckets)(v => buckets + (key -> mapper(v))) -> m
        }.groupBy(_._1).map {
          case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
        }
      )

      def collapseBuckets(keys: String*)(implicit M: Monoid[MetricData]): Bucketed[MetricData] = collapseBuckets(keys.toList)
      def collapseBuckets(keys: List[String])(implicit M: Monoid[MetricData]): Bucketed[MetricData] = Bucketed(
        data.toList.map {
          case (buckets, m) => keys.foldLeft(buckets)(_ - _) -> m
        }.groupBy(_._1).map {
          case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
        }
      )

      def filter(p: MetricData => Boolean): Bucketed[MetricData] = Bucketed(
        data.collect { case (k, v) if p(v) => k -> v }
      )

      def collapsed(implicit M : Monoid[MetricData]) = data.values.toList.combineAll
    }
    object Bucketed {
      implicit val bucketedFunctor: Functor[Bucketed] = new Functor[Bucketed] {
        def map[A, B](fa: Bucketed[A])(f: A => B): Bucketed[B] = Bucketed(
          fa.data.map { case (k, v) => k -> f(v) }
        )
      }
      implicit def bucketedMonoid[A : Monoid]: Monoid[Bucketed[A]] = {
        import cats.derived.auto.monoid._
        cats.derived.semi.monoid
      }
      // TODO draw out the proportion calculation into a separate method so user can decide when to do it
      implicit def bucketedHasMetrics[A : HasMetrics : Monoid]: HasMetrics[Bucketed[A]] = new HasMetrics[Bucketed[A]] {
        def getMetrics(ba: Bucketed[A]): MapTree[String, MetricValue] = {
          val collapsedMetrics = ba.collapsed.getMetrics
          val computeIntsOfTotal = (x: MetricValue, y: MetricValue) => (x, y) match {
            case (MetricValue.MetricInt(xi), MetricValue.MetricInt(yi)) => MetricValue.MetricIntOfTotal(xi, yi)
            case (x, _) => x
          }
          MapTree.fork(
            ba.data.map { case (bucketSpec, bucketData) =>
              val keyStr = "{ " + bucketSpec.toList.sortBy(_._1).map { case (k, v) => s"$k: $v"}.mkString(", ") + " }"
              keyStr -> bucketData.getMetrics.merge(collapsedMetrics, computeIntsOfTotal)
            }
          )
        }
      }
    }

    def bucket[Instance, MetricData](
      bucketers: Map[String, Instance => String])(
      metric: Instance => MetricData
    ): (Instance => Bucketed[MetricData]) = (instance: Instance) => {
      val bucket = bucketers.map { case (key, bucketer) => key -> bucketer(instance) }
      Bucketed(Map(bucket -> metric(instance)))
    }

    case class Chosen[Param, MetricData](
      data: Map[Param, MetricData]
    )
    object Chosen {
      implicit def chosenFunctor[Param]: Functor[Chosen[Param, ?]] = new Functor[Chosen[Param, ?]] {
        def map[A, B](fa: Chosen[Param, A])(f: A => B): Chosen[Param, B] = Chosen(
          fa.data.map { case (k, v) => k -> f(v) }
        )
      }
      implicit def chosenMonoid[Param, A: Monoid]: Monoid[Chosen[Param, A]] = {
        import cats.derived.auto.monoid._
        cats.derived.semi.monoid
      }
      implicit def chosenHasMetrics[Param: Show, A: HasMetrics]: HasMetrics[Chosen[Param, A]] = new HasMetrics[Chosen[Param, A]] {
        def getMetrics(ba: Chosen[Param, A]): MapTree[String, MetricValue] = {
          MapTree.fork(
            ba.data.map { case (param, bucketData) =>
              param.show -> bucketData.getMetrics
            }
          )
        }
      }
    }

    def choose[Param, Instance, MetricData](
      params: List[Param])(
      metric: Param => Instance => MetricData
    ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
      Chosen(params.map(p => p -> metric(p)(instance)).toMap)
    }

    def choose[Param, Instance, MetricData](
      choices: (Param, Instance => MetricData)*
    ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
      Chosen(choices.map { case (p, metric) => p -> metric(instance) }.toMap)
    }

    // TODO typed choice

    def split[BigInstance, SmallInstance, MetricData: Monoid](
      splittingFn: BigInstance => List[SmallInstance])(
      metric: SmallInstance => MetricData
    ): (BigInstance => MetricData) = (bigInstance: BigInstance) => {
      splittingFn(bigInstance).foldMap(metric)
    }
  }

  case class Conf(
    tp: Int = 0,
    tn: Int = 0,
    fp: Int = 0,
    fn: Int = 0
  ) {

    def numPredicted = tp + fp

    def numGold = tp + fn

    def precision = if(tp + fp > 0) {
      tp.toDouble / (tp + fp)
    } else 0.0

    def recall = if(tp + fn > 0) {
      tp.toDouble / (tp + fn)
    } else 0.0

    def f1 = if((precision + recall) > 0.0) {
      2 * (precision * recall) / (precision + recall)
    } else 0.0

    def allStats: MapTree[String, MetricValue] = MapTree.fromPairs(
      "num gold" -> MetricValue(numGold),
      "num predicted" -> MetricValue(numPredicted),
      "precision" -> MetricValue(precision),
      "recall" -> MetricValue(recall),
      "f1" -> MetricValue(f1)
    )
  }
  object Conf {
    implicit val confMonoid = new Monoid[Conf] {
      def empty: Conf = Conf()
      def combine(x: Conf, y: Conf): Conf =
        Conf(x.tp + y.tp, x.tn + y.tn, x.fp + y.fp, x.fn + y.fn)
    }
    implicit val confHasMetrics = new HasMetrics[Conf] {
      def getMetrics(conf: Conf) = conf.allStats
    }

    def fromSets[A](
      gold: Set[A],
      pred: Set[A],
      all: Set[A]
    ): Conf = Conf(
      tp = gold.intersect(pred).size,
      tn = (all -- pred -- gold).size,
      fp = (pred -- gold).size,
      fn = (gold -- pred).size
    )

    def fromSets[A](
      gold: Set[A],
      pred: Set[A]
    ): Conf = fromSets(gold, pred, gold ++ pred)
  }

  case class BoundedAcc(
    numValidReferences: Int,
    numInvalidReferences: Int,
    numCorrect: Int,
    numIncorrect: Int,
    numUncertain: Int
  ) {
    def numPredicted = numCorrect + numIncorrect + numUncertain
    def accuracyLowerBound = numCorrect.toDouble / numPredicted
    def accuracyUpperBound = (numCorrect + numUncertain).toDouble / numPredicted

    def allStats: MapTree[String, MetricValue] = MapTree.fromPairs(
      "num valid refs" -> MetricValue(numValidReferences),
      "num invalid refs" -> MetricValue(numInvalidReferences),
      "num predicted" -> MetricValue(numPredicted),
      "acc-lb" -> MetricValue(accuracyLowerBound),
      "acc-ub" -> MetricValue(accuracyUpperBound)
    )
  }

  case class UnfilteredInstance(
    goldThreshold: (Int, Int) => Boolean,
    goldSentence: Sentence,
    goldVerb: VerbEntry,
    predSentence: Sentence,
    predVerbGoodQuestions: VerbEntry,
    predVerbMetadata: VerbPredictionMetadata
  )

  case class FilteredVerbPrediction(
    verbEntry: VerbEntry,
    badQAs: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
  )

  case class Instance(
    goldThreshold: (Int, Int) => Boolean,
    goldSentence: Sentence,
    goldVerb: VerbEntry,
    predSentence: Sentence,
    predVerb: FilteredVerbPrediction
  )

  case class QuestionInstance(
    goldValidQuestions: Set[String],
    goldInvalidQuestions: Set[String],
    predQuestions: Set[String],
    thisQuestion: String,
    thisQuestionSlots: SlotBasedLabel[VerbForm]
  )

  case class TemplatedQuestionInstance(
    goldValidQuestions: Set[String],
    goldInvalidQuestions: Set[String],
    predQuestions: Set[String],
    thisQuestion: String,
    thisQuestionSlots: List[String]
  )

  def overlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
    x.begin <= y.end && y.begin <= x.end
  }

  case class SpanSetInstance(
    verbIndex: Int,
    goldSpanSets: List[Set[AnswerSpan]],
    predSpanSets: List[Set[AnswerSpan]]
  )

  // TODO for dependency length calculation
  // case class AlignedSpanSetInstance(
  //   verbIndex: Int,
  //   alignedSpanSets: List[(Set[AnswerSpan], Set[AnswerSpan])],
  //   goldOnlySpanSets: List[Set[AnswerSpan]],
  //   predOnlySpanSets: List[Set[AnswerSpan]],
  //   thisSpanSet: Ior[Set[AnswerSpan], Set[AnswerSpan]]
  // )

  val getSpanSetInstance = (instance: Instance) => {
    val goldSpanSets = instance.goldVerb.questionLabels.toList.flatMap {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        if(instance.goldThreshold(numInvalid, judgments.size)) Some(
          judgments.flatMap(_.getAnswer).map(_.spans).foldLeft(Set.empty[AnswerSpan])(_ union _)
        )
        else None
    }
    val predSpanSets = instance.predVerb.verbEntry.questionLabels.values.toList
      .map(_.answerJudgments.flatMap(_.judgment.getAnswer).map(_.spans).foldLeft(Set.empty[AnswerSpan])(_ union _)) ++
      instance.predVerb.badQAs.toList.map(_._2._2)
    SpanSetInstance(instance.goldVerb.verbIndex, goldSpanSets, predSpanSets)
  }

  // does not do bipartite matching thing
  val getSpanConf = (instance: SpanSetInstance) => {
    case class SpanAlignment(
      remainingPred: Set[Set[AnswerSpan]],
      conf: Conf)
    val alignment = instance.goldSpanSets.foldLeft(SpanAlignment(instance.predSpanSets.toSet, Conf())) {
      case (SpanAlignment(preds, conf), goldSpanSet) =>
        preds.find(_.exists(s => goldSpanSet.exists(overlaps(_, s)))) match {
          case None => (SpanAlignment(preds, conf |+| Conf(fn = 1)))
          case Some(predSpanSet) => (SpanAlignment(preds - predSpanSet, conf |+| Conf(tp = 1)))
        }
    }
    alignment.conf |+| Conf(fp = alignment.remainingPred.size)
  }

  // from the (full beam, prediction metadata) produce the (parseable narrow beam, extra unparseable QA pairs).
  type BeamFilter = (VerbEntry, VerbPredictionMetadata) => FilteredVerbPrediction

  val getQuestionInstances = (instance: Instance) => {
    val (goldInvalidQuestionStringsList, goldValidQuestionStringsList) = instance.goldVerb.questionLabels.toList.map {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        if(instance.goldThreshold(numInvalid, judgments.size)) Right(questionString)
        else Left(questionString)
    }.separate
    val goldValidQuestionStrings = goldValidQuestionStringsList.toSet
    val goldInvalidQuestionStrings = goldInvalidQuestionStringsList.toSet
    val predQuestionStrings = instance.predVerb.verbEntry.questionLabels.keySet ++ instance.predVerb.badQAs.keySet

    (goldInvalidQuestionStrings ++ goldValidQuestionStrings ++ predQuestionStrings).toList.map { thisQuestion =>
      val thisQuestionSlots = instance.goldVerb.questionLabels.get(thisQuestion).map(_.questionSlots).orElse {
        instance.predVerb.verbEntry.questionLabels.get(thisQuestion).map(_.questionSlots).orElse {
          instance.predVerb.badQAs.get(thisQuestion).map(_._1)
        }
      }.get
      QuestionInstance(
        goldValidQuestionStrings,
        goldInvalidQuestionStrings,
        predQuestionStrings,
        thisQuestion,
        thisQuestionSlots
      )
    }
  }

  def abstractNoun(slot: LowerCaseString): String = {
    if(slot.toString == "who") "what"
    else if(slot.toString == "someone") "something"
    else slot.toString
  }
  def getTemplateTokensFromLabel(questionLabel: QuestionLabel) = {
    val slots = questionLabel.questionSlots
    List(
      abstractNoun(slots.wh),
      slots.subj.fold("_")(abstractNoun),
      if(questionLabel.isPassive) "verb[pss]" else "verb",
      slots.obj.fold("_")(abstractNoun),
      slots.prep.fold("_")(_.toString),
      slots.obj2.fold("_")(abstractNoun)
    )
  }
  def getTemplateTokensFromSlots(slots: SlotBasedLabel[VerbForm]) = {
    val isPassive = slots.verb == PastParticiple &&
      slots.verbPrefix.map(_.toString).toSet.intersect(Set("be", "is", "was")).nonEmpty
    List(
      abstractNoun(slots.wh),
      slots.subj.fold("_")(abstractNoun),
      if(isPassive) "verb[pss]" else "verb",
      slots.obj.fold("_")(abstractNoun),
      slots.prep.fold("_")(_.toString),
      slots.obj2.fold("_")(abstractNoun)
    )
  }

  val getTemplatedQuestionInstances = (instance: Instance) => {
    def getTemplateString(templateTokens: List[String]) = Text.render(templateTokens.map(_.trim).filter(s => s.nonEmpty && s != "_"))
    val (goldInvalidTemplateTokensList, goldValidTemplateTokensList) = instance.goldVerb.questionLabels.toList.map {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        if(instance.goldThreshold(numInvalid, judgments.size)) Right(getTemplateTokensFromLabel(qLabel))
        else Left(getTemplateTokensFromLabel(qLabel))
    }.separate
    val goldValidTemplateTokens = goldValidTemplateTokensList.toSet
    val goldInvalidTemplateTokens = goldInvalidTemplateTokensList.toSet
    val predTemplateTokens = instance.predVerb.verbEntry.questionLabels.values.map(getTemplateTokensFromLabel).toSet ++
      instance.predVerb.badQAs.values.toList.map(_._1).map(getTemplateTokensFromSlots).toSet
    val goldValidTemplateStrings = goldValidTemplateTokens.map(getTemplateString)
    val goldInvalidTemplateStrings = goldInvalidTemplateTokens.map(getTemplateString)
    val predTemplateStrings = predTemplateTokens.map(getTemplateString)

    val templateStringAndTokenPairs = (goldValidTemplateTokens ++ goldInvalidTemplateTokens ++ predTemplateTokens)
      .groupBy(getTemplateString)
      .map { case (templateString, tokenLists) => templateString -> tokenLists.head }

    templateStringAndTokenPairs.toList.map { case (templateString, templateTokens) =>
      TemplatedQuestionInstance(
        goldValidQuestions = goldValidTemplateStrings,
        goldInvalidQuestions = goldInvalidTemplateStrings,
        predQuestions = predTemplateStrings,
        thisQuestion = templateString,
        thisQuestionSlots = templateTokens
      )
    }
  }

  val getQuestionConf = (instance: QuestionInstance) => {
    val isPredicted = instance.predQuestions.contains(instance.thisQuestion)
    val isTrue = isPredicted == instance.goldValidQuestions.contains(instance.thisQuestion)
    if(isTrue && isPredicted) Conf(tp = 1)
    else if(!isTrue && isPredicted) Conf(fp = 1)
    else if(!isTrue && !isPredicted) Conf(fn = 1)
    else Conf(tn = 1)
  }

  val getTemplatedQuestionConf = (instance: TemplatedQuestionInstance) => {
    val isPredicted = instance.predQuestions.contains(instance.thisQuestion)
    val isTrue = isPredicted == instance.goldValidQuestions.contains(instance.thisQuestion)
    if(isTrue && isPredicted) Conf(tp = 1)
    else if(!isTrue && isPredicted) Conf(fp = 1)
    else if(!isTrue && !isPredicted) Conf(fn = 1)
    else Conf(tn = 1)
  }

  // def computeDenseMetricsForInstance(
  //   instance: Instance
  // ): DenseMetrics = {
  //   // case class BoundedAcc(
  //   //   numValidReferences: Int,
  //   //   numInvalidReferences: Int,
  //   //   numCorrect: Int,
  //   //   numIncorrect: Int,
  //   //   numUncertain: Int
  //   // ) {

  //   val questionAcc = {
  //     val goldQuestionLabelOpt = instance.goldVerb.questionLabels.get(instance.questionString)
  //     val isQuestionCertain = goldQuestionLabelOpt.exists(l => l.answerJudgments.size)
  //     val isValid = instance.goldVerb.questionLabels.get(instance.questionString).exists { qLabel =>
  //       val judgments = qLabel.answerJudgments.toList.map(_.judgment)
  //       val numInvalid = judgments.filter(_.isInvalid).size
  //       instance.goldThreshold(numInvalid, judgments.size)
  //     }
  //     val isPredicted = instance.predVerb.verbEntry.questionLabels.contains(instance.questionString) ||
  //       instance.predVerb.badQAs.contains(instance.questionString)
  //     val isTrue = isValid == isPredicted
  //   }
  //   DenseMetrics(full = BaseDenseMetrics(questionAcc))
  // }

  case class Thresholds(
    questionThreshold: Double,
    spanThreshold: Double,
    invalidThreshold: Double,
    shouldRemoveSpansBelowInvalidProb: Boolean
  ) {
    def filterQuestion(pred: QuestionPrediction): Option[(SlotBasedLabel[VerbForm], NonEmptyList[AnswerSpan])] = {
      val spans = pred.answerSpans.collect {
        case (span, prob) if prob >= spanThreshold => span
      }
      if(pred.questionProb >= questionThreshold && pred.invalidProb <= invalidThreshold) {
        NonEmptyList.fromList(spans).map(pred.questionSlots -> _)
      } else None
    }
    val filterBeamNonoverlapping: BeamFilter = (fullVerb: VerbEntry, metadata: VerbPredictionMetadata) => {
      type BeamAcc = (List[QuestionLabel], Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])])
      def hasOverlap(acc: BeamAcc, span: AnswerSpan) = {
        val allAccSpans = acc._1.flatMap(_.answerJudgments).flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet ++ acc._2.flatMap(_._2._2).toSet
        allAccSpans.exists(overlaps(_, span))
      }
      val initAcc = (List.empty[QuestionLabel], Map.empty[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])])
      val (qLabels, badQAs) = metadata.qaScores.toList.sortBy(-_._2.questionProb).foldLeft(initAcc) {
        case (acc, (qString, qPrediction)) =>
          if(qPrediction.questionProb < questionThreshold) acc
          else if(qPrediction.invalidProb > invalidThreshold) acc
          else {
            val goodSpans = qPrediction.answerSpans.collect {
              case (span, prob) if prob >= spanThreshold &&
                  !hasOverlap(acc, span) &&
                  (!shouldRemoveSpansBelowInvalidProb || prob >= qPrediction.invalidProb) => span
            }.toSet
            if(goodSpans.isEmpty) acc
            else {
              fullVerb.questionLabels.get(qString) match {
                case None => (acc._1, acc._2 + (qString -> (qPrediction.questionSlots, goodSpans)))
                case Some(qLabel) =>
                  val newQuestionLabel = qLabel.copy(
                    answerJudgments = Set(AnswerLabel("model-XXX", Answer(goodSpans)))
                  )
                  (newQuestionLabel :: acc._1, acc._2)
              }
            }
          }
      }
      val newVerbEntry = fullVerb.copy(questionLabels = qLabels.map(l => l.questionString -> l).toMap.toSortedMap)
      FilteredVerbPrediction(newVerbEntry, badQAs)
    }
  }
  object Thresholds {
    implicit def thresholdsShow: Show[Thresholds] = {
      import cats.derived.auto.show._
      cats.derived.semi.show
    }
  }

  def getFilteredInstance = (thresholds: Thresholds) => (instance: UnfilteredInstance) => {
    val filteredVerbPrediction = thresholds.filterBeamNonoverlapping(instance.predVerbGoodQuestions, instance.predVerbMetadata)
    val filteredInstance = Instance(
      instance.goldThreshold,
      instance.goldSentence,
      instance.goldVerb,
      instance.predSentence,
      filteredVerbPrediction
    )
    filteredInstance
  }

  def foldMapInstances[A: Monoid](
    gold: Dataset,
    goldThreshold: (Int, Int) => Boolean,
    pred: Dataset,
    predMetadata: Metadata)(
    mapping: UnfilteredInstance => A
  ): A = {
    gold.sentences.toList
      .filter(_._2.verbEntries.values.exists(_.questionLabels.nonEmpty)) // NOTE due to some "empty sentences" -- to fix in data
      .foldMap { case (sentenceId, goldSentence) =>
        val predSentence = pred.sentences(sentenceId)
        // NOTE filter below due to some "empty verbs" -- to fix in data
        goldSentence.verbEntries.toList.filter(_._2.questionLabels.nonEmpty).foldMap { case (verbIndex, goldVerb) =>
          val verbMeta = predMetadata(sentenceId)(verbIndex)
          val predVerb =  predSentence.verbEntries(verbIndex)
          val instance = UnfilteredInstance(
            goldThreshold = goldThreshold,
            goldSentence = goldSentence,
            goldVerb = goldVerb,
            predSentence = predSentence,
            predVerbGoodQuestions = predVerb,
            predVerbMetadata = verbMeta
          )
          mapping(instance)
        }
    }
  }

  object Bucketers {

    val wh = (instance: QuestionInstance) => {
      instance.thisQuestionSlots.wh.toString
    }

    val prep = (instance: QuestionInstance) => {
      instance.thisQuestionSlots.prep.fold("_")(_.toString)
    }

    object Mappers {
      val prepIsPresent = (x: String) => if(x != "none") "yes" else "no"
      val whAdv = (x: String) => if(x == "who" || x == "what") "who/what" else "adv"
    }

    object Templated {
      val wh = (instance: TemplatedQuestionInstance) => {
        instance.thisQuestionSlots(0)
      }

      val prep = (instance: TemplatedQuestionInstance) => {
        instance.thisQuestionSlots(4)
      }
    }
  }

  import ammonite.ops._

  // @main
  def main(goldFile: java.nio.file.Path, predDir: java.nio.file.Path) = {
    System.err.println("Success.")
    val gold = Data.readDataset(goldFile)
    val (pred, metadata) = Consolidate.readDatasetAndMetadata(Path(predDir, pwd))
    val goldThreshold = (numInvalid: Int, numAnswers: Int) => {
      numInvalid == 0 && numAnswers >= 3
    }
    val questionBucketers = Map(
      "wh" -> Bucketers.wh,
      "prep" -> Bucketers.prep
    )
    val templateBucketers = Map(
      "wh" -> Bucketers.Templated.wh,
      "prep" -> Bucketers.Templated.prep,
      )
    val predThresholds = Thresholds(
      questionThreshold = 0.1,
      spanThreshold = 0.5,
      invalidThreshold = 0.9,
      shouldRemoveSpansBelowInvalidProb = true)
    val computeMetrics = {
      Metrics.choose(List(predThresholds)) { (thresholds: Thresholds) =>
        getFilteredInstance(thresholds) andThen
        Metrics.choose(
          // "span" -> Metrics.bucket(Map.empty[String, Instance => String]) { // TODO heterogeneous choose
          //   (getSpanSetInstance andThen getSpanConf)
          // },
          "full question" ->
            Metrics.split(getQuestionInstances) {
              Metrics.bucket(questionBucketers) {
                getQuestionConf
              }
            },
          "question template" ->
            Metrics.split(getTemplatedQuestionInstances) {
              Metrics.bucket(templateBucketers) {
                getTemplatedQuestionConf
              }
            }
        )
      }
    }

    val rawResult = foldMapInstances(
      gold, goldThreshold,
      pred, metadata)(
      computeMetrics)
    // take initial params, maxes, etc.
    val result = rawResult.data(predThresholds)
    // println(result.getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("wh")
    //           .all.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("prep")
    //           .all.toStringPretty(identity, x => x.render))
    println("================================")
    println(result.map(b =>
              b.collapseBuckets("prep")
                // .filter(_.numGold >= 50)
            ).getMetrics.toStringPretty(identity, x => x.render))
    // println(result.map(b =>
    //           b.mapBucketValues("prep", Bucketers.Mappers.prepIsPresent)
    //             .mapBucketValues("wh", Bucketers.Mappers.whAdv)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("prep")
    //             .mapBucketValues("wh", Bucketers.Mappers.whAdv)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("wh")
    //             .mapBucketValues("prep", Bucketers.Mappers.prepIsPresent)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    println("================================")
    println(result.map(_.collapsed).getMetrics.toStringPretty(identity, x => x.render))
  }
}

