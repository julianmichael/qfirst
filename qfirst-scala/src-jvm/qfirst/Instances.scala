package qfirst

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

// import scala.collection.immutable.SortedMap

import HasMetrics.ops._

case class Thresholds(
  questionThreshold: Double,
  spanThreshold: Double,
  invalidThreshold: Double,
  shouldRemoveSpansBelowInvalidProb: Boolean
) {
  // val filterBeamNonoverlapping: BeamFilter = (fullVerb: VerbEntry, metadata: VerbPredictionMetadata) => {
  val filterBeamNonoverlapping = (prediction: VerbPrediction) => {
    def overlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
      x.begin <= y.end && y.begin <= x.end
    }
    def hasOverlap(acc: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])], span: AnswerSpan) = {
      val allAccSpans = acc.toList.flatMap(_._2._2).toSet
      allAccSpans.exists(overlaps(_, span))
    }
    val initAcc = Map.empty[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
    val predQAs = prediction.questions.toList.sortBy(-_.questionProb).foldLeft(initAcc) {
      case (acc, qPrediction) =>
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
            val qString = qPrediction.questionSlots.renderQuestionString(prediction.verbInflectedForms)
            acc + (qString -> (qPrediction.questionSlots, goodSpans))
          }
        }
    }
    predQAs
  }
}
object Thresholds {
  implicit def thresholdsShow: Show[Thresholds] = {
    import cats.derived.auto.show._
    cats.derived.semi.show
  }
}

object Instances {

  case class SentenceInstance(
    gold: Sentence,
    pred: SentencePrediction
  )

  val sentenceToVerbs = (sentence: SentenceInstance) => {
    // NOTE: filtering out "empty" verbs which are mistakenly present in the data
    sentence.gold.verbEntries.toList.filter(_._2.questionLabels.nonEmpty).flatMap {
      case (verbIndex, goldVerb) =>
        val predVerb = sentence.pred.verbs.find(_.verbIndex == verbIndex)
        if(predVerb.isEmpty) System.err.println("Could not find predicted verb: TODO better error message")
        predVerb.map(VerbInstance(sentence, goldVerb, _))
    }
  }

  case class VerbInstance(
    sentence: SentenceInstance,
    gold: VerbEntry,
    pred: VerbPrediction
  )

  def verbToQASet(
    filterGold: VerbEntry => (Map[String, QuestionLabel], Map[String, QuestionLabel]),
    filterPred: VerbPrediction => Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
  ) = (verb: VerbInstance) => {
    val (goldInvalidQAs, goldValidQAs) = filterGold(verb.gold)
    val predQAs = filterPred(verb.pred)
    QASetInstance(verb, goldValidQAs, goldInvalidQAs, predQAs)
  }

  case class QASetInstance(
    verb: VerbInstance,
    goldValid: Map[String, QuestionLabel],
    goldInvalid: Map[String, QuestionLabel],
    pred: Map[String, (SlotBasedLabel[VerbForm], Set[AnswerSpan])]
  )

  val qaSetToQuestions = (qas: QASetInstance) => {
    val allStrings = (qas.goldValid.keySet ++ qas.goldInvalid.keySet ++ qas.pred.keySet)
    allStrings.toList.map { qString =>
      val qSlots = qas.goldValid.get(qString).map(_.questionSlots).orElse {
        qas.goldInvalid.get(qString).map(_.questionSlots).orElse {
          qas.pred.get(qString).map(_._1)
        }
      }.get // should always work
      QuestionInstance(qas, qString, qSlots)
    }
  }

  case class QuestionInstance(
    qas: QASetInstance,
    string: String,
    slots: SlotBasedLabel[VerbForm]
  )

  val getQuestionConf = (question: QuestionInstance) => {
    val isPredicted = question.qas.pred.contains(question.string)
    val isTrue = isPredicted == question.qas.goldValid.contains(question.string)
    if(isTrue && isPredicted) Conf(tp = 1)
    else if(!isTrue && isPredicted) Conf(fp = 1)
    else if(!isTrue && !isPredicted) Conf(fn = 1)
    else Conf(tn = 1)
  }

  def foldMapInstances[A: Monoid](
    gold: Dataset,
    pred: Map[String, SentencePrediction])(
    mapping: SentenceInstance => A
  ): A = {
    gold.sentences.toList
      .filter(_._2.verbEntries.values.exists(_.questionLabels.nonEmpty)) // NOTE due to some "empty sentences" -- to fix in data
      .foldMap { case (sentenceId, goldSentence) =>
        pred.get(sentenceId) match {
          case None =>
            System.err.println("Could not find sentence; TODO better error message")
            Monoid[A].empty
          case Some(predSentence) =>
            mapping(SentenceInstance(goldSentence, predSentence))
        }
    }
  }
}

object Bucketers {

  import Instances._

  val wh = (question: QuestionInstance) => {
    question.slots.wh.toString
  }

  val prep = (question: QuestionInstance) => {
    question.slots.prep.fold("_")(_.toString)
  }

  object Mappers {
    val prepIsPresent = (x: String) => if(x != "none") "yes" else "no"
    val whAdv = (x: String) => if(x == "who" || x == "what") "who/what" else "adv"
  }

  // object Templated {
  //   val wh = (instance: TemplatedQuestionInstance) => {
  //     instance.thisQuestionSlots(0)
  //   }

  //   val prep = (instance: TemplatedQuestionInstance) => {
  //     instance.thisQuestionSlots(4)
  //   }
  // }
}

object Run {

  // import io.circe.Error
  import ammonite.ops._

  def readPredictions(path: java.nio.file.Path) = {
    import io.circe.jawn
    read.lines(Path(path, pwd)).toList
      .traverse(jawn.decode[SentencePrediction])
      .map(_.map(pred => pred.sentenceId -> pred).toMap)
  }

  // @main
  def main(goldFile: java.nio.file.Path, predFile: java.nio.file.Path) = {
    val filterGold = (verb: VerbEntry) => {
      val (invalids, valids) = verb.questionLabels.toList.flatMap {
        case (questionString, qLabel) =>
          val judgments = qLabel.answerJudgments.toList.map(_.judgment)
          val numInvalid = judgments.filter(_.isInvalid).size
          val numAnswers = judgments.size
          if(numAnswers >= 3) {
            if(numInvalid == 0) Some(Right(questionString -> qLabel))
            else Some(Left(questionString -> qLabel))
          } else None
      }.separate
      invalids.toMap -> valids.toMap
    }
    val questionBucketers = Map(
      // "wh" -> Bucketers.wh,
      "prep" -> Bucketers.prep
    )
    // val templateBucketers = Map(
    //   "wh" -> Bucketers.Templated.wh,
    //   "prep" -> Bucketers.Templated.prep,
    //   )
    val predThresholds = Thresholds(
      questionThreshold = 0.1,
      spanThreshold = 0.5,
      invalidThreshold = 0.9,
      shouldRemoveSpansBelowInvalidProb = true)
    val computeMetrics = {
      import qfirst.{Instances => I}
      import qfirst.{Metrics => M}
      // val M = Metrics
      M.split(I.sentenceToVerbs) {
        I.verbToQASet(filterGold, predThresholds.filterBeamNonoverlapping) andThen
        M.choose {
          "full question" -> M.split(I.qaSetToQuestions) {
            M.bucket(questionBucketers) {
              I.getQuestionConf
            }
          }
        }
      }
      // Metrics.choose(List(predThresholds)) { (thresholds: Thresholds) =>
      //   getFilteredInstance(thresholds) andThen
      //   Metrics.choose(
      //     // "span" -> Metrics.bucket(Map.empty[String, Instance => String]) { // TODO heterogeneous choose
      //     //   (getSpanSetInstance andThen getSpanConf)
      //     // },
      //     "full question" ->
      //       Metrics.split(getQuestionInstances) {
      //           getQuestionConf
      //       },
      //     // "question template" ->
      //     //   Metrics.split(getTemplatedQuestionInstances) {
      //     //     Metrics.bucket(templateBucketers) {
      //     //       getTemplatedQuestionConf
      //     //     }
      //     //   }
      //   )
      // }
    }
    val sortSpec = {
      import MapTree._
      import SortQuery._
      import MetricValue._
      val getMetricDouble = (mv: MetricValue) => mv match {
        case MetricInt(x) => x.toDouble
        case MetricDouble(x) => x
        case MetricIntOfTotal(x, _) => x.toDouble
      }
      List(
        key("num gold") {
          value[String](getMetricDouble)
        }
      )
    }

    val gold = Data.readDataset(goldFile)
    val predEither = readPredictions(predFile)
    predEither match {
      case Left(error) => System.err.println(error)
      case Right(pred) =>
        val rawResult = Instances.foldMapInstances(gold, pred)(computeMetrics)
        val result = rawResult
        println(result.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
    }

    // take initial params, maxes, etc.
    // println(result.getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("wh")
    //           .all.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("prep")
    //           .all.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("prep")
    //             // .filter(_.numGold >= 50)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
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
  }
}

// case class TemplatedQuestionInstance(
//   goldValidQuestions: Set[String],
//   goldInvalidQuestions: Set[String],
//   predQuestions: Set[String],
//   thisQuestion: String,
//   thisQuestionSlots: List[String]
// )

// case class SpanSetInstance(
//   verbIndex: Int,
//   goldSpanSets: List[Set[AnswerSpan]],
//   predSpanSets: List[Set[AnswerSpan]]
// )

// TODO for dependency length calculation
// case class AlignedSpanSetInstance(
//   verbIndex: Int,
//   alignedSpanSets: List[(Set[AnswerSpan], Set[AnswerSpan])],
//   goldOnlySpanSets: List[Set[AnswerSpan]],
//   predOnlySpanSets: List[Set[AnswerSpan]],
//   thisSpanSet: Ior[Set[AnswerSpan], Set[AnswerSpan]]
// )

// val getSpanSetInstance = (instance: Instance) => {
//   val goldSpanSets = instance.goldVerb.questionLabels.toList.flatMap {
//     case (questionString, qLabel) =>
//       val judgments = qLabel.answerJudgments.toList.map(_.judgment)
//       val numInvalid = judgments.filter(_.isInvalid).size
//       if(instance.goldThreshold(numInvalid, judgments.size)) Some(
//         judgments.flatMap(_.getAnswer).map(_.spans).foldLeft(Set.empty[AnswerSpan])(_ union _)
//       )
//       else None
//   }
//   val predSpanSets = instance.predVerb.verbEntry.questionLabels.values.toList
//     .map(_.answerJudgments.flatMap(_.judgment.getAnswer).map(_.spans).foldLeft(Set.empty[AnswerSpan])(_ union _)) ++
//     instance.predVerb.badQAs.toList.map(_._2._2)
//   SpanSetInstance(instance.goldVerb.verbIndex, goldSpanSets, predSpanSets)
// }

// NOTE: does not do bipartite matching thing
// val getSpanConf = (instance: SpanSetInstance) => {
//   case class SpanAlignment(
//     remainingPred: Set[Set[AnswerSpan]],
//     conf: Conf)
//   val alignment = instance.goldSpanSets.foldLeft(SpanAlignment(instance.predSpanSets.toSet, Conf())) {
//     case (SpanAlignment(preds, conf), goldSpanSet) =>
//       preds.find(_.exists(s => goldSpanSet.exists(overlaps(_, s)))) match {
//         case None => (SpanAlignment(preds, conf |+| Conf(fn = 1)))
//         case Some(predSpanSet) => (SpanAlignment(preds - predSpanSet, conf |+| Conf(tp = 1)))
//       }
//   }
//   alignment.conf |+| Conf(fp = alignment.remainingPred.size)
// }

// val getQuestionInstances = (instance: Instance) => {
//   val (goldInvalidQuestionStringsList, goldValidQuestionStringsList) = instance.goldVerb.questionLabels.toList.map {
//     case (questionString, qLabel) =>
//       val judgments = qLabel.answerJudgments.toList.map(_.judgment)
//       val numInvalid = judgments.filter(_.isInvalid).size
//       if(instance.goldThreshold(numInvalid, judgments.size)) Right(questionString)
//       else Left(questionString)
//   }.separate
//   val goldValidQuestionStrings = goldValidQuestionStringsList.toSet
//   val goldInvalidQuestionStrings = goldInvalidQuestionStringsList.toSet
//   val predQuestionStrings = instance.predVerb.verbEntry.questionLabels.keySet ++ instance.predVerb.badQAs.keySet

//   (goldInvalidQuestionStrings ++ goldValidQuestionStrings ++ predQuestionStrings).toList.map { thisQuestion =>
//     val thisQuestionSlots = instance.goldVerb.questionLabels.get(thisQuestion).map(_.questionSlots).orElse {
//       instance.predVerb.verbEntry.questionLabels.get(thisQuestion).map(_.questionSlots).orElse {
//         instance.predVerb.badQAs.get(thisQuestion).map(_._1)
//       }
//     }.get
//     QuestionInstance(
//       goldValidQuestionStrings,
//       goldInvalidQuestionStrings,
//       predQuestionStrings,
//       thisQuestion,
//       thisQuestionSlots
//     )
//   }
// }

// def abstractNoun(slot: LowerCaseString): String = {
//   if(slot.toString == "who") "what"
//   else if(slot.toString == "someone") "something"
//   else slot.toString
// }
// def getTemplateTokensFromLabel(questionLabel: QuestionLabel) = {
//   val slots = questionLabel.questionSlots
//   List(
//     abstractNoun(slots.wh),
//     slots.subj.fold("_")(abstractNoun),
//     if(questionLabel.isPassive) "verb[pss]" else "verb",
//     slots.obj.fold("_")(abstractNoun),
//     slots.prep.fold("_")(_.toString),
//     slots.obj2.fold("_")(abstractNoun)
//   )
// }
// def getTemplateTokensFromSlots(slots: SlotBasedLabel[VerbForm]) = {
//   val isPassive = slots.verb == PastParticiple &&
//     slots.verbPrefix.map(_.toString).toSet.intersect(Set("be", "is", "was")).nonEmpty
//   List(
//     abstractNoun(slots.wh),
//     slots.subj.fold("_")(abstractNoun),
//     if(isPassive) "verb[pss]" else "verb",
//     slots.obj.fold("_")(abstractNoun),
//     slots.prep.fold("_")(_.toString),
//     slots.obj2.fold("_")(abstractNoun)
//   )
// }

// val getTemplatedQuestionInstances = (instance: Instance) => {
//   def getTemplateString(templateTokens: List[String]) = Text.render(templateTokens.map(_.trim).filter(s => s.nonEmpty && s != "_"))
//   val (goldInvalidTemplateTokensList, goldValidTemplateTokensList) = instance.goldVerb.questionLabels.toList.map {
//     case (questionString, qLabel) =>
//       val judgments = qLabel.answerJudgments.toList.map(_.judgment)
//       val numInvalid = judgments.filter(_.isInvalid).size
//       if(instance.goldThreshold(numInvalid, judgments.size)) Right(getTemplateTokensFromLabel(qLabel))
//       else Left(getTemplateTokensFromLabel(qLabel))
//   }.separate
//   val goldValidTemplateTokens = goldValidTemplateTokensList.toSet
//   val goldInvalidTemplateTokens = goldInvalidTemplateTokensList.toSet
//   val predTemplateTokens = instance.predVerb.verbEntry.questionLabels.values.map(getTemplateTokensFromLabel).toSet ++
//     instance.predVerb.badQAs.values.toList.map(_._1).map(getTemplateTokensFromSlots).toSet
//   val goldValidTemplateStrings = goldValidTemplateTokens.map(getTemplateString)
//   val goldInvalidTemplateStrings = goldInvalidTemplateTokens.map(getTemplateString)
//   val predTemplateStrings = predTemplateTokens.map(getTemplateString)

//   val templateStringAndTokenPairs = (goldValidTemplateTokens ++ goldInvalidTemplateTokens ++ predTemplateTokens)
//     .groupBy(getTemplateString)
//     .map { case (templateString, tokenLists) => templateString -> tokenLists.head }

//   templateStringAndTokenPairs.toList.map { case (templateString, templateTokens) =>
//     TemplatedQuestionInstance(
//       goldValidQuestions = goldValidTemplateStrings,
//       goldInvalidQuestions = goldInvalidTemplateStrings,
//       predQuestions = predTemplateStrings,
//       thisQuestion = templateString,
//       thisQuestionSlots = templateTokens
//     )
//   }
// }

// val getQuestionConf = (instance: QuestionInstance) => {
//   val isPredicted = instance.predQuestions.contains(instance.thisQuestion)
//   val isTrue = isPredicted == instance.goldValidQuestions.contains(instance.thisQuestion)
//   if(isTrue && isPredicted) Conf(tp = 1)
//   else if(!isTrue && isPredicted) Conf(fp = 1)
//   else if(!isTrue && !isPredicted) Conf(fn = 1)
//   else Conf(tn = 1)
// }

// val getTemplatedQuestionConf = (instance: TemplatedQuestionInstance) => {
//   val isPredicted = instance.predQuestions.contains(instance.thisQuestion)
//   val isTrue = isPredicted == instance.goldValidQuestions.contains(instance.thisQuestion)
//   if(isTrue && isPredicted) Conf(tp = 1)
//   else if(!isTrue && isPredicted) Conf(fp = 1)
//   else if(!isTrue && !isPredicted) Conf(fn = 1)
//   else Conf(tn = 1)
// }

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

// def getFilteredInstance = (thresholds: Thresholds) => (instance: UnfilteredInstance) => {
//   val filteredVerbPrediction = thresholds.filterBeamNonoverlapping(instance.predVerbGoodQuestions, instance.predVerbMetadata)
//   val filteredInstance = Instance(
//     instance.goldThreshold,
//     instance.goldSentence,
//     instance.goldVerb,
//     instance.predSentence,
//     filteredVerbPrediction
//   )
//   filteredInstance
// }

