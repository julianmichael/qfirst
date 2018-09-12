import $ivy.`org.typelevel::cats-core:1.1.0`
import $ivy.`org.julianmichael::nlpdata:0.2.0`
import $ivy.`org.julianmichael::qasrl:0.1.0`
import $ivy.`org.julianmichael::qasrl-bank:0.1.0`
import $file.Predictions, Predictions.Prediction, Predictions.QuestionPrediction
import $file.Consolidate
import Consolidate.Metadata
import Consolidate.VerbPredictionMetadata
import Consolidate.RichMap
// import $ivy.`io.circe::circe-core:0.9.3`
// import $ivy.`io.circe::circe-generic:0.9.3`

import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._

// import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
// import nlpdata.util.LowerCaseStrings._

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
// import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

import qasrl.labeling.SlotBasedLabel

// import scala.collection.immutable.SortedMap

sealed trait MapTree[A, B] {
  def toStringPretty(
    renderKey: A => String,
    renderValue: B => String,
    numSpacesPerIndent: Int = 2
  ): String = toStringPrettyAux(renderKey, renderValue, numSpacesPerIndent, 0)
  private def toStringPrettyAux(
    renderKey: A => String,
    renderValue: B => String,
    numSpacesPerIndent: Int,
    indentLevel: Int
  ): String = this match {
    case MapTree.Leaf(value) => renderValue(value)
    case MapTree.Fork(children) =>
      "{\n" + children.map {
        case (key, child) =>
          (" " * (numSpacesPerIndent * (indentLevel + 1))) +
            renderKey(key) + ": " +
            child.toStringPrettyAux(renderKey, renderValue, numSpacesPerIndent, indentLevel + 1)
      }.mkString(",\n") + "\n" + (" " * (numSpacesPerIndent * indentLevel)) + "}"
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

case class Conf(
  tp: Int = 0,
  tn: Int = 0,
  fp: Int = 0,
  fn: Int = 0
) {

  def precision = if(tp + fp > 0) {
    tp.toDouble / (tp + fp)
  } else 0.0

  def recall = if(tp + tn > 0) {
    tp.toDouble / (tp + fn)
  } else 0.0

  def f1 = if((precision + recall) > 0.0) {
    2 * (precision * recall) / (precision + recall)
  } else 0.0

  def allStats: MapTree[String, Double] = MapTree.fromPairs(
    "precision" -> precision,
    "recall" -> recall,
    "f1" -> f1
  )
}
object Conf {
  implicit val confMonoid = new Monoid[Conf] {
    def empty: Conf = Conf()
    def combine(x: Conf, y: Conf): Conf =
      Conf(x.tp + y.tp, x.tn + y.tn, x.fp + y.fp, x.fn + y.fn)
  }
}

case class Metrics(
  questionConf: Conf = Conf()
) {
  def all = MapTree.fork("questions" -> questionConf.allStats)
}
object Metrics {
  implicit val metricsMonoid = new Monoid[Metrics] {
    def empty: Metrics = Metrics(Monoid[Conf].empty)
    def combine(x: Metrics, y: Metrics): Metrics =
      Metrics(x.questionConf |+| y.questionConf)
  }
}

def overlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
  x.begin <= y.end && y.begin <= x.end
}

// from the (full beam, prediction metadata) produce the (parseable narrow beam, extra unparseable QA pairs).
type BeamFilter = (VerbEntry, VerbPredictionMetadata) => (VerbEntry, List[(String, Set[AnswerSpan])])

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
    type BeamAcc = (List[QuestionLabel], List[(String, Set[AnswerSpan])])
    def hasOverlap(acc: BeamAcc, span: AnswerSpan) = {
      val allAccSpans = acc._1.flatMap(_.answerJudgments).flatMap(_.judgment.getAnswer).flatMap(_.spans).toSet ++ acc._2.flatMap(_._2).toSet
      allAccSpans.exists(overlaps(_, span))
    }
    val initAcc = (List.empty[QuestionLabel], List.empty[(String, Set[AnswerSpan])])
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
              case None => (acc._1, (qString, goodSpans) :: acc._2)
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
    (newVerbEntry, badQAs)
  }
}

def computeMetrics(
  gold: Dataset,
  goldThreshold: (Int, Int) => Boolean, // num invalids, num answers => is valid
  pred: Dataset,
  predMetadata: Metadata,
  beamFilter: BeamFilter
): Metrics = {
  gold.sentences.toList
    .filter(_._2.verbEntries.values.exists(_.questionLabels.nonEmpty)) // NOTE due to some "empty sentences" -- to fix in data
    .foldMap { case (sentenceId, goldSentence) =>
      val predSentence = pred.sentences(sentenceId)
      // NOTE filter below due to some "empty verbs" -- to fix in data
      goldSentence.verbEntries.toList.filter(_._2.questionLabels.nonEmpty).foldMap { case (verbIndex, goldVerb) =>
        val verbMeta = predMetadata(sentenceId)(verbIndex)
        val (predVerb, badQAs) = beamFilter(predSentence.verbEntries(verbIndex), verbMeta)
        val badQuestionStrings = badQAs.map(_._1).toSet
        val allQuestionStrings = (
          goldVerb.questionLabels.keySet ++
            predVerb.questionLabels.keySet ++
            badQuestionStrings
        )
        val questionConf = allQuestionStrings.toList.foldMap { qString =>
          val isValid = goldVerb.questionLabels.get(qString).exists { qLabel =>
            val judgments = qLabel.answerJudgments.toList.map(_.judgment)
            val numInvalid = judgments.filter(_.isInvalid).size
            goldThreshold(numInvalid, judgments.size)
          }
          val isPredicted = (predVerb.questionLabels.contains(qString) || badQuestionStrings.contains(qString))
          val isTrue = isValid == isPredicted
          if(isTrue && isPredicted) Conf(tp = 1)
          else if(isTrue && !isPredicted) Conf(tn = 1)
          else if(!isTrue && isPredicted) Conf(fp = 1)
          else Conf(fn = 1)
        }
        Metrics(questionConf)
      }
  }
}

import ammonite.ops._

@main
def main(goldFile: String, predDir: Path) = {
  val gold = Data.readDataset(java.nio.file.Paths.get(goldFile))
  val (pred, metadata) = Consolidate.readDatasetAndMetadata(predDir)
  val goldThreshold = (numInvalid: Int, numAnswers: Int) => {
    numInvalid == 0 && numAnswers >= 3
  }
  // TODO use invalid as threshold
  val predThresholds = Thresholds(
    questionThreshold = 0.1,
    spanThreshold = 0.65,
    invalidThreshold = 0.9,
    shouldRemoveSpansBelowInvalidProb = false)
  val metrics = computeMetrics(gold, goldThreshold, pred, metadata, predThresholds.filterBeamNonoverlapping)
  println(metrics.all.toStringPretty(identity, x => f"$x%.3f"))
}

