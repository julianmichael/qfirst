package qfirst

import cats.Functor
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.LowerCaseStrings._

import qasrl.QuestionProcessor
import qasrl.TemplateStateMachine

import qasrl.data.Answer
import qasrl.data.AnswerLabel
import qasrl.data.AnswerSpan
import qasrl.data.Dataset
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

import qasrl.labeling.SlotBasedLabel

import scala.collection.immutable.SortedMap

import ammonite.ops._

import Predictions._

object Consolidate {
  case class VerbPredictionMetadata(
    verbIndex: Int,
    badQuestions: List[SlotBasedLabel[VerbForm]],
    qaScores: Map[String, QuestionPrediction],
    )
  object VerbPredictionMetadata {
    import io.circe.{Encoder, Decoder}
    import qasrl.data.JsonCodecs.{slotBasedLabelEncoder, slotBasedLabelDecoder}
    implicit val verbPredictionMetadataEncoder: Encoder[VerbPredictionMetadata] = {
      import io.circe.generic.semiauto._
      deriveEncoder[VerbPredictionMetadata]
    }
    implicit val verbPredictionMetadataDecoder: Decoder[VerbPredictionMetadata] = {
      import io.circe.generic.semiauto._
      deriveDecoder[VerbPredictionMetadata]
    }
  }

  type Metadata = Map[String, Map[Int, VerbPredictionMetadata]]

  implicit class RichMap[A, B](val m: Map[A, B]) extends AnyVal {
    def toSortedMap(implicit o: Ordering[A]): SortedMap[A, B] = SortedMap(m.toSeq: _*)
  }

  // @main
  def main(_predFile: java.nio.file.Path) = {
    val predFile = Path(_predFile, pwd)
    val outPrefix = predFile / up

    val (dataset, metadata) = createDatasetAndMetadata(predFile)
    writeDatasetAndMetadata(dataset, metadata, outPrefix)
  }

  def writeDatasetAndMetadata(dataset: Dataset, metadata: Metadata, outDir: Path) = {
    val datasetPath = outDir / "full-data.jsonl"
    val metadataPath = outDir / "metadata.json"
    val printer = io.circe.Printer.noSpaces

    {
      import io.circe.syntax._
      import qasrl.data.JsonCodecs._
      val datasetLines = dataset.sentences.toSeq.map {
        case (_, sentence) => printer.pretty(sentence.asJson) + "\n"
      }
      write(datasetPath, datasetLines)
    }

    {
      import io.circe.syntax._
      import io.circe.generic.auto._
      write(metadataPath, printer.pretty(metadata.asJson))
    }
  }

  def readDatasetAndMetadata(dir: Path) = {
    val datasetPath = dir / "full-data.jsonl"
    val metadataPath = dir / "metadata.json"

    val dataset = {
      import io.circe.jawn
      import qasrl.data.JsonCodecs._
      val lines = read.lines(datasetPath)
      val sentences = lines.map(l => jawn.decode[Sentence](l).right.get).map(s => s.sentenceId -> s).toMap.toSortedMap
      Dataset(sentences)
    }

    val metadata = {
      import io.circe.jawn
      import io.circe.generic.auto._
      val str = read(metadataPath)
      jawn.decode[Metadata](str).right.get
    }
    (dataset, metadata)
  }

  def createDatasetAndMetadata(predFile: Path): (Dataset, Metadata) = {
    val predLines = read.lines(predFile)
    import io.circe.jawn
    import qasrl.data.JsonCodecs._
    val predictionsEither = predLines.map(jawn.decode[Prediction]).toList.sequence
    val predictions = predictionsEither match {
      case Left(error) =>
        System.err.println(error)
        System.exit(1)
          ???
      case Right(preds) => preds
    }
    val infoBySentenceId = predictions.groupBy(_.sentenceId).map {
      case (sentenceId, sentencePreds) =>
        val verbEntriesAndMetadata = sentencePreds.map { pred =>
          val questionsOrBadSlots = pred.questions.map { question =>
            val questionString = question.questionSlots.renderQuestionString(pred.verbInflectedForms)
            val positiveLabel = AnswerLabel(
              predFile.toString, Answer(question.answerSpans.map(_._1).toSet)
            )
            val frameOpt = {
              val questionTokensIsh =
                questionString.init.split(" ").toVector.map(_.lowerCase)
              val qPreps = questionTokensIsh
                .filter(TemplateStateMachine.allPrepositions.contains)
                .toSet
              val qPrepBigrams = questionTokensIsh
                .sliding(2)
                .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
                .map(_.mkString(" ").lowerCase)
                .toSet
              val stateMachine = new TemplateStateMachine(
                sentencePreds.head.sentenceTokens,
                pred.verbInflectedForms,
                Some(qPreps ++ qPrepBigrams)
              )
              val questionProcessor = new QuestionProcessor(stateMachine)
              questionProcessor
                .processStringFully(questionString)
                .right.toOption.flatMap { s =>
                s.toList.collect {
                  case QuestionProcessor.CompleteState(_, frame, _) => frame
                }.headOption
              }
            }

            val sanityCheckSlotsOpt = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
              pred.sentenceTokens,
              pred.verbInflectedForms,
              List(questionString)
            ).head

            if(sanityCheckSlotsOpt.exists(_ != question.questionSlots)) {
              System.err.println("Slot reconstruction error for question: " + questionString)
              System.err.println("Old slots: " + question.questionSlots.renderWithSeparator(pred.verbInflectedForms, " "))
              System.err.println(
                "New slots: " + sanityCheckSlotsOpt.fold("<could not reconstruct>")(slots =>
                  slots.renderWithSeparator(pred.verbInflectedForms, " ")
                )
              )
            }

            frameOpt match {
              case None => Left(question.questionSlots)
              case Some(frame) => Right(
                QuestionLabel(
                  questionString = questionString,
                  questionSources = Set(predFile.toString),
                  answerJudgments = Set(positiveLabel),
                  questionSlots = question.questionSlots,
                  tense = frame.tense,
                  isPerfect = frame.isPerfect,
                  isProgressive = frame.isProgressive,
                  isNegated = frame.isNegated,
                  isPassive = frame.isPassive
                )
              )
            }
          }
          val questionPredsByQuestionString = pred.questions.map(
            question => question.questionSlots.renderQuestionString(pred.verbInflectedForms) -> question
          ).toMap
          val (badSlots, questions) = questionsOrBadSlots.separate
          val questionLabels = questions.map(qLabel => qLabel.questionString -> qLabel).toMap.toSortedMap
          val verbEntry = VerbEntry(
            pred.verbIndex,
            pred.verbInflectedForms,
            questionLabels
          )
          val metadata = VerbPredictionMetadata(
            pred.verbIndex,
            badSlots,
            questionPredsByQuestionString
          )
          (verbEntry, metadata)
        }
        val verbEntries = verbEntriesAndMetadata.map(_._1).map(ve => ve.verbIndex -> ve).toMap.toSortedMap
        val sentence = Sentence(sentenceId, sentencePreds.head.sentenceTokens, verbEntries)
        val verbMetadataByVerbIndex = verbEntriesAndMetadata.map(_._2).map(m => m.verbIndex -> m).toMap
        sentenceId -> (sentence, verbMetadataByVerbIndex)
    }
    val sentences = infoBySentenceId.map {
      case (sid, (sentence, _)) => sid -> sentence
    }
    val metadata = infoBySentenceId.map {
      case (sid, (_, verbMetadata)) => sid -> verbMetadata
    }
    val dataset = Dataset(sentences.toSortedMap)
    (dataset, metadata)
  }
}
