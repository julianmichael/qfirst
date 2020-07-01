package qfirst.datasets.conll05

import jjm.ling.ESpan
// import jjm.implicits._

import cats.data.NonEmptyList
import cats.implicits._

import qfirst.datasets.PredArgStructure
import qfirst.datasets.PropBankPredicate

object PropsParsing {

  import fastparse.all._
  // TODO import more specifically...
  import cats._
  import cats.data._
  import cats.implicits._
  private[this] type TokenState[A] = State[Int, A]

  import fastparse.all._
  private[this] val nextWord: TokenState[Int] = for {
    index <- State.get
    _ <- State.set(index + 1)
  } yield index

  private[this] val labelP: P[String] =
    P(CharIn(('A' to 'Z') ++ ('0' to '9') ++ Seq('-')).rep.!)

  private[this] val wordP: P[TokenState[Int]] =
    P("*").map(_ => nextWord)

  private[this] val wordsP: P[TokenState[List[Int]]] =
    P(wordP.rep).map(_.toList.sequence)

  private[this] val spanP: P[TokenState[ESpan]] =
    P(wordP.rep(1)).map(_.toList.sequence.map(is => ESpan(is.head, is.last + 1)))

  private[this] val argP: P[TokenState[(String, ESpan)]] =
    P("(" ~ labelP ~ spanP ~ ")").map {
      case (label, spanState) => for {
        span <- spanState
      } yield label -> span
    }

  private[this] val wordsAndArgP: P[TokenState[(String, ESpan)]] =
    P(wordsP ~ argP).map {
      case (wordsState, argState) => for {
        _ <- wordsState
        arg <- argState
      } yield arg
    }

  private[this] val allArgsP: P[TokenState[List[(String, ESpan)]]] =
    P(wordsAndArgP.rep ~ wordsP).map {
      case (argStates, finalWords) => for {
        args <- argStates.toList.sequence
        // TODO return an informative error if finalWords aren't all used up by the end?
        _ <- finalWords
      } yield args
    }

  def readArgumentSpans(s: String): List[(String, ESpan)] =
    allArgsP.parse(s).get.value.runA(0).value

  def readSentenceProps(lines: NonEmptyList[String]): List[PredArgStructure[String, ESpan]] = {
    val rows = lines.map(_.split("\\s+").toVector).toList.toVector

    val columns = rows.transpose

    def getPredLemma(fields: Vector[String]): Option[String] = Option(fields(0)).filter(_ != "-")

    val predicates = rows.zipWithIndex.flatMap { case (fields, index) =>
      getPredLemma(fields).map(lemma =>
        index -> lemma
      )
    }
    val argLists = columns.drop(1).map(col => readArgumentSpans(col.mkString))

    require(predicates.size == argLists.size)

    predicates.zip(argLists).map { case ((index, lemma), args) =>
      PredArgStructure(index, lemma, args)
    }.toList
  }

  import fs2.Pipe

  // TODO replace with .toNel after fs2 update
  def streamPropsFromLines[F[_]](split: CoNLL05Split): Pipe[F, String, (CoNLL05SentenceId, List[PredArgStructure[String, ESpan]])] = lines => {
    lines.groupAdjacentBy(_.trim.nonEmpty)
      .collect { case (isNonEmpty, sentenceLines) if isNonEmpty => NonEmptyList.fromList(sentenceLines.toList).get }
      .map(readSentenceProps)
      .zipWithIndex
      .map { case (props, sentenceNum) => CoNLL05SentenceId(split, sentenceNum.toInt) -> props }
  }
}
