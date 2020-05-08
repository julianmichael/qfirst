package qfirst.ontonotes

import jjm.ling.ESpan
import jjm.implicits._

import scala.util.Try

import cats.data.State
import cats.data.NonEmptyList
import cats.implicits._

// TODO merge stuff in properly
object PredicateArgumentStructureParsing {

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
}

object SyntaxTreeParsing {
  private[this] type SentenceState[A] = State[List[CoNLLToken], A]

  import fastparse.all._
  private[this] val symbolP: P[String] = P(CharIn('A' to 'Z').rep.!)
  private[this] lazy val treeP: P[SentenceState[SyntaxTree[CoNLLToken]]] =
    P("(" ~ symbolP ~ treeP.rep(1) ~ ")").map {
      case (symbol, childrenState) => for {
        children <- childrenState.toList.sequence
      } yield SyntaxTreeNode(symbol, NonEmptyList.fromList(children).get): SyntaxTree[CoNLLToken]
    } | (P("*") | P("-")).map { _ =>
      for {
        words <- State.get
        _ <- State.set(words.tail)
      } yield SyntaxTreeLeaf(words.head)
    }

  /** Parses a SyntaxTree from its flattened column representation in the CoNLL data.
    *
    * Assumes the data is in the correct format. Undefined behavior otherwise.
    *
    * @param s the flattened column representation of the tree
    * @param words the words of the sentence this tree parses
    */
  def readSyntaxTree(s: String, words: List[CoNLLToken]): SyntaxTree[CoNLLToken] =
    treeP.parse(s).get.value.runA(words).value
}

object CoNLLParsing {

  /** Reads a CoNLLSentence from a list of lines from a CoNLLFile.
    * This will grow as the number of fields of CoNLLSentence grows.
    *
    * Does not expect empty lines on either end of the list.
    * Assumes the lines are taken from a CoNLL data file,
    * undefined behavior otherwise.
    *
    * @param sentenceNum the index of the sentence in the document
    * @param lines the lines of the file containing the sentence's info
    * @return the CoNLL sentence stored in the data
    */
  def readSentence(path: CoNLLPath, sentenceNum: Int, lines: NonEmptyList[String]): CoNLLSentence = {
    val lineArrays = lines.map(_.split("\\s+"))
    val partNum = lineArrays.head(1).toInt
    val words = lineArrays.map(arr => CoNLLToken(index = arr(2).toInt, pos = arr(4), token = arr(3))).toList
    val treeString = lineArrays.map(arr => arr(5)).fold
    val tree = SyntaxTreeParsing.readSyntaxTree(treeString, words)
    val predicates = for {
      (arr, index) <- lineArrays.zipWithIndex.toList
      predicateLemma = arr(6)
      if !predicateLemma.equals("-")
      framesetId = arr(7)
      if !framesetId.equals("-")
      head = words(index)
    } yield Predicate(head.index, predicateLemma, framesetId)

    val paStructures = for {
      (pred, num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
      spansString = lineArrays.map(arr => arr(11 + num)).fold
      argSpans = PredicateArgumentStructureParsing.readArgumentSpans(spansString)
    } yield PredicateArgumentStructure(pred, argSpans)
    val sentencePath = CoNLLSentencePath(path, sentenceNum)
    CoNLLSentence(sentencePath, partNum, words, tree, paStructures)
  }

  private[this] val firstLineRegex = """#begin document \((.*)\); part ([0-9]+)""".r
  private[this] val endDocumentLine = "#end document"

  /** Reads a CoNLLFile from an iterator over lines.
    *
    * Assumes that the given lines are taken directly from a CoNLL file.
    * Behavior is undefined if not.
    * See http://conll.cemantix.org/2012/data.html for the data format.
    *
    * @param lines the lines of a CoNLL file
    */
  def readFile(path: CoNLLPath, lines: Iterator[String]): CoNLLFile = {
    val (sentences, _, _) = lines.foldLeft((List.empty[CoNLLSentence], List.empty[String], 0)) {
      case (acc @ (prevSentences, curLines, sentenceNum), line) =>
        if(line.trim.isEmpty) NonEmptyList.fromList(curLines).fold(acc) { neCurLines =>
          (readSentence(path, sentenceNum, neCurLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
        } else if(firstLineRegex.findFirstIn(line).nonEmpty || line.equals(endDocumentLine)) {
          acc
        } else {
          (prevSentences, line :: curLines, sentenceNum)
        }
    }
    CoNLLFile(path, sentences.toVector.reverse)
  }


  // def readSentence(sentencePath: CoNLLSentencePath, lines: List[String]): CoNLLSentence = {
  //   val lineArrays = lines.map(_.split("\\s+"))
  //   val words = lineArrays.map(arr => Word(arr(2).toInt, arr(4), arr(3)))
  //   val treeString = lineArrays.map(arr => arr(5)).mkString
  //   val tree = conll.CoNLLParsing.readSyntaxTree(treeString, words)
  //   val predicates = for {
  //     (arr, index) <- lineArrays.zipWithIndex
  //     predicateLemma = arr(6)
  //     if !predicateLemma.equals("-")
  //     framesetIdString = arr(7)
  //     head = words(index)
  //   } yield Predicate(head, predicateLemma, framesetIdString)
  //   val paStructures = for {
  //     (pred, num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
  //     spansString = lineArrays.map(arr => arr(8 + num)).mkString
  //     argSpans = readArgumentSpans(spansString, words)
  //   } yield PredicateArgumentStructure(pred, argSpans)
  //   PropBankSentence(sentencePath, words, tree, paStructures)
  // }

  // def readFile(path: PropBankPath, lines: Iterator[String]): PropBankFile = {
  //   val (sentences, _, _) = lines
  //     .foldLeft((List.empty[PropBankSentence], List.empty[String], 0)) {
  //       case ((prevSentences, curLines, sentenceNum), line) =>
  //         if(line.trim.isEmpty) {
  //           (readSentence(PropBankSentencePath(path, sentenceNum), curLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
  //         } else {
  //           (prevSentences, line :: curLines, sentenceNum)
  //         }
  //     }
  //   PropBankFile(path, sentences.toVector.reverse)
  // }
}

