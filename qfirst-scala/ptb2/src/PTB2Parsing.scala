package qfirst.ptb2

import scala.util.Try

// TODO make imports more explicit
import cats._
import cats.data._
import cats.implicits._

import fs2.Stream
import fs2.Stream.Compiler

object PTB2Parsing {
  private[this] type SentenceState[A] = State[Int, A]

  import fastparse.all._
  private[this] val symbolP: P[String] = P(CharPred(c => !" ()".contains(c)).rep.!)
  // P(CharIn('A' to 'Z', '0' to '9', "-$,.").rep.!)
  private[this] val tokenP: P[String] = P(CharPred(c => !" ()".contains(c)).rep.!)
  private[this] lazy val treeP: P[SentenceState[SyntaxTree[PTB2Token]]] =
    P("(" ~ symbolP ~ " " ~ treeP.rep(1) ~ ")").map {
      case (symbol, childrenState) =>
        for {
          children <- childrenState.toList.sequence
        } yield SyntaxTreeNode(symbol, NonEmptyList.fromList(children).get): SyntaxTree[PTB2Token]
    } | P("(" ~ symbolP ~ " " ~ tokenP ~ ")" ~ " ".?).map {
      case (pos, token) =>
        for {
          index <- State.get
          _     <- State.set(index + 1)
        } yield SyntaxTreeLeaf(PTB2Token(pos = pos, token = token)): SyntaxTree[PTB2Token]
    }
  private[this] val fullTreeP: P[SyntaxTree[PTB2Token]] =
    P("(" ~ " ".? ~ treeP ~ ")").map(_.runA(0).value)

  def readSyntaxTree(s: String): SyntaxTree[PTB2Token] =
    fullTreeP.parse(s).get.value

  // /** Reads a PTBFile from an iterator over lines.
  //   *
  //   * Assumes that the given lines are taken directly from a PTB file.
  //   * Behavior is undefined if not.
  //   *
  //   * @param lines the lines of a PTB file
  //   */
  // def readFile[F[_]: Foldable](lines: F[String]): PTBFile = {
  //   val (sentences, lastChunk, lastIndex) = lines
  //     .foldLeft((List.empty[PTB2Sentence], List.empty[String], 0)) {
  //       case ((prevSentences, curLines, sentenceNum), line) =>
  //         if (line.isEmpty) {
  //           (prevSentences, curLines, sentenceNum)
  //         } else if (!line.startsWith(" ") && !curLines.isEmpty) {
  //           val tree = readSyntaxTree(curLines.reverse.map(_.dropWhile(_ == ' ')).mkString)
  //           val sentence = PTB2Sentence(sentenceNum, tree.words, tree)
  //           (sentence :: prevSentences, line :: Nil, sentenceNum + 1)
  //         } else {
  //           (prevSentences, line :: curLines, sentenceNum)
  //         }
  //     }
  //   val lastSentence = {
  //     val tree = readSyntaxTree(lastChunk.reverse.map(_.dropWhile(_ == ' ')).mkString)
  //     PTB2Sentence(lastIndex, tree.words, tree)
  //   }
  //   PTB2File((lastSentence :: sentences).toVector.reverse)
  // }

  def readSentence(id: PTB2SentenceId, lines: NonEmptyList[String]): PTB2Sentence = {
    val tree = readSyntaxTree(lines.foldMap(_.dropWhile(_ == ' ')))
    System.err.println(id)
    PTB2Sentence(id, tree.words, tree)
  }

  // Sigh. Had to use this nasty fold I wrote in the past bc there's no super easy way to group the sentences properly.
  // I guess I could try inserting a blank line before each sentence start.
  // But this was already written. Meh
  def readFile[F[_], G[_]: Functor](filePath: PTB2FilePath, lines: Stream[F, String])(implicit c: Compiler[F, G]): G[PTB2File] = {
    lines.compile.fold[(List[PTB2Sentence], List[String], Int)]((Nil, Nil, 0)) { case ((prevSentences, curLines, sentenceNum), line) =>
      if (line.isEmpty) {
        (prevSentences, curLines, sentenceNum)
      } else if (!line.startsWith(" ") && !curLines.isEmpty) {
        val tree = readSyntaxTree(curLines.reverse.map(_.dropWhile(_ == ' ')).mkString)
        val sentence = PTB2Sentence(PTB2SentenceId(filePath, sentenceNum), tree.words, tree)
        (sentence :: prevSentences, line :: Nil, sentenceNum + 1)
      } else {
        (prevSentences, line :: curLines, sentenceNum)
      }
    }.map {
      case (sentences, lastChunk, lastIndex) =>
        val lastSentence = {
          val tree = readSyntaxTree(lastChunk.reverse.map(_.dropWhile(_ == ' ')).mkString)
          PTB2Sentence(PTB2SentenceId(filePath, lastIndex), tree.words, tree)
        }
        PTB2File(filePath, (lastSentence :: sentences).toVector.reverse)
    }
  }
}
