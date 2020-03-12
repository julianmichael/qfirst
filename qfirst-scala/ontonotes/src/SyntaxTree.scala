package qfirst.ontonotes

import cats.data.NonEmptyList
import cats.implicits._

import jjm.ling.{HasIndex, ESpan}
import jjm.implicits._

/** Represents a syntax tree. */
sealed trait SyntaxTree[Word] {
  final def cata[A](leaf: Word => A)(node: (String, NonEmptyList[A]) => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(label, children.map(_.cata(leaf)(node)))
  }

  final def cataUnlabeled[A](leaf: Word => A)(node: NonEmptyList[A] => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(children.map(_.cataUnlabeled(leaf)(node)))
  }

  // final def foldM[M: Monad](leaf: Word => M[A])(node: (String, List[A]) => M[A]): M[A] = this match {
  //   case SyntaxTreeLeaf(word) => leaf(word)
  //   case SyntaxTreeNode(label, children) =>
  //     children.map(_.foldM(leaf)(node)).sequence.flatMap(node(label, _))
  // }

  // final def foldMUnlabeled[M: Monad](leaf: Word => M[A])(node: List[A] => M[A]): M[A] = this match {
  //   case SyntaxTreeLeaf(word) => leaf(word)
  //   case SyntaxTreeNode(label, children) =>
  //     children.map(_.foldM(leaf)(node)).sequence.flatMap(node)
  // }

  // which of the following is the better generalization? guess the first... kind of the same
  // first one lets you avoid intermediate structure, second lets you delay msumming to later....
  // but they can obv replicate each other trivially
  // guess the second one obviates the need to map to something right away...
  // final def wordsMonoid[M : Monoid](leaf: Word => M) = foldUnlabeled(leaf)(_.msum)
  // final def wordsList = fold(List(_))(_.flatten)
  final def words = cataUnlabeled(Vector(_))(_.toList.toVector.flatten)

  final def depth = cataUnlabeled(_ => 0)(_.maximum + 1)
  final def beginIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.head)
  final def endIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.last)
  final def toSpan(implicit ev: HasIndex[Word]) = ESpan(beginIndex, endIndex + 1)

  final def toStringMultiline(renderWord: Word => String) =
    cata(renderWord) { case (nodeLabel, subtreeStrings) =>
      val childrenStr = subtreeStrings.map(_.replaceAll("\n", "\n ")).toList.mkString("\n")
      s"$nodeLabel\n$childrenStr"
    }

  // final def toStringMultiline(renderWord: Word => String): String = toStringMultilineAux(0, renderWord)
  // // TODO could do this with state monad lol
  // protected[structure] def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String
}

/** Represents a nonterminal node of a SyntaxTree.
  *
  * @param label the nonterminal symbol of this node
  * @param this node's children
  */
case class SyntaxTreeNode[Word](
  label: String,
  children: NonEmptyList[SyntaxTree[Word]]
) extends SyntaxTree[Word] {
  // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
  //   val indent = " " * indentLevel
  //   val childrenStr = children.map(_.toStringMultilineAux(indentLevel + 1)).mkString("\n")
  //   s"$indent$label\n$childrenStr"
  // }
}

/** Represents a terminal node of a SyntaxTree.
  *
  * @param word the word at this node
  */
case class SyntaxTreeLeaf[Word](
  word: Word
) extends SyntaxTree[Word] {
  // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
  //   val indent = " " * indentLevel
  //   val wordStr = renderWord(word)
  //   s"$indent$wordStr"
  // }

}
