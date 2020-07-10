package qfirst.datasets


import cats.Apply
import cats.Applicative
import cats.Eval
import cats.Monad
import cats.NonEmptyTraverse
import cats.data.NonEmptyList
import cats.implicits._

import io.circe.generic.JsonCodec

import jjm.ling.{HasIndex, ESpan}
import jjm.implicits._

/** Represents a syntax tree. */
@JsonCodec sealed trait SyntaxTree[Word] {
  import SyntaxTree.{Node, Leaf}
  final def cata[A](leaf: Word => A)(node: (String, NonEmptyList[A]) => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(label, children.map(_.cata(leaf)(node)))
  }

  final def cataUnlabeled[A](leaf: Word => A)(node: NonEmptyList[A] => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(children.map(_.cataUnlabeled(leaf)(node)))
  }

  final def cataM[M[_]: Monad, A](leaf: Word => M[A])(node: (String, NonEmptyList[A]) => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataM(leaf)(node)).flatMap(node(label, _))
  }

  final def cataUnlabeledM[M[_]: Monad, A](leaf: Word => M[A])(node: NonEmptyList[A] => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataUnlabeledM(leaf)(node)).flatMap(node)
  }

  final def depth = cataUnlabeled(_ => 0)(_.maximum + 1)
  final def beginIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.head)
  final def endIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.last)
  final def toSpan(implicit ev: HasIndex[Word]) = ESpan(beginIndex, endIndex + 1)

  // final def getSubtree(branch: SyntaxTreeBranch): SyntaxTree[Word]

  final def toStringMultiline(renderWord: Word => String) =
    cata(renderWord) { case (nodeLabel, subtreeStrings) =>
      val childrenStr = subtreeStrings.map(_.replaceAll("\n", "\n ")).toList.mkString("\n")
      s"$nodeLabel\n$childrenStr"
    }

  // final def toStringMultiline(renderWord: Word => String): String = toStringMultilineAux(0, renderWord)
  // // TODO could do this with state monad lol
  // protected[structure] def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String

  final def toVector = cataUnlabeled(Vector(_))(_.toList.toVector.flatten)
}

object SyntaxTree {
  /** Represents a nonterminal node of a SyntaxTree.
    *
    * @param label the nonterminal symbol of this node
    * @param this node's children
    */
  @JsonCodec case class Node[Word](
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
  @JsonCodec case class Leaf[Word](
    word: Word
  ) extends SyntaxTree[Word] {
    // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
    //   val indent = " " * indentLevel
    //   val wordStr = renderWord(word)
    //   s"$indent$wordStr"
    // }
  }

  implicit val syntaxTreeInstances = new NonEmptyTraverse[SyntaxTree] with Monad[SyntaxTree] {
    def nonEmptyTraverse[G[_]: Apply, A, B](fa: SyntaxTree[A])(f: A => G[B]): G[SyntaxTree[B]] = fa match {
      case Leaf(a) => f(a).map(Leaf(_))
      case Node(label, children) => children.nonEmptyTraverse(nonEmptyTraverse(_)(f)).map(Node(label, _))
    }

    def reduceLeftTo[A, B](fa: SyntaxTree[A])(f: A => B)(g: (B, A) => B): B = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => children.reduceLeftTo(reduceLeftTo(_)(f)(g))((b, c) => foldLeft(c, b)(g))
    }

    def reduceRightTo[A, B](fa: SyntaxTree[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => Eval.now(f(a))
      case Node(label, children) => children.reduceRightTo(
        reduceRightTo(_)(f)(g))(
        (c, llb) => llb.map(lb => foldRight(c, lb)(g))
      ).flatten
    }

    def foldLeft[A, B](fa: SyntaxTree[A], b: B)(f: (B, A) => B): B = fa match {
      case Leaf(a) => f(b, a)
      case Node(label, children) => children.foldLeft(b)((b, c) => foldLeft(c, b)(f))
    }

    def foldRight[A, B](fa: SyntaxTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => f(a, lb)
      case Node(label, children) => children.foldRight(lb)(foldRight(_, _)(f))
    }

    def flatMap[A, B](fa: SyntaxTree[A])(f: A => SyntaxTree[B]): SyntaxTree[B] = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => Node(label, children.map(flatMap(_)(f)))
    }

    // TODO: not stack safe. shouldn't be an issue bc of typical syntax tree size though.
    def tailRecM[A, B](a: A)(f: A => SyntaxTree[Either[A, B]]): SyntaxTree[B] = {
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }
    }

    // @annotation.tailrec
    // def tailRecM[A, B](a: A)(f: A => SyntaxTree[Either[A, B]]): SyntaxTree[B] = f(a) match {
    //   case Leaf(Left(a)) => tailRefM(a)(f)
    //   case Leaf(Right(b)) => Leaf(b)
    //   case Node(label, children) =>
    // }

    def pure[A](x: A): SyntaxTree[A] = Leaf(x)
  }
}
