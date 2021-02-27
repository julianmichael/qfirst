package qfirst.cafe

import cats.Apply
import cats.Applicative
import cats.Eval
import cats.Monad
import cats.NonEmptyTraverse
import cats.data.NonEmptyList
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.macros._

import jjm.ling.ESpan
import jjm.ling.HasIndex
import jjm.implicits._

/** Represents a syntax tree. */
sealed trait SyntaxTree[+Label, Word] {
  import SyntaxTree.{Node, Leaf, Branch, leaf, node}
  final def cata[A](leaf: Word => A)(node: (Label, NonEmptyList[A]) => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(label, children.map(_.cata(leaf)(node)))
  }

  final def cataUnlabeled[A](leaf: Word => A)(node: NonEmptyList[A] => A): A = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) => node(children.map(_.cataUnlabeled(leaf)(node)))
  }

  final def cataM[M[_]: Monad, A](leaf: Word => M[A])(node: (Label, NonEmptyList[A]) => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataM(leaf)(node)).flatMap(node(label, _))
  }

  final def cataUnlabeledM[M[_]: Monad, A](leaf: Word => M[A])(node: NonEmptyList[A] => M[A]): M[A] = this match {
    case Leaf(word) => leaf(word)
    case Node(label, children) =>
      children.traverse(_.cataUnlabeledM(leaf)(node)).flatMap(node)
  }

  final def mapLabels[B](f: Label => B) = cata[SyntaxTree[B, Word]](leaf(_))((l, c) => node(f(l), c))
  final def size = cataUnlabeled(_ => 1)(_.combineAll)
  final def depth = cataUnlabeled(_ => 0)(_.maximum + 1)
  final def beginIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.head)
  final def endIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.last)
  final def toSpan(implicit ev: HasIndex[Word]) = ESpan(beginIndex, endIndex + 1)

  final def getSubtree(branch: Branch)(
    implicit ev: HasIndex[Word]
  ): SyntaxTree[Label, Word] = {
    // System.err.println(s"Getting subtree: ${branch}")
    // System.err.println(s"Tree: ${this}")
    getSubtreeAux(branch, Nil).getOrElse(
      throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    )
  }

  final def getSubtreeAux(
    branch: Branch, ancestors: List[SyntaxTree[Label, Word]])(
    implicit ev: HasIndex[Word]
  ): Option[SyntaxTree[Label, Word]] = this match {
    case Leaf(token) =>
      if(token.index == branch.tokenIndex) Some {
        (this :: ancestors).lift(branch.constituentHeight).getOrElse(
          throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
        )
      } else None
    case Node(_, children) =>
      children.toList.flatMap(_.getSubtreeAux(branch, this :: ancestors)).headOption
  }

  def getHighestUnaryBranch(p: Word => Boolean): Option[SyntaxTree[Label, Word]] =
    getHighestUnaryBranchAux(p, None)
  def getHighestUnaryBranchAux(
    p: Word => Boolean, topAncestor: Option[SyntaxTree[Label, Word]]
  ): Option[SyntaxTree[Label, Word]] = this match {
    case Leaf(token) => if(p(token)) topAncestor.orElse(Some(Leaf(token))) else None
    case Node(_, children) =>
      // if >1 child, we're no longer valid top of branch.
      // if <= 1 child, our top ancestor remains valid. if there is none, we're it.
      val ancestor = if(children.size > 1) None else topAncestor.orElse(Some(this))
      children.toList.flatMap(_.getHighestUnaryBranchAux(p, ancestor)).headOption
  }

  // levels: non-empty list of unexplored siblings. includes `this` as head of first one.
  // @annotation.tailrec
  // final def getSubtreeAux(
  //   branch: Branch, levels: NonEmptyList[NonEmptyList[SyntaxTree[Word]]])(
  //   implicit ev: HasIndex[Word]
  // ): SyntaxTree[Word] = {
  //   System.err.println(s"$this\n.getSubtreeAux($branch, $levels)")
  //   this match {
  //     case Leaf(token) =>
  //       if(token.index == branch.tokenIndex) {
  //         levels.get(branch.constituentHeight).fold(
  //           throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //         )(_.head)
  //       } else levels.head.tail match {
  //         // not done with this set of siblings yet
  //         case next :: remSiblings =>
  //           next.getSubtreeAux(branch, NonEmptyList(NonEmptyList(next, remSiblings), levels.tail))
  //         // done with this set of children, move up in the tree
  //         case Nil => levels.tail match {
  //           case nextLevel :: remLevels =>
  //             NonEmptyList.fromList(nextLevel.tail) match {
  //               // move on to parent's next sibling
  //               case Some(parentRemainingSiblings) =>
  //                 parentRemainingSiblings.head.getSubtreeAux(branch, NonEmptyList(parentRemainingSiblings, remLevels))
  //               // or parent's parent's next sibling... etc.?
  //               case None => NonEmptyList.fromList(remLevels) match {
  //                 case Some(neRemLevels) =>
  //                   neRemLevels.head.head.getSubtreeAux(branch, neRemLevels)
  //                 case None =>
  //                   throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //               }
  //             }
  //             // nextLevel.head.getSubtreeAux(branch, NonEmptyList(nextLevel, remLevels))
  //           case Nil =>
  //             throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
  //         }
  //       }
  //     case Node(_, children) => children.head.getSubtreeAux(branch, NonEmptyList(children, levels.toList))
  //   }
  // }

  final def toStringMultiline(renderLabel: Label => String, renderWord: Word => String) =
    cata(renderWord) { case (nodeLabel, subtreeStrings) =>
      val childrenStr = subtreeStrings.map(_.replaceAll("\n", "\n ")).toList.mkString("\n")
      s"${renderLabel(nodeLabel)}\n$childrenStr"
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
  case class Node[Label, Word](
    label: Label,
    children: NonEmptyList[SyntaxTree[Label, Word]]
  ) extends SyntaxTree[Label, Word] {
    // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
    //   val indent = " " * indentLevel
    //   val childrenStr = children.map(_.toStringMultilineAux(indentLevel + 1)).mkString("\n")
    //   s"$indent$label\n$childrenStr"
    // }
  }
  object Node

  def node[Label, Word](
    label: Label,
    children: NonEmptyList[SyntaxTree[Label, Word]]
  ): SyntaxTree[Label, Word] = Node(label, children)

  /** Represents a terminal node of a SyntaxTree.
    *
    * @param word the word at this node
    */
  @JsonCodec case class Leaf[Word](
    word: Word
  ) extends SyntaxTree[Nothing, Word] {
    // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
    //   val indent = " " * indentLevel
    //   val wordStr = renderWord(word)
    //   s"$indent$wordStr"
    // }
  }
  object Leaf

  def leaf[Word](
    word: Word
  ): SyntaxTree[Nothing, Word] =
    Leaf(word)

  @Lenses case class Branch(
    tokenIndex: Int,
    constituentHeight: Int)

  implicit def syntaxTreeInstances[Label] =
    new NonEmptyTraverse[SyntaxTree[Label, *]]
      with Monad[SyntaxTree[Label, *]] {
    def nonEmptyTraverse[G[_]: Apply, A, B](fa: SyntaxTree[Label, A])(f: A => G[B]): G[SyntaxTree[Label, B]] = fa match {
      case Leaf(a) => f(a).map(Leaf(_))
      case Node(label, children) => children.nonEmptyTraverse(nonEmptyTraverse(_)(f)).map(Node(label, _))
    }

    def reduceLeftTo[A, B](fa: SyntaxTree[Label, A])(f: A => B)(g: (B, A) => B): B = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => children.reduceLeftTo(reduceLeftTo(_)(f)(g))((b, c) => foldLeft(c, b)(g))
    }

    def reduceRightTo[A, B](fa: SyntaxTree[Label, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => Eval.now(f(a))
      case Node(label, children) => children.reduceRightTo(
        reduceRightTo(_)(f)(g))(
        (c, llb) => llb.map(lb => foldRight(c, lb)(g))
      ).flatten
    }

    def foldLeft[A, B](fa: SyntaxTree[Label, A], b: B)(f: (B, A) => B): B = fa match {
      case Leaf(a) => f(b, a)
      case Node(label, children) => children.foldLeft(b)((b, c) => foldLeft(c, b)(f))
    }

    def foldRight[A, B](fa: SyntaxTree[Label, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Leaf(a) => f(a, lb)
      case Node(label, children) => children.foldRight(lb)(foldRight(_, _)(f))
    }

    def flatMap[A, B](fa: SyntaxTree[Label, A])(f: A => SyntaxTree[Label, B]): SyntaxTree[Label, B] = fa match {
      case Leaf(a) => f(a)
      case Node(label, children) => Node(label, children.map(flatMap(_)(f)))
    }

    // TODO: not stack safe. shouldn't be an issue bc of typical syntax tree size though.
    def tailRecM[A, B](a: A)(f: A => SyntaxTree[Label, Either[A, B]]): SyntaxTree[Label, B] = {
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }
    }

    // @annotation.tailrec
    // def tailRecM[A, B](a: A)(f: A => SyntaxTree[Label, Either[A, B]]): SyntaxTree[Label, B] = f(a) match {
    //   case Leaf(Left(a)) => tailRefM(a)(f)
    //   case Leaf(Right(b)) => Leaf(b)
    //   case Node(label, children) =>
    // }

    def pure[A](x: A): SyntaxTree[Label, A] = Leaf(x)
  }
}
