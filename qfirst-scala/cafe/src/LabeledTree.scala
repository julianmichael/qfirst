package qfirst.cafe

import cats.Monoid
import cats.MonoidK

import jjm.implicits._

sealed trait LabeledTree[+Label, +A]

object LabeledTree {

  case class Leaf[+A](value: A) extends LabeledTree[Nothing, A]

  case class Node[+Label, +A](
    branches: Vector[(Label, LabeledTree[Label, A])]
  ) extends LabeledTree[Label, A]
  object Node {
    def apply[Label, A](branches: (Label, LabeledTree[Label, A])*): Node[Label, A] =
      Node(branches.toVector)
  }

  def node[Label, A](branches: (Label, LabeledTree[Label, A])*): LabeledTree[Label, A] =
    Node(branches.toVector)

  def leaf[A](value: A): LabeledTree[Nothing, A] =
    Leaf(value)

  def leaves[Label, A](leaves: (Label, A)*): Node[Label, A] =
    Node(leaves.toVector.mapSecond(leaf[A](_)))

  implicit def labeledTreeNodeMonoidK[Label]: MonoidK[Node[Label, *]] =
    new MonoidK[Node[Label, *]] {
      def empty[A]: Node[Label, A] = Node[Label, A](Vector())
      def combineK[A](x: Node[Label, A], y: Node[Label, A]): Node[Label, A] =
        Node(x.branches ++ y.branches)
    }
  implicit def labeledTreeNodeMonoid[Label, A]: Monoid[Node[Label, A]] =
    labeledTreeNodeMonoidK[Label].algebra[A]
}
