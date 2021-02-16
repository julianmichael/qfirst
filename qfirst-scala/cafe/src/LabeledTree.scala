package qfirst.cafe

import cats.Monoid
import cats.MonoidK

import jjm.implicits._

sealed trait LabeledTreeChild[+Label, +A]
case class LabeledTree[+Label, +A](
  branches: Vector[(Label, LabeledTreeChild[Label, A])]
) extends LabeledTreeChild[Label, A]
case class LabeledTreeLeaf[+A](value: A) extends LabeledTreeChild[Nothing, A]
object LabeledTree {
  def apply[Label, A](branches: (Label, LabeledTreeChild[Label, A])*): LabeledTree[Label, A] =
    LabeledTree(branches.toVector)

  def leaf[A](value: A) =
    LabeledTreeLeaf(value)

  def leaves[Label, A](leaves: (Label, A)*) =
    LabeledTree(leaves.toVector.mapSecond(leaf[A](_)))

  implicit def labeledTreeMonoidK[Label]: MonoidK[LabeledTree[Label, *]] =
    new MonoidK[LabeledTree[Label, *]] {
      def empty[A]: LabeledTree[Label, A] = LabeledTree[Label, A]()
      def combineK[A](x: LabeledTree[Label, A], y: LabeledTree[Label, A]): LabeledTree[Label, A] =
        LabeledTree(x.branches ++ y.branches)
    }
  implicit def labeledTreeMonoid[Label, A]: Monoid[LabeledTree[Label, A]] =
    labeledTreeMonoidK[Label].algebra[A]
}
