package qfirst.cafe

import cats.Monoid
import cats.MonoidK
import cats.implicits._

import jjm.implicits._
import _root_.cats.Show

sealed trait LabeledTree[+Label, A] {

  import LabeledTree.{Leaf, Node}
  import LabeledTree.{PrintStats}

  def cata[B](
    leaf: A => B)(
    node: Vector[(Label, B)] => B
  ): B = this match {
    case Leaf(a) => leaf(a)
    case Node(bs) => node(bs.mapSecond(_.cata(leaf)(node)))
  }

  def depth: Int = cata(
    _ => 0)(
    _.map(_._2 + 1).maximumOption.getOrElse(1))

}

object LabeledTree {

  private[LabeledTree] case class PrintStats[+B](width: Int, depth: Int, value: B)

  case class Leaf[A](value: A) extends LabeledTree[Nothing, A]

  case class Node[+Label, A](
    branches: Vector[(Label, LabeledTree[Label, A])]
  ) extends LabeledTree[Label, A]
  object Node {
    def apply[Label, A](branches: (Label, LabeledTree[Label, A])*): Node[Label, A] =
      Node(branches.toVector)
  }

  def node[Label, A](branches: (Label, LabeledTree[Label, A])*): Node[Label, A] =
    Node(branches.toVector)

  def leaf[A](value: A): Leaf[A] =
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

  private[this] def glossyStringStats[L, V](tree: LabeledTree[L, V])(
    renderLabel: L => String, renderValue: V => String
  ): LabeledTree[PrintStats[L], PrintStats[V]] = tree match {
    case Leaf(value) => Leaf(PrintStats(renderValue(value).length + 1, 0, value))
    case Node(branches) => Node(
      branches.map { case (label, child) =>
        val labelWidth = renderLabel(label).length
        val newChild = glossyStringStats(child)(renderLabel, renderValue)
        val width = newChild match {
          case Leaf(PrintStats(width, _, _)) =>
            math.max(width, labelWidth)
          case Node(childChildren) =>
            val childrenWidth = childChildren.foldMap(_._1.width) +
              math.max(0, 3 * (childChildren.size - 1))
            math.max(childrenWidth, labelWidth)
        }
        val depth = newChild match {
          case Leaf(PrintStats(_, _, _)) => 1
          case Node(childChildren) =>
            childChildren.map(_._1.depth + 1).maximumOption.getOrElse(1)
        }
        PrintStats(width, depth, label) -> newChild
      }
    )
  }

  private[this] def stringToWidth(x: String, width: Int) = {
    require(width >= x.length)
    val extraSpace = width - x.length
    val rightSpace = extraSpace / 2
    val leftSpace = extraSpace - rightSpace
    (" " * leftSpace) + x + (" " * rightSpace)
  }

  // returns a Vector of [depth+1] strings, each of length [width],
  // to be stacked vertically in order.
  private[this] def glossyLayers[L, V](tree: LabeledTree[PrintStats[L], PrintStats[V]])(
    width: Int, depth: Int, renderLabel: L => String, renderValue: V => String
  ): Vector[String] = tree match {
    case Leaf(PrintStats(leafWidth, _, value)) =>
      val leafStr = stringToWidth(renderValue(value), width)
      val fullSpace = " " * width
      leafStr +: Vector.fill(depth)(fullSpace)
    case Node(children) =>
      val filledWidth = children.map(_._1.width).sum + math.max(0, 3 * (children.size - 1))
      val extraWidth = width - filledWidth
      val extraWidthForAllChildren = extraWidth / children.size
      val numChildrenWithOneExtraSpace = extraWidth % children.size
      val childStrings = children.zipWithIndex.map {
        case ((PrintStats(branchWidth, _, label), subtree), index) =>
          val adjustedBranchWidth = {
            val extraAdjustment = if(index < numChildrenWithOneExtraSpace) 1 else 0
            branchWidth + extraWidthForAllChildren + extraAdjustment
          }
          glossyLayers(subtree)(adjustedBranchWidth, depth - 1, renderLabel, renderValue) :+
            stringToWidth(renderLabel(label), adjustedBranchWidth)
      }.transpose
      childStrings.take(1).map(_.mkString("   ")) ++
        childStrings.drop(1).map(_.mkString(" \u2502 "))
  }

  def gloss[L, V](
    tree: LabeledTree[L, V], renderLabel: L => String, renderValue: V => String
  ): String = {
    val stats = glossyStringStats(tree)(renderLabel, renderValue)
    val depth = stats.depth
    val width = stats match {
      case Leaf(PrintStats(width, _, _)) => width
      case Node(childChildren) =>
        childChildren.foldMap(_._1.width) +
          math.max(0, 3 * (childChildren.size - 1))
    }
    glossyLayers(stats)(width, depth, renderLabel, renderValue).mkString("\n")
  }

  def showGloss[L: Show, V: Show](
    tree: LabeledTree[L, V]
  ): String = gloss(tree, Show[L].show, Show[V].show)
}
