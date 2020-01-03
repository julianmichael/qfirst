package freelog

import cats.Functor
import cats.Monoid
import cats.implicits._

sealed trait LogTree[Label] {
  def labelOption: Option[Label]
  def children: List[LogTree[Label]]
  def cata[A](labeled: (Label, List[A]) => A, unlabeled: List[A] => A): A = this match {
    case LogTree.Labeled(label, children) => labeled(label, children.map(_.cata(labeled)))
    case LogTree.Unlabeled(children) => unlabeled(children.map(_.cata(labeled)))
  }
}

object LogTree {
  case class Labeled[Label](
    label: Label, val children: List[LogTree.Labeled[Label]]
  ) extends LogTree[Label] {
    def labelOption: Option[Label] = Some(label)
    def cata[A](f: (Label, List[A]) => A): A = f(label, children.map(_.cata(f)))
  }
  object Labeled {
    implicit val logTreeLabeledFunctor = new Functor[LogTree.Labeled] {
      def map[A, B](fa: LogTree.Labeled[A])(f: A => B): LogTree.Labeled[B] =
        Labeled(f(fa.label), fa.children.map(map(_)(f)))
    }
  }
  case class Unlabeled[Label](
    val children: List[LogTree.Labeled[Label]]
  ) extends LogTree[Label] {
    def labelOption: Option[Label] = None
  }
  object Unlabeled {
    implicit val logTreeUnlabeledFunctor = new Functor[LogTree.Unlabeled] {
      def map[A, B](fa: LogTree.Unlabeled[A])(f: A => B): LogTree.Unlabeled[B] =
        Unlabeled(fa.children.map(_.map(f)))
    }
  }

  def empty[Label]: LogTree[Label] = Unlabeled[Label](Nil)
  def labeled[Label](label: Label, children: List[LogTree.Labeled[Label]]): LogTree[Label] =
    Labeled(label, children)
  def unlabeled[Label](children: List[LogTree.Labeled[Label]]): LogTree[Label] =
    Unlabeled(children)

  implicit val logTreeFunctor = new Functor[LogTree] {
    def map[A, B](fa: LogTree[A])(f: A => B): LogTree[B] = fa match {
      case Labeled(label, children) => Labeled(f(label), children.map(_.map(f)))
      case Unlabeled(children) => Unlabeled(children.map(_.map(f)))
    }
  }

  implicit def logTreeMonoid[Label] = new Monoid[LogTree[Label]] {
    def empty: LogTree[Label] = LogTree.empty[Label]
    def combine(x: LogTree[Label], y: LogTree[Label]): LogTree[Label] = (x, y) match {
      case (Unlabeled(Nil), r) => r
      case (l, Unlabeled(Nil)) => l
      case (Unlabeled(ls), Unlabeled(rs)) => Unlabeled(ls ++ rs)
      case (l @ Labeled(_, _), Unlabeled(rs)) => Unlabeled(l :: rs)
      case (Unlabeled(ls), r @ Labeled(_, _)) => Unlabeled(ls ++ List(r))
      case (l @ Labeled(_, _), r @ Labeled(_, _)) => Unlabeled(l :: r :: Nil)
    }
  }
}
