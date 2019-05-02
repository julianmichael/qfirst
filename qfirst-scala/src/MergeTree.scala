package qfirst

import cats.Eval
import cats.Reducible
// import cats.implicits._

import monocle.macros._

import io.circe.generic.JsonCodec

@JsonCodec sealed trait MergeTree[A] {
  def rank: Int
  def loss: Double
  def values: Vector[A]

  import MergeTree._

  def map[B](f: A => B): MergeTree[B] = this match {
    case Leaf(l, a) => Leaf(l, f(a))
    case Merge(r, l, left, right) => Merge(r, l, left.map(f), right.map(f))
  }

  def splitByPredicate(shouldSplit: Merge[A] => Boolean): Vector[MergeTree[A]] = this match {
    case l @ Leaf(_, _) => Vector(l)
    case m @ Merge(r, p, left, right) =>
      if(shouldSplit(m)) {
        left.splitByPredicate(shouldSplit) ++ right.splitByPredicate(shouldSplit)
      } else Vector(m)
  }

  def splitToN(n: Int): Vector[MergeTree[A]] = {
    assert(n >= 1)
    splitByPredicate(_.rank > (rank + 1 - n))
  }
}
object MergeTree {
  @Lenses case class Leaf[A](
    loss: Double, value: A) extends MergeTree[A] {
    def rank: Int = 0
    def values: Vector[A] = Vector(value)
  }
  @Lenses case class Merge[A](
    rank: Int,
    loss: Double,
    left: MergeTree[A],
    right: MergeTree[A]
  ) extends MergeTree[A] {
    def values: Vector[A] = left.values ++ right.values
  }

  // TODO add if needed
  // def leaf[P, A] = GenPrism[MergeTree[P, A], Leaf[P, A]]
  // def merge[P, A] = GenPrism[MergeTree[P, A], Merge[P, A]]
  // def param[P1, P2, A] = PLens[MergeTree[P1, A], MergeTree[P2, A], P1, P2](
  //   _.param)(p2 => mt =>
  //   mt match {
  //     case Leaf(_, value) => Leaf(p2, value)
  //     case Merge(rank, _, left, right) => Merge(rank, p2, left, right)
  //   }
  // )

  implicit val mergeTreeReducible: Reducible[MergeTree] = {
    new Reducible[MergeTree] {
      type F[A] = MergeTree[A]

      def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
        fa match {
          case Leaf(_, value) => f(value)
          case Merge(_, _, left, right) =>
            foldLeft(right, reduceLeftTo(left)(f)(g))(g)
        }
      }

      // TODO make this properly lazy or... ?? not sure if this works exactly as I expect
      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
        fa match {
          case Leaf(_, value) => Eval.later(f(value))
          case Merge(_, _, left, right) =>
            foldRight(left, reduceRightTo(right)(f)(g))(g)
        }
      }

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa match {
        case Leaf(_, value) => f(b, value)
        case Merge(_, _, left, right) =>
          foldLeft(right, foldLeft(left, b)(f))(f)
      }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        fa match {
          case Leaf(_, value) => f(value, lb)
          case Merge(_, _, left, right) =>
            foldRight(left, foldRight(right, lb)(f))(f)
        }
      }
    }
  }
}
