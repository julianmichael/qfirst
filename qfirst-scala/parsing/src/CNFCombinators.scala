package qfirst.parsing

import shapeless._
import ops.hlist._

import jjm.DependentMap

import SyncCNFProduction._

// TODO maybe change combinator type members to type parameters
sealed trait CNFCombinator

case class NullaryCombinator(
  val productions: Vector[Nullary[_]]
) extends CNFCombinator {
  val derivations: ScoredStream[Derivation] =
    productions.map(p => p.construct.map(Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], _))).foldLeft(ScoredStream.empty[Derivation])(_ merge _)
}

sealed trait UnaryCombinator extends CNFCombinator {
  type Child
  def childSymbol: ParseSymbol[Child]
  def productions: Vector[Unary[Child, _]]
  def apply(d: Derivation { type Result = Child }): ScoredStream[Derivation]
}
object UnaryCombinator {
  def apply[C](childSymbol: ParseSymbol[C], productions: Vector[Unary[C, _]]): UnaryCombinator { type Child = C } =
    UnaryCombinatorImpl(childSymbol, productions)

  private[this] case class UnaryCombinatorImpl[C](
    override val childSymbol: ParseSymbol[C],
    override val productions: Vector[Unary[C, _]]
  ) extends UnaryCombinator {
    override type Child = C
    override def apply(d: Derivation { type Result = Child }): ScoredStream[Derivation] = d match {
      case Derivation(`childSymbol`, child) =>
        val vecOfStreams = for {
          p <- productions
          scoredResults <- p.construct.lift(child :: HNil).toVector
        } yield scoredResults.map(Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], _))
        vecOfStreams.foldLeft(ScoredStream.empty[Derivation])(_ merge _)
      case _ => ScoredStream.empty[Derivation]
    }
  }
}

sealed trait BinaryCombinator {
  type Left
  type Right
  def leftSymbol: ParseSymbol[Left]
  def rightSymbol: ParseSymbol[Right]
  def productions: Vector[Binary[Left, Right, _]]
  def apply(left: Derivation { type Result = Left }, right: Derivation { type Result = Right }): ScoredStream[Derivation]
}
object BinaryCombinator {
  def apply[L, R](leftSymbol: ParseSymbol[L], rightSymbol: ParseSymbol[R], productions: Vector[Binary[L, R, _]]): BinaryCombinator { type Left = L; type Right = R } =
    BinaryCombinatorImpl(leftSymbol, rightSymbol, productions)

  private[this] case class BinaryCombinatorImpl[L, R](
    override val leftSymbol: ParseSymbol[L],
    override val rightSymbol: ParseSymbol[R],
    override val productions: Vector[Binary[L, R, _]]
  ) extends BinaryCombinator {
    override type Left = L
    override type Right = R
    override def apply(left: Derivation { type Result = Left }, right: Derivation { type Result = Right }): ScoredStream[Derivation] = (left, right) match {
      case (Derivation(`leftSymbol`, leftChild), Derivation(`rightSymbol`, rightChild)) =>
        val vecOfStreams = for {
          p <- productions
          scoredResults <- p.construct.lift(leftChild :: rightChild :: HNil)
        } yield scoredResults.map(
          // not really sure why we need this cast...sigh...
          result => Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], result))
        vecOfStreams.foldLeft(ScoredStream.empty[Derivation])(_ merge _)
      case _ => ScoredStream.empty[Derivation]
    }
  }
}

object addCNFRule extends Poly2 {
  implicit def caseNullary[Parent] =
    at[Nullary[Parent], CNFCombinators] { case (n @ Nullary(_, _), combinators) =>
      val newProds = n +: combinators.nullary.productions
      val newNullary = combinators.nullary.copy(productions = newProds)
      combinators.copy(nullary = newNullary)
    }
  implicit def caseUnary[Child, Parent] =
    at[Unary[Child, Parent], CNFCombinators] { case (u @ Unary(childSymbol, _, _), combinators) =>
      val curProductions = combinators.unary.get(childSymbol).map(_.productions).getOrElse(Vector.empty[Unary[Child, _]])
      val newUnary = combinators.unary.put(childSymbol, UnaryCombinator(childSymbol, u +: curProductions))
      combinators.copy(unary = newUnary)
    }
  implicit def caseBinary[L, R, Parent] =
    at[Binary[L, R, Parent], CNFCombinators] { case (b @ Binary(leftSymbol, rightSymbol, _, _), combinators) =>
      val rightBinaries = combinators.leftThenRightBinary.get(leftSymbol)
        .getOrElse(DependentMap.empty[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]])
      val leftBinaries = combinators.rightThenLeftBinary.get(rightSymbol)
        .getOrElse(DependentMap.empty[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]])
      // could also do with leftBinaries; doesn't matter since contents are the same (just indexed differently)
      val binaryCombinator = rightBinaries.get(rightSymbol)
        .getOrElse(BinaryCombinator(leftSymbol, rightSymbol, Vector.empty))

      val newLeftBinaries  =  leftBinaries.put(leftSymbol,  BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
      val newRightBinaries = rightBinaries.put(rightSymbol, BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
      val newLeftThenRightBinary = combinators.leftThenRightBinary.put(leftSymbol, newRightBinaries)
      val newRightThenLeftBinary = combinators.rightThenLeftBinary.put(rightSymbol, newLeftBinaries)
      combinators.copy(
        leftThenRightBinary = newLeftThenRightBinary,
        rightThenLeftBinary = newRightThenLeftBinary)
    }
}

case class CNFCombinators(
  val nullary: NullaryCombinator,
  val unary: DependentMap[ParseSymbol, λ[C => UnaryCombinator { type Child = C }]],
  val leftThenRightBinary: DependentMap[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]]]],
  val rightThenLeftBinary: DependentMap[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]]]])
object CNFCombinators {
  val empty = CNFCombinators(
    nullary = NullaryCombinator(Vector()),
    unary = DependentMap.empty[ParseSymbol, λ[C => UnaryCombinator { type Child = C }]],
    leftThenRightBinary = DependentMap.empty[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]]]],
    rightThenLeftBinary = DependentMap.empty[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]]]])
  def fromSyncCNFProductions[AllProductions <: HList](
    cnfProductions: AllProductions)(
    implicit folder: RightFolder.Aux[AllProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
  ): CNFCombinators = {
    cnfProductions.foldRight(empty)(addCNFRule)
  }
}

