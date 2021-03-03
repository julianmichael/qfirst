package qfirst.parsing

import scala.annotation.tailrec

import cats.collections.Heap

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import ops.hlist._

case class CFGProduction[ChildSymbols <: HList, ParentSymbol](
  val childSymbols: ChildSymbols,
  val parentSymbol: ParentSymbol)

case class SyncCFGProduction[ChildSymbols <: HList, Children <: HList, Parent](
  val production: CFGProduction[ChildSymbols, ParseSymbol[Parent]],
  val construct: PartialFunction[Children, ScoredStream[Parent]]) (
  implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]) {
  def childSymbols: ChildSymbols = production.childSymbols
  def parentSymbol: ParseSymbol[Parent] = production.parentSymbol
}

case class SyncCFG[AllProductions <: HList : <<:[SyncCFGProduction[_, _, _]]#λ](productions: AllProductions)

object SyncCFGProductionSyntax {
  import syntax.std.tuple._
  import scala.language.implicitConversions

  case class CFGProductionHelper[ChildSymbols <: HList, ChildrenTuple <: Product, Children <: HList, Parent](
    val children: ChildSymbols,
    val parent: ParseSymbol[Parent])(
    implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children],
    gen: Generic.Aux[ChildrenTuple, Children]
    ) {

    def using(
      construct: PartialFunction[ChildrenTuple, ScoredStream[Parent]])(
    ): SyncCFGProduction[ChildSymbols, Children, Parent] =
      SyncCFGProduction(this, Function.unlift(x => construct.lift(gen.from(x))))

    def usingSingle(construct: PartialFunction[ChildrenTuple, Scored[Parent]]): SyncCFGProduction[ChildSymbols, Children, Parent] =
      SyncCFGProduction(this, Function.unlift(x => construct.lift(gen.from(x)).map(ScoredStream.unit)))
    def usingSingleZ(construct: PartialFunction[ChildrenTuple, Parent]): SyncCFGProduction[ChildSymbols, Children, Parent] =
      SyncCFGProduction(this, Function.unlift(x => construct.lift(gen.from(x)).map(ScoredStream.unit(_))))
  }

  // really only exists for nice syntax
  case class UnaryCFGProductionHelper[Child, Parent](
    val child: ParseSymbol[Child],
    val parent: ParseSymbol[Parent]
  ) {
    def using(construct: PartialFunction[Child, ScoredStream[Parent]]): SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
      SyncCFGProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct)
    def usingSingle(construct: PartialFunction[Child, Scored[Parent]]): SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
      SyncCFGProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct andThen ScoredStream.unit[Parent])
    def usingSingleZ(construct: PartialFunction[Child, Parent]): SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
      SyncCFGProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct andThen ScoredStream.unit[Parent])
  }

  case class NullaryCFGProductionHelper[Parent](
    val parent: ParseSymbol[Parent]
  ) {
    def using(construct: ScoredStream[Parent]): SyncCFGProduction[HNil, HNil, Parent] =
      SyncCFGProduction(CFGProduction(HNil, parent), { case HNil => construct })
    def usingSingle(construct: Scored[Parent]): SyncCFGProduction[HNil, HNil, Parent] =
      SyncCFGProduction(CFGProduction(HNil, parent), { case HNil => ScoredStream.unit(construct) })
    def usingSingleZ(construct: Parent): SyncCFGProduction[HNil, HNil, Parent] =
      SyncCFGProduction(CFGProduction(HNil, parent), { case HNil => ScoredStream.unit(construct) })
  }

  implicit class tuple2Children[
    ChildSymbolsTuple <: Product,
    ChildSymbols <: HList,
    ChildrenTuple <: Product,
    Children <: HList](
    childSymbolsTuple: ChildSymbolsTuple)(
    implicit gen1: Generic.Aux[ChildSymbolsTuple, ChildSymbols],
    comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children],
    tupler: Tupler.Aux[Children, ChildrenTuple],
    gen2: Generic.Aux[ChildrenTuple, Children],
    ) {
    def to[Parent](symb: ParseSymbol[Parent]) = CFGProductionHelper[ChildSymbols, ChildrenTuple, Children, Parent](childSymbolsTuple.productElements, symb)
  }

  implicit class symbol2Child[Child](childSymbol: ParseSymbol[Child]) {
    def to[Parent](symb: ParseSymbol[Parent]) = UnaryCFGProductionHelper[Child, Parent](childSymbol, symb)
  }

  implicit class unit2Null(empty: Unit) {
    def to[Parent](symb: ParseSymbol[Parent]) = NullaryCFGProductionHelper[Parent](symb)
  }

  implicit class TerminalInterpolator(val sc: StringContext) extends AnyVal {
    def t(args: Any*) = Terminal(sc.s(args: _*))
  }

  implicit def cfgHelper2Production[ChildSymbols <: HList, ChildrenTuple <: Product, Children <: HList, Parent](
    helper: CFGProductionHelper[ChildSymbols, ChildrenTuple, Children, Parent]): CFGProduction[ChildSymbols, ParseSymbol[Parent]] =
    CFGProduction(helper.children, helper.parent)

  implicit def unaryCfgHelper2Production[Child, Parent](
    helper: UnaryCFGProductionHelper[Child, Parent]): CFGProduction[ParseSymbol[Child] :: HNil, ParseSymbol[Parent]] =
    CFGProduction(helper.child :: HNil, helper.parent)

  implicit def nullaryCfgHelper2Production[Parent](
    helper: NullaryCFGProductionHelper[Parent]): CFGProduction[HNil, ParseSymbol[Parent]] =
    CFGProduction(HNil, helper.parent)
}
