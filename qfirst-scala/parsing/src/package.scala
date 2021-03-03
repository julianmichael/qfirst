package qfirst

package object parsing {

  implicit class ScoredStreamConsInfixConstructors[A](a: Scored[A]) {
    @inline def ::<(ss: => ScoredStream[A]) = new ScoredCons(a, ss)
    @inline def ::<+(ss: ScoredStream[A]) = new ScoredCons(a, ss)
  }

  // equals is NOT done by string---we don't want to cause unwanted collisions
  class ParseSymbol[A](label: String) {
    override def toString = label
  }
  object Terminal {
    private[this] case class Terminal(val token: String) extends ParseSymbol[String](token)
    def apply(token: String): ParseSymbol[String] = new Terminal(token)
  }

  /* One of the main datatypes in the parser; also involved in how we translate a CFG */
  sealed trait Derivation {
    type Result
    val symbol: ParseSymbol[Result]
    val item: Result
  }
  object Derivation {
    private[this] case class DerivationImpl[A](
      override val symbol: ParseSymbol[A],
      override val item: A) extends Derivation {
      override type Result = A
    }

    def apply[A](symbol: ParseSymbol[A], item: A): Derivation = DerivationImpl(symbol, item)
    def unapply(d: Derivation): Some[(ParseSymbol[d.Result], d.Result)] = Some((d.symbol, d.item))
  }

  // sad futile endeavor
  // object CFGParsableAdaptation {
  //   import molt.syntax.cfg.parsable._
  //   import molt.syntax.cfg._
  //   import shapeless.syntax.typeable._

  //   import scala.language.implicitConversions
  //   import scalaz._
  //   import scalaz.std.list._
  //   import scalaz.std.option._
  //   import scalaz.syntax.traverse._

  //   implicit def convSyncProd[
  //     ChildSymbols <: HList,
  //     Children <: HList : Typeable,
  //     Result](
  //     sp: SyncProduction[ChildSymbols, Children, Result])(
  //     implicit ev1: ToList[ChildSymbols, CFGParsable[_]]): (List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Result])) = {
  //     (sp.production._1.toList,
  //      ((c: List[AST[CFGParsable[_]]]) => for {
  //         childrenList <- sp.production._1.toList.zip(c).map {
  //           case (parsable, ast) => parsable.fromAST(ast)
  //         }.sequence
  //         children <- childrenList.cast[Children]
  //         result <- sp.construct.lift(children)
  //       } yield result))
  //   }

  //   example intended use (DOESN't WORK BECAUSE DUMBNESS):
  //   import molt.syntax.agenda._
  //   import molt.syntax.agenda.SyncProductionSyntax._
  //   import shapeless._
  //   val syncProduction = convSyncProd(
  //     (NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) to CFGProductionParser using {
  //       case (head: String) :: "->" :: (children: List[String]) :: HNil => CFGProduction(head, children.map(ASTNormalTag(_)))
  //     })
  // }
}

