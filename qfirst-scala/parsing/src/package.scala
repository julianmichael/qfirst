package qfirst

import scala.annotation.tailrec

import cats.Order
import cats.collections.Heap

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import ops.hlist._

import jjm.DependentMap

/** This is a temporary package for my new approach to agenda-based parsing of CFGs */
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

  final class Chart(length: Int) {
    // just doing a square because I'm lazy. TODO change
    private[this] val cells = Array.fill(length * length){ new Cell }
    private[this] val nullCell = new Cell
    // assume end > begin and they fit in the sentence
    private[this] def cellIndex(begin: Int, end: Int): Int = (begin * length) + end - 1
    def cell(span: Option[(Int, Int)]): Cell = span.fold(nullCell) { case (begin, end) => cells(cellIndex(begin, end)) }
  }

  final class Cell {
    private[this] val map = MutableDependentMap.empty[ParseSymbol, λ[A => Heap[Scored[Derivation { type Result = A }]]]]
    def getDerivations[A](ps: ParseSymbol[A]): Option[Heap[Scored[Derivation { type Result = A }]]] = map.get(ps)
    def getDerivationStream[A](ps: ParseSymbol[A]): Option[ScoredStream[Derivation { type Result = A }]] = map.get(ps).map { heap =>
      ScoredStream.unfold(
        heap, (h: Heap[Scored[Derivation { type Result = A }]]) => h.getMin.map(_ -> h.remove)
      )
    }
    def add(scoredDerivation: Scored[Derivation]): Unit = {
      val heap = map
        .get(scoredDerivation.item.symbol)
        .getOrElse(Heap.empty[Scored[Derivation { type Result = scoredDerivation.item.Result }]])
      map.put(
        scoredDerivation.item.symbol,
        heap.add(scoredDerivation.asInstanceOf[
                   Scored[Derivation { type Result = scoredDerivation.item.Result }]
                 ])
      )
    }
  }

  case class EdgeStream(
    derivationStream: ScoredCons[Derivation],
    span: Option[(Int, Int)],
    heuristic: Double) {
    def priority: Double = derivationStream.head.score + heuristic
  }
  object EdgeStream {
    implicit val order: Order[EdgeStream] = Order.by[EdgeStream, Double](_.priority)
  }

  object AgendaBasedSyncCNFParser {
    def buildFromSyncCFG[AllCFGProductions <: HList : <<:[SyncCFGProduction[_, _, _]]#λ,
                         AllCNFProductions <: HList](
      genlex: String => ScoredStream[Derivation],
      cfg: SyncCFG[AllCFGProductions])(
      implicit fm: FlatMapper.Aux[transformProduction.type, AllCFGProductions, AllCNFProductions],
      folder: RightFolder.Aux[AllCNFProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
    ): AgendaBasedSyncCNFParser = {
      val cnfProductions = SyncCNFGrammar.productionsFromSyncCFG(cfg)
      val combinators = CNFCombinators.fromSyncCNFProductions(cnfProductions)
      new AgendaBasedSyncCNFParser(genlex, combinators)
    }
  }
  class AgendaBasedSyncCNFParser(
    val genlex: String => ScoredStream[Derivation],
    val combinators: CNFCombinators) {

    def parse[A](tokens: Vector[String], rootSymbol: ParseSymbol[A]): ScoredStream[Derivation { type Result = A }] = {
      val chart = new Chart(tokens.size)
      var agenda = Heap.empty[EdgeStream]

      val lexicalDerivStreams = tokens.map(genlex)
      // cache lexical scores for the A* heuristic. Assumes that all lexical items produce at least one node
      // TODO decide on a way to fail gracefully if that isn't the case
      val lexicalScores = lexicalDerivStreams.map(_.headOption.get.score)
      val outsideScores = {
        val iter = for {
          begin <- (0 until tokens.size)
          end <- ((begin + 1) to tokens.size)
        } yield (begin, end) -> (lexicalScores.slice(0, begin).sum + lexicalScores.slice(end, tokens.size).sum)
        iter.toMap
      }
      val totalOutsideScore = lexicalScores.sum
      @inline def outsideScore(span: Option[(Int, Int)]) = span.fold(totalOutsideScore)(outsideScores)

      val allSpans = None +: (0 until tokens.size).flatMap(b => ((b + 1) to tokens.size).map(e => Some((b, e)))).toVector

      def addToAgenda(es: EdgeStream): Unit = {
        agenda = agenda.add(es)
      }

      def addEdgeStream(derivStream: ScoredStream[Derivation], span: Option[(Int, Int)]): Unit =
        derivStream.ifNonEmpty.map(EdgeStream(_, span, outsideScore(span))).foreach(addToAgenda)

      // start the agenda
      (tokens, lexicalDerivStreams, (0 until tokens.size)).zipped.foreach {
        (word, ds, i) => addEdgeStream(ds, Some((i, i + 1)))
      }
      addEdgeStream(combinators.nullary.derivations, None)

      @tailrec
      def nextRootNode: Option[Scored[Derivation { type Result = A }]] = agenda.getMin match {
        case None => None
        case Some(headEdgeStream) => headEdgeStream match {
          case EdgeStream((newSD @ Scored(curDeriv, score)) ::<+ remainingDerivs, span, heuristic) =>
            agenda = agenda.remove
            addEdgeStream(remainingDerivs, span)
            // to avoid `unapply-selector` path non-unification problems
            val symbol = curDeriv.symbol
            val item = curDeriv.item
            chart.cell(span).add(newSD)
            // find matching guys and put them in the agenda

            def leftCombine(leftSpan: Option[(Int, Int)], resultSpan: Option[(Int, Int)]): Unit = for {
              leftCombinators <- combinators.rightThenLeftBinary.get(symbol).toSeq
              leftSymbol <- leftCombinators.keys
              leftCombinator <- leftCombinators.get(leftSymbol)
              leftTargets <- chart.cell(leftSpan).getDerivationStream(leftCombinator.leftSymbol)
            } yield {
              val newDerivations = leftTargets.flatMap(leftCombinator(_, curDeriv)).adjust(score)
              addEdgeStream(newDerivations, resultSpan)
            }

            def rightCombine(rightSpan: Option[(Int, Int)], resultSpan: Option[(Int, Int)]): Unit = for {
              rightCombinators <- combinators.leftThenRightBinary.get(symbol).toSeq
              rightSymbol <- rightCombinators.keys
              rightCombinator <- rightCombinators.get(rightSymbol)
              rightTargets <- chart.cell(rightSpan).getDerivationStream(rightCombinator.rightSymbol)
            } yield {
              val newDerivations = rightTargets.flatMap(rightCombinator(curDeriv, _)).adjust(score)
              addEdgeStream(newDerivations, resultSpan)
            }

            // unary case is same for null and non-null spans
            for {
              unaryCombinator <- combinators.unary.get(symbol)
            } yield {
              val newDerivations = unaryCombinator(curDeriv).adjust(score)
              addEdgeStream(newDerivations, span)
            }

            span match {
              case None =>
                for(curSpan <- allSpans) yield {
                  leftCombine(curSpan, curSpan)
                  rightCombine(curSpan, curSpan)
                }
                nextRootNode
              case Some((begin, end)) =>
                for(newBegin <- 0 to (begin - 1)) yield {
                  leftCombine(Some((newBegin, begin)), Some((newBegin, end)))
                }
                leftCombine(None, span)

                for(newEnd <- (end + 1) to tokens.length) yield {
                  rightCombine(Some((end, newEnd)), Some((begin, newEnd)))
                }
                rightCombine(span, None)

                if(begin == 0 && end == tokens.size && symbol == rootSymbol) {
                  Some(newSD.asInstanceOf[Scored[Derivation { type Result = A }]])
                } else {
                  nextRootNode
                }
            }
        }
      }
      ScoredStream.exhaustively(nextRootNode)
    }
  }

  // WITH TREES!!

  // implicit def edgeASTOrdering[A]: Ordering[(Derivation { type Result = A }, EdgeNode)] =
  //   Ordering.by[(Derivation { type Result = A }, EdgeNode), Double](p => p._2.edge.priority)
  // implicit def edgeNodeOrdering[A]: Ordering[EdgeNode] =
  //   Ordering.by[EdgeNode, Double](_.edge.priority)

  // sealed trait EdgeAST {
  //   def toStringPretty: String = toStringPretty(0)
  //   def toStringPretty(tabs: Int): String = this match {
  //     case EdgeTerminal(token, _, index) => ("\t" * tabs) + token
  //     case EdgeNode(Edge(Derivation(symbol, item, score), _, begin, end), children) =>
  //       ("\t" * tabs) + s"$symbol: $item\n" + children.map(_.toStringPretty(tabs + 1)).mkString("\n")
  //   }
  // }
  // case class EdgeTerminal(
  //   token: String,
  //   index: Int,
  //   heuristic: Double
  // ) extends EdgeAST
  // case class EdgeNode(
  //   edge: Edge,
  //   children: List[EdgeAST]
  // ) extends EdgeAST

  // final class TreeChart(length: Int) {
  //   // just doing a square because I'm lazy. TODO change
  //   private[this] val cells = Array.fill(length * length){ new TreeCell }
  //   // assume end > begin and they fit in the sentence
  //   private[this] def cellIndex(begin: Int, end: Int): Int = (begin * length) + end - 1

  //   def cell(begin: Int, end: Int): TreeCell = cells(cellIndex(begin, end))
  // }

  // final class TreeCell {
  //   private[this] val map = MutableDependentMap.empty[ParseSymbol, λ[A => Heap[(Derivation { type Result = A }, EdgeNode)]]]
  //   def getDerivations[A](ps: ParseSymbol[A]): Option[Heap[(Derivation { type Result = A }, EdgeNode)]] = map.get(ps)
  //   def getDerivationStream[A](ps: ParseSymbol[A]): Option[OrderedStream[(Derivation { type Result = A }, EdgeNode)]] = map.get(ps).map { heap =>
  //     OrderedStream.unfold(heap, (h: Heap[(Derivation { type Result = A}, EdgeNode)]) => h.uncons)
  //   }
  //   def add[A](pair: (Derivation { type Result = A }, EdgeNode)): Unit = {
  //     val heap = map.get(pair._1.symbol).getOrElse(Heap.Empty[(Derivation { type Result = A }, EdgeNode)])
  //     map.put(pair._1.symbol, heap.insert(pair))
  //   }
  // }

  // // TODO: Make one generalized version that encompasses all of the ways you could generalize the initial parser.
  // object AgendaBasedSyncCNFParserWithTrees {
  //   def buildFromSyncCFG[AllCFGProductions <: HList : <<:[SyncCFGProduction[_, _, _]]#λ,
  //                        AllCNFProductions <: HList](
  //     genlex: String => OrderedStream[Derivation],
  //     cfg: SyncCFG[AllCFGProductions])(
  //     implicit fm: FlatMapper.Aux[transformProduction.type, AllCFGProductions, AllCNFProductions],
  //     folder: RightFolder.Aux[AllCNFProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
  //   ): AgendaBasedSyncCNFParserWithTrees = {
  //     val cnfProductions = SyncCNFGrammar.productionsFromSyncCFG(cfg)
  //     val combinators = CNFCombinators.fromSyncCNFProductions(cnfProductions)
  //     new AgendaBasedSyncCNFParserWithTrees(genlex, combinators)
  //   }
  // }
  // class AgendaBasedSyncCNFParserWithTrees(
  //   val genlex: String => OrderedStream[Derivation],
  //   val combinators: CNFCombinators) {

  //   final class ParseProcessWithTrees(val tokens: Vector[String]) {
  //     val chart: TreeChart = new TreeChart(tokens.size)
  //     // immutable agenda means we could easily keep track of the agenda history... could be useful in learning, hypothetically
  //     var agenda: Heap[:<[EdgeNode]] = Heap.Empty[:<[EdgeNode]]
  //     // cache lexical scores for the A* heuristic
  //     // TODO should I just initialize the agenda with lexical stuff? test this---I suspect it'd be slightly worse to do so.
  //     val lexicalDerivStreams = tokens.map(genlex)
  //     val lexicalScores = lexicalDerivStreams.map(_.headOption.get.score)
  //     val outsideScores = {
  //       val iter = for {
  //         begin <- (0 until tokens.size)
  //         end <- (begin to tokens.size)
  //       } yield (begin, end) -> (lexicalScores.slice(0, begin).sum + lexicalScores.slice(end, tokens.size).sum)
  //       iter.toMap
  //     }
  //     @inline def outsideScore(begin: Int, end: Int) = outsideScores((begin, end))

  //     // start the agenda
  //     for ((word, ds, i) <- (tokens, lexicalDerivStreams, (0 until tokens.size)).zipped) yield {
  //       val edgeNodes = ds.mapMonotone(d => EdgeNode(Edge(d, outsideScore(i, i + 1), i, i + 1), List[EdgeAST](EdgeTerminal(word, i, outsideScore(i, i + 1))))).asInstanceOf[:<[EdgeNode]]
  //       agenda = agenda.insert(edgeNodes)
  //     }

  //     def step: Option[EdgeNode]= agenda.uncons map {
  //       case (headStream, newAgenda) => headStream match {
  //         case edgeNode :<+ remainingEdges =>
  //           agenda = remainingEdges.ifNonEmpty match {
  //             case None => newAgenda
  //             case Some(re) => newAgenda.insert(re)
  //           }
  //           val Edge(curDeriv, _, begin, end) = edgeNode.edge
  //           val symbol = curDeriv.symbol
  //           val item = curDeriv.item
  //           val score = curDeriv.score
  //           chart.cell(begin, end).add((curDeriv: Derivation { type Result = curDeriv.Result }, edgeNode))

  //           for {
  //             leftCombinators <- combinators.rightThenLeftBinary.get(symbol).toSeq
  //             newBegin <- 0 to (begin - 1)
  //             cell = chart.cell(newBegin, begin)
  //             leftSymbol <- leftCombinators.keys
  //             leftCombinator <- leftCombinators.get(leftSymbol)
  //             leftTargets <- cell.getDerivationStream(leftCombinator.leftSymbol)
  //             newEdgeASTs = leftTargets.flatMap {
  //               case (leftDeriv, leftEdgeAST) =>
  //                 val derivStream = leftCombinator(leftDeriv, curDeriv)
  //                 derivStream.mapMonotone(deriv => EdgeNode(Edge(deriv, outsideScore(newBegin, end), newBegin, end), List(leftEdgeAST, edgeNode)))
  //             }
  //             nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
  //           } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

  //           for {
  //             rightCombinators <- combinators.leftThenRightBinary.get(symbol).toSeq
  //             newEnd <- (end + 1) to tokens.length
  //             cell = chart.cell(end, newEnd)
  //             rightSymbol <- rightCombinators.keys
  //             rightCombinator <- rightCombinators.get(rightSymbol)
  //             rightTargets <- cell.getDerivationStream(rightCombinator.rightSymbol)
  //             newEdgeASTs = rightTargets.flatMap {
  //               case (rightDeriv, rightEdgeAST) =>
  //                 val derivStream = rightCombinator(curDeriv, rightDeriv)
  //                 derivStream.mapMonotone(deriv => EdgeNode(Edge(deriv, outsideScore(begin, newEnd), begin, newEnd), List(edgeNode, rightEdgeAST)))
  //             }
  //             nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
  //           } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

  //           for {
  //             unaryCombinator <- combinators.unary.get(symbol).toSeq
  //             newEdgeASTs = unaryCombinator(curDeriv).mapMonotone(deriv => EdgeNode(Edge(deriv, outsideScore(begin, end), begin, end), List(edgeNode)))
  //             nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
  //           } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

  //           edgeNode
  //       }
  //     }

  //     def evalNode[A](begin: Int, end: Int): Option[(Derivation, EdgeAST)] = {
  //       var next: Option[EdgeNode] = None
  //       var done = false
  //       do {
  //         next = step
  //         done = next.map(n => n.edge.begin == begin && n.edge.end == end).getOrElse(true)
  //       } while(!done)

  //       next.map(n => (n.edge.derivation, n))
  //     }

  //     def evalNode[A](symbol: ParseSymbol[A], begin: Int, end: Int): Option[(Derivation { type Result = A }, EdgeNode)] = {
  //       var next: Option[EdgeNode] = None
  //       var done = false
  //       do {
  //         next = step
  //         done = next.map(n => n.edge.derivation.symbol == symbol && n.edge.begin == begin && n.edge.end == end).getOrElse(true)
  //       } while(!done)

  //       next.map(n => (n.edge.derivation.asInstanceOf[Derivation { type Result = A }], n))
  //     }

  //     def evalRoot[A](symbol: ParseSymbol[A]): Option[(Derivation { type Result = A }, EdgeNode)] =
  //       evalNode(symbol, 0, tokens.size)
  //   }

  //   def parseProcess(tokens: Vector[String]): ParseProcessWithTrees =
  //     new ParseProcessWithTrees(tokens)

  //   def parse[A](
  //     tokens: Vector[String],
  //     rootSymbol: ParseSymbol[A]
  //   ): OrderedStream[(Derivation { type Result = A }, EdgeNode)] = {
  //     val process = parseProcess(tokens)
  //     OrderedStream.exhaustively(process.evalRoot(rootSymbol))
  //   }
  // }

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

