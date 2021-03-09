package qfirst.parsing

import scala.annotation.tailrec

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import ops.hlist._

import cats.collections.Heap
import cats.kernel.Order

object EvaluationBlock

object AgendaBasedSyncCNFParser {
  def buildFromSyncCFG[
    Token,
    AllCFGProductions <: HList : <<:[SyncCFGProduction[_, _, _]]#λ,
    AllCNFProductions <: HList](
    genlex: Token => ScoredStream[Derivation],
    cfg: SyncCFG[AllCFGProductions])(
    implicit fm: FlatMapper.Aux[transformProduction.type, AllCFGProductions, AllCNFProductions],
    folder: RightFolder.Aux[AllCNFProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
  ): AgendaBasedSyncCNFParser[Token] = {
    val cnfProductions = SyncCNFGrammar.productionsFromSyncCFG(cfg)
    val combinators = CNFCombinators.fromSyncCNFProductions(cnfProductions)
    new AgendaBasedSyncCNFParser(genlex, combinators)
  }
}
class AgendaBasedSyncCNFParser[Token](
  val genlex: Token => ScoredStream[Derivation],
  val combinators: CNFCombinators) {

  def parse[A](
    tokens: Vector[Token],
    rootSymbol: ParseSymbol[A]
  ): ScoredStream[Derivation { type Result = A }] = {
    parseWithBlocks(tokens, rootSymbol).collect { case Right(deriv) => Scored(deriv, 0.0) }
  }

  def parseWithBlocks[A](
    tokens: Vector[Token],
    rootSymbol: ParseSymbol[A]
  ) = {
    parse(
      tokens, rootSymbol,
      ScoredStream.recurrence(
        Scored(EvaluationBlock, 0.0), (x: EvaluationBlock.type) => Scored(x, 10.0)
      )
    )
  }

  def parse[A](
    tokens: Vector[Token],
    rootSymbol: ParseSymbol[A],
    blocks: ScoredStream[EvaluationBlock.type]
  ): ScoredStream[Either[EvaluationBlock.type, Derivation { type Result = A }]] = {
    val chart = new Chart(tokens.size)
    println(chart)
    var agenda = Heap.empty[EdgeStream]
    println(agenda)
    var evalBlocks = blocks
    println(evalBlocks)

    val lexicalDerivStreams = tokens.map(genlex)
    println(lexicalDerivStreams)
    // cache lexical scores for the A* heuristic.
    // if some lexical item is produced by no nodes, we can immediately return the empty stream.
    // (TODO.)
    // lexicalDerivStreams.map(_.headOption).sequence
    val lexicalScores = lexicalDerivStreams.map(_.headOption.get.score)
    println(lexicalScores)
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
    def nextRootNode: Option[Scored[Either[EvaluationBlock.type, Derivation { type Result = A }]]] = agenda.getMin match {
      case None => None
      case Some(headEdgeStream) => headEdgeStream match {
        case EdgeStream((newSD @ Scored(curDeriv, score)) ::<+ remainingDerivs, span, heuristic) =>
          Thread.sleep(100)
          println("===== ===== ===== =====")
          println(newSD)
          println(span)
          println(evalBlocks.headOption)
          if(evalBlocks.headOption.exists(_.score < score)) {
            val res = evalBlocks.headOption.get
            evalBlocks = evalBlocks.tailOption.get
            Some(res.map(Left(_)))
          } else {
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
                  Some(
                    newSD.asInstanceOf[Scored[Derivation { type Result = A }]]
                      .map(Right(_))
                  )
                } else {
                  nextRootNode
                }
            }
          }
      }
    }
    ScoredStream.exhaustively(nextRootNode)
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
