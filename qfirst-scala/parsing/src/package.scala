package qfirst

package object parsing {

  type EvaluationBlock = EvaluationBlock.type

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
    val tree: SyntaxTree[ParseSymbol[_], String]

    def treeGloss = SyntaxTree.gloss[ParseSymbol[_], String](
      tree, _.toString, _.toString
    )
  }
  object Derivation {
    private[this] case class DerivationImpl[A](
      override val symbol: ParseSymbol[A],
      override val item: A,
      override val tree: SyntaxTree[ParseSymbol[_], String]
    ) extends Derivation {
      override type Result = A
    }

    def apply[A](
      symbol: ParseSymbol[A],
      item: A,
      tree: SyntaxTree[ParseSymbol[_], String]
    ): Derivation = DerivationImpl(symbol, item, tree)

    def apply[A](
      symbol: ParseSymbol[A],
      item: A,
      leaf: String
    ): Derivation = DerivationImpl(
      symbol, item, SyntaxTree.node(symbol, Vector(SyntaxTree.leaf(leaf)))
    )

    def unapply(d: Derivation): Some[(ParseSymbol[d.Result], d.Result)] = Some((d.symbol, d.item))
  }

// package qfirst.cafe

  import cats.Apply
  import cats.Applicative
  import cats.Eval
  import cats.Monad
  import cats.Traverse
  import cats.implicits._

  import io.circe.generic.JsonCodec

  import monocle.macros._

  import jjm.ling.ESpan
  import jjm.ling.HasIndex
  import jjm.implicits._

  /** Represents a syntax tree. */
  sealed trait SyntaxTree[+Label, Word] {
    import SyntaxTree.{Node, Leaf, Branch, leaf, node}
    final def cata[A](leaf: Word => A)(node: (Label, Vector[A]) => A): A = this match {
      case Leaf(word) => leaf(word)
      case Node(label, children) => node(label, children.map(_.cata(leaf)(node)))
    }

    final def cataUnlabeled[A](leaf: Word => A)(node: Vector[A] => A): A = this match {
      case Leaf(word) => leaf(word)
      case Node(label, children) => node(children.map(_.cataUnlabeled(leaf)(node)))
    }

    final def cataM[M[_]: Monad, A](leaf: Word => M[A])(node: (Label, Vector[A]) => M[A]): M[A] = this match {
      case Leaf(word) => leaf(word)
      case Node(label, children) =>
        children.traverse(_.cataM(leaf)(node)).flatMap(node(label, _))
    }

    final def cataUnlabeledM[M[_]: Monad, A](leaf: Word => M[A])(node: Vector[A] => M[A]): M[A] = this match {
      case Leaf(word) => leaf(word)
      case Node(label, children) =>
        children.traverse(_.cataUnlabeledM(leaf)(node)).flatMap(node)
    }

    final def mapLabels[B](f: Label => B) = cata[SyntaxTree[B, Word]](leaf(_))((l, c) => node(f(l), c))
    final def size = cataUnlabeled(_ => 1)(_.combineAll)
    final def depth = cataUnlabeled(_ => 0)(_.maximumOption.fold(1)(_ + 1))
    final def beginIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.head)
    final def endIndex(implicit ev: HasIndex[Word]) = cataUnlabeled(_.index)(_.last)
    final def toSpan(implicit ev: HasIndex[Word]) = ESpan(beginIndex, endIndex + 1)

    // final def getSubtree(branch: Branch)(
    //   implicit ev: HasIndex[Word]
    // ): SyntaxTree[Label, Word] = {
    //   // System.err.println(s"Getting subtree: ${branch}")
    //   // System.err.println(s"Tree: ${this}")
    //   getSubtreeAux(branch, Nil).getOrElse(
    //     throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    //   )
    // }

    // final def getSubtreeAux(
    //   branch: Branch, ancestors: List[SyntaxTree[Label, Word]])(
    //   implicit ev: HasIndex[Word]
    // ): Option[SyntaxTree[Label, Word]] = this match {
    //   case Leaf(token) =>
    //     if(token.index == branch.tokenIndex) Some {
    //       (this :: ancestors).lift(branch.constituentHeight).getOrElse(
    //         throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    //       )
    //     } else None
    //   case Node(_, children) =>
    //     children.toList.flatMap(_.getSubtreeAux(branch, this :: ancestors)).headOption
    // }

    // def getHighestUnaryBranch(p: Word => Boolean): Option[SyntaxTree[Label, Word]] =
    //   getHighestUnaryBranchAux(p, None)
    // def getHighestUnaryBranchAux(
    //   p: Word => Boolean, topAncestor: Option[SyntaxTree[Label, Word]]
    // ): Option[SyntaxTree[Label, Word]] = this match {
    //   case Leaf(token) => if(p(token)) topAncestor.orElse(Some(Leaf(token))) else None
    //   case Node(_, children) =>
    //     // if >1 child, we're no longer valid top of branch.
    //     // if <= 1 child, our top ancestor remains valid. if there is none, we're it.
    //     val ancestor = if(children.size > 1) None else topAncestor.orElse(Some(this))
    //     children.toList.flatMap(_.getHighestUnaryBranchAux(p, ancestor)).headOption
    // }

    // levels: non-empty list of unexplored siblings. includes `this` as head of first one.
    // @annotation.tailrec
    // final def getSubtreeAux(
    //   branch: Branch, levels: Vector[Vector[SyntaxTree[Word]]])(
    //   implicit ev: HasIndex[Word]
    // ): SyntaxTree[Word] = {
    //   System.err.println(s"$this\n.getSubtreeAux($branch, $levels)")
    //   this match {
    //     case Leaf(token) =>
    //       if(token.index == branch.tokenIndex) {
    //         levels.get(branch.constituentHeight).fold(
    //           throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    //         )(_.head)
    //       } else levels.head.tail match {
    //         // not done with this set of siblings yet
    //         case next :: remSiblings =>
    //           next.getSubtreeAux(branch, Vector(Vector(next, remSiblings), levels.tail))
    //         // done with this set of children, move up in the tree
    //         case Nil => levels.tail match {
    //           case nextLevel :: remLevels =>
    //             Vector.fromList(nextLevel.tail) match {
    //               // move on to parent's next sibling
    //               case Some(parentRemainingSiblings) =>
    //                 parentRemainingSiblings.head.getSubtreeAux(branch, Vector(parentRemainingSiblings, remLevels))
    //               // or parent's parent's next sibling... etc.?
    //               case None => Vector.fromList(remLevels) match {
    //                 case Some(neRemLevels) =>
    //                   neRemLevels.head.head.getSubtreeAux(branch, neRemLevels)
    //                 case None =>
    //                   throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    //               }
    //             }
    //             // nextLevel.head.getSubtreeAux(branch, Vector(nextLevel, remLevels))
    //           case Nil =>
    //             throw new IllegalArgumentException(s"$branch fell outside of syntax tree (size $size, depth $depth)")
    //         }
    //       }
    //     case Node(_, children) => children.head.getSubtreeAux(branch, Vector(children, levels.toList))
    //   }
    // }

    final def toStringMultiline(renderLabel: Label => String, renderWord: Word => String) =
      cata(renderWord) { case (nodeLabel, subtreeStrings) =>
        val childrenStr = subtreeStrings.map(_.replaceAll("\n", "\n ")).toList.mkString("\n")
        s"${renderLabel(nodeLabel)}\n$childrenStr"
      }

    // final def toStringMultiline(renderWord: Word => String): String = toStringMultilineAux(0, renderWord)
    // // TODO could do this with state monad lol
    // protected[structure] def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String

    final def toVector = cataUnlabeled(Vector(_))(_.toList.toVector.flatten)
  }

  object SyntaxTree {
    /** Represents a nonterminal node of a SyntaxTree.
      *
      * @param label the nonterminal symbol of this node
      * @param this node's children
      */
    case class Node[Label, Word](
      label: Label,
      children: Vector[SyntaxTree[Label, Word]]
    ) extends SyntaxTree[Label, Word] {
      // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
      //   val indent = " " * indentLevel
      //   val childrenStr = children.map(_.toStringMultilineAux(indentLevel + 1)).mkString("\n")
      //   s"$indent$label\n$childrenStr"
      // }
    }
    object Node

    def node[Label, Word](
      label: Label,
      children: Vector[SyntaxTree[Label, Word]]
    ): SyntaxTree[Label, Word] = Node(label, children)

    /** Represents a terminal node of a SyntaxTree.
      *
      * @param word the word at this node
      */
    @JsonCodec case class Leaf[Word](
      word: Word
    ) extends SyntaxTree[Nothing, Word] {
      // override def toStringMultilineAux(indentLevel: Int, renderWord: Word => String): String = {
      //   val indent = " " * indentLevel
      //   val wordStr = renderWord(word)
      //   s"$indent$wordStr"
      // }
    }
    object Leaf

    def leaf[Word](
      word: Word
    ): SyntaxTree[Nothing, Word] =
      Leaf(word)

    @Lenses case class Branch(
      tokenIndex: Int,
      constituentHeight: Int)

    implicit def syntaxTreeInstances[Label] =
      new Traverse[SyntaxTree[Label, *]]
        with Monad[SyntaxTree[Label, *]] {
      def traverse[G[_]: Applicative, A, B](fa: SyntaxTree[Label, A])(f: A => G[B]): G[SyntaxTree[Label, B]] = fa match {
        case Leaf(a) => f(a).map(Leaf(_))
        case Node(label, children) => children.traverse(traverse(_)(f)).map(Node(label, _))
      }

      // def reduceLeftTo[A, B](fa: SyntaxTree[Label, A])(f: A => B)(g: (B, A) => B): B = fa match {
      //   case Leaf(a) => f(a)
      //   case Node(label, children) => children.reduceLeftTo(reduceLeftTo(_)(f)(g))((b, c) => foldLeft(c, b)(g))
      // }

      // def reduceRightTo[A, B](fa: SyntaxTree[Label, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      //   case Leaf(a) => Eval.now(f(a))
      //   case Node(label, children) => children.reduceRightTo(
      //     reduceRightTo(_)(f)(g))(
      //     (c, llb) => llb.map(lb => foldRight(c, lb)(g))
      //   ).flatten
      // }

      def foldLeft[A, B](fa: SyntaxTree[Label, A], b: B)(f: (B, A) => B): B = fa match {
        case Leaf(a) => f(b, a)
        case Node(label, children) => children.foldLeft(b)((b, c) => foldLeft(c, b)(f))
      }

      def foldRight[A, B](fa: SyntaxTree[Label, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case Leaf(a) => f(a, lb)
        case Node(label, children) => children.foldRight(lb)(foldRight(_, _)(f))
      }

      def flatMap[A, B](fa: SyntaxTree[Label, A])(f: A => SyntaxTree[Label, B]): SyntaxTree[Label, B] = fa match {
        case Leaf(a) => f(a)
        case Node(label, children) => Node(label, children.map(flatMap(_)(f)))
      }

      // TODO: not stack safe. shouldn't be an issue bc of typical syntax tree size though.
      def tailRecM[A, B](a: A)(f: A => SyntaxTree[Label, Either[A, B]]): SyntaxTree[Label, B] = {
        flatMap(f(a)) {
          case Right(b) => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }
      }

      // @annotation.tailrec
      // def tailRecM[A, B](a: A)(f: A => SyntaxTree[Label, Either[A, B]]): SyntaxTree[Label, B] = f(a) match {
      //   case Leaf(Left(a)) => tailRefM(a)(f)
      //   case Leaf(Right(b)) => Leaf(b)
      //   case Node(label, children) =>
      // }

      def pure[A](x: A): SyntaxTree[Label, A] = Leaf(x)
    }

    private[SyntaxTree] case class PrintStats[+B](width: Int, depth: Int, value: B)

    private[this] def glossyStringStats[L, V](tree: SyntaxTree[L, V])(
      renderLabel: L => String, renderValue: V => String
    ): SyntaxTree[PrintStats[L], PrintStats[V]] = tree match {
      case Leaf(value) => Leaf(PrintStats(renderValue(value).length + 1, 0, value))
      case Node(label, children) =>
        val newChildren = children.map(glossyStringStats(_)(renderLabel, renderValue))
        val labelWidth = renderLabel(label).length
        val childrenWidth = newChildren.foldMap {
          case Leaf(PrintStats(width, _, _)) => width
          case Node(PrintStats(width, _, _), _) => width
        } + math.max(0, 3 * (newChildren.size - 1))
        val width = math.max(childrenWidth, labelWidth)
        val depth = newChildren.map {
          case Leaf(PrintStats(_, depth, _)) => depth
          case Node(PrintStats(_, depth, _), _) => depth
        }.maximumOption.fold(1)(_ + 1)
        Node(PrintStats(width, depth, label), newChildren)
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
    private[this] def glossyLayers[L, V](tree: SyntaxTree[PrintStats[L], PrintStats[V]])(
      width: Int, depth: Int, renderLabel: L => String, renderValue: V => String
    ): Vector[String] = tree match {
      case Leaf(PrintStats(leafWidth, _, value)) =>
        val leafStr = stringToWidth(renderValue(value), width)
        val fullSpace = " " * width
        leafStr +: Vector.fill(depth)(fullSpace)
      case Node(PrintStats(_, _, label), children) =>
        val childrenWidth = children.map {
          case Leaf(PrintStats(width, _, _)) => width
          case Node(PrintStats(width, _, _), _) => width
        }.sum + math.max(0, 3 * (children.size - 1))
        val extraWidth = width - childrenWidth
        val extraWidthForAllChildren = extraWidth / math.max(1, children.size)
        val numChildrenWithOneExtraSpace = extraWidth % math.max(1, children.size)
        val childrenStrings = if(children.size == 0) {
          val fullSpace = " " * width
          Vector.fill(depth)(Vector(fullSpace))
        } else children.zipWithIndex.map {
          case (child, index) =>
            val childWidth = child match {
              case Leaf(PrintStats(width, _, _)) => width
              case Node(PrintStats(width, _, _), _) => width
            }
            // (PrintStats(branchWidth, _, label), subtree)
            val adjustedBranchWidth = {
              val extraAdjustment = if(index < numChildrenWithOneExtraSpace) 1 else 0
              childWidth + extraWidthForAllChildren + extraAdjustment
            }
            glossyLayers(child)(adjustedBranchWidth, depth - 1, renderLabel, renderValue)
        }.transpose
        val layerStrings = childrenStrings :+ Vector(stringToWidth(renderLabel(label), width))
        layerStrings.take(1).map(_.mkString("   ")) ++
          layerStrings.drop(1).map(_.mkString(" \u2502 "))
    }

    def gloss[L, V](
      tree: SyntaxTree[L, V], renderLabel: L => String, renderValue: V => String
    ): String = {
      val stats = glossyStringStats(tree)(renderLabel, renderValue)
      val depth = stats.depth
      val width = stats match {
        case Leaf(PrintStats(width, _, _)) => width
        case Node(PrintStats(width, _, _), _) => width
      }
      glossyLayers(stats)(width, depth, renderLabel, renderValue).mkString("\n")
    }

    import cats.Show

    def showGloss[L: Show, V: Show](
      tree: SyntaxTree[L, V]
    ): String = gloss(tree, Show[L].show, Show[V].show)
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

