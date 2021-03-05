package qfirst.parsing

import cats.MonoidK
import cats.kernel.Monoid

// TODO FIX SERIOUS PROBLEM:
// if you do a map/flatMap/collect
// where f(a_i) < a_{i + 1} for all i,
// it won't terminate.

sealed trait ScoredStream[A] {

  import ScoredStream._

  def headOption: Option[Scored[A]]
  def headItemOption: Option[A]
  def tailOption: Option[ScoredStream[A]]
  def isEmpty: Boolean
  def ifNonEmpty: Option[ScoredCons[A]]

  def adjust(adjustment: Double): ScoredStream[A]

  def find(p: A => Boolean): Option[Scored[A]]

  def insert(sa: Scored[A]): ScoredStream[A]

  def map[B](f: A => B): ScoredStream[B]
  def mapScored[B](f: A => Scored[B]): ScoredStream[B]
  // TODO scoping
  protected[parsing]def mapScoredAux[B](f: A => Scored[B], candidateHead: Scored[B]): ScoredCons[B]

  def flatMap[B](f: A => ScoredStream[B]): ScoredStream[B]
  protected[parsing]def flatMapAux[B](f: A => ScoredStream[B], candidateHeadStream: ScoredStream[B]): ScoredStream[B]

  def flatten[B](implicit ev: A =:= ScoredStream[B]): ScoredStream[B] =
    flatMap(ev)

  // be careful about filtering all of the elements from an infinite stream---that will cause nontermination
  def filter(p: A => Boolean): ScoredStream[A]
  @inline final def withFilter(p: A => Boolean): ScoredStream[A] = filter(p)
  @inline final def filterNot(p: A => Boolean): ScoredStream[A] = filter(a => !p(a))

  def merge(other: ScoredStream[A]): ScoredStream[A]

  def take(n: Int): ScoredStream[A]

  def takeWhile(f: A => Boolean): ScoredStream[A]

  def drop(n: Int): ScoredStream[A]

  def dropWhile(f: A => Boolean): ScoredStream[A]

  def removeFirst(p: A => Boolean): ScoredStream[A]

  // def collect[B](f: PartialFunction[A, B]): ScoredStream[B]
  def collect[B](f: PartialFunction[A, Scored[B]]): ScoredStream[B]

  // careful to take(n) first! this won't terminate if you're infinite
  def toList: List[A]
  def toListScored: List[Scored[A]]

  def toStream: Stream[A]
  def toStreamScored: Stream[Scored[A]]

  // utility methods for internal use

  // all elements of other guaranteed to be >= all elements of this
  // protected[parsing]def append(other: => ScoredStream[A]): ScoredStream[A]
}

object ScoredStream extends ScoredStreamInstances {

  def empty[A]: ScoredStream[A] =
    ScoredNil[A]

  def unit[A](el: A): ScoredStream[A] =
    Scored.unit(el) ::< ScoredNil[A]
  def unit[A](el: Scored[A]): ScoredStream[A] =
    el ::< ScoredNil[A]

  def unfold[B, A](unacc: B, uncons: B => Option[(Scored[A], B)]): ScoredStream[A] = uncons(unacc) match {
    case None => empty[A]
    case Some((head, remainder)) => head ::< unfold(remainder, uncons)
  }

  // accumulate scores as you go!
  def unfoldAccumulating[B, A](unacc: B, uncons: B => Option[(Scored[A], B)]): ScoredStream[A] = uncons(unacc) match {
    case None => empty[A]
    case Some((head, remainder)) => head ::< unfold(remainder, uncons).adjust(head.score)
  }

  def recurrence[A](z: Scored[A], s: A => Scored[A]): ScoredCons[A] =
    z ::< recurrence(z.flatMap(s), s)
  def recurrence[A](z: A, s: A => Scored[A]): ScoredCons[A] =
    recurrence(Scored.unit(z), s)
  def recurrenceAccumulating[A](z: Scored[A], s: A => Scored[A]): ScoredCons[A] =
    z ::< recurrenceAccumulating(z.flatMap(s), s)

  // for use with side-effecting computations. assumes compute <= subsequent computes.
  // repeats the computation until no result is returned.
  def exhaustively[A](compute: => Option[Scored[A]]): ScoredStream[A] = compute match {
    case None => empty[A]
    case Some(a) => a ::< exhaustively(compute)
  }
  def exhaustivelyAccumulating[A](compute: => Option[Scored[A]]): ScoredStream[A] = compute match {
    case None => empty[A]
    case Some(a) => a ::< exhaustively(compute).adjust(a.score)
  }

  def fromIndexedSeq[A](is: IndexedSeq[Scored[A]]): ScoredStream[A] =
    is.sorted.foldRight(empty[A])(_ ::< _)

  // TODO lazy quicksort is cool, but only useful once we have stack safety, honestly...
  // def fromIndexedSeq[A](is: IndexedSeq[A])(implicit ord: Ordering[A]): ScoredStream[A] = {
  //   if(is.size < 7) {
  //     is.sorted.foldRight(empty[A])(_ ::< _)
  //   } else {
  //     // median of 3 randomly chosen pivots
  //     val pivot = {
  //       import util.{Random => r}
  //       val piv1 = is(r.nextInt(is.size))
  //       val piv2 = is(r.nextInt(is.size))
  //       val piv3 = is(r.nextInt(is.size))
  //       if(ord.lteq(piv1, piv2)) ord.max(piv1, piv3) else ord.max(piv2, piv3)
  //     }
  //     val left = is.filter(ord.lt(_, pivot))
  //     ScoredStream.fromIndexedSeq(left).append(
  //       is.filter(ord.equiv(_, pivot)).foldRight(empty[A])(_ ::< _) // all eq to pivot are already sorted
  //         .append(ScoredStream.fromIndexedSeq(is.filter(ord.gt(_, pivot))))
  //     )
  //   }
  // }

  protected[parsing]def fromSortedSeq[A](s: Seq[Scored[A]]): ScoredStream[A] = {
    s.foldRight(empty[A])(_ ::< _)
  }

  def fromSeq[A](is: Seq[Scored[A]]): ScoredStream[A] =
    fromIndexedSeq(is.toIndexedSeq)

  def fromIterator[A](is: Iterator[Scored[A]]): ScoredStream[A] =
    fromIndexedSeq(is.toVector)

  def fromOption[A](opt: Option[Scored[A]]): ScoredStream[A] = opt match {
    case None => empty[A]
    case Some(a) => unit(a)
  }
  // def fromOption[A](opt: Option[A]): ScoredStream[A] = opt match {
  //   case None => empty[A]
  //   case Some(a) => unit(a)
  // }
}

import ScoredStream._

class ScoredNil[A]() extends ScoredStream[A] {
  override def headOption = None
  override def headItemOption = None
  override def tailOption = None
  override def isEmpty = true
  override def ifNonEmpty = None

  override def adjust(adjustment: Double): ScoredNil[A] =
    this
  override def find(p: A => Boolean) =
    None
  override def insert(a: Scored[A]): ScoredCons[A] =
    a ::< this
  override def map[B](f: A => B) =
    ScoredNil[B]
  override def mapScored[B](f: A => Scored[B]) =
    ScoredNil[B]
  protected[parsing]def mapScoredAux[B](f: A => Scored[B], candidateHead: Scored[B]): ScoredCons[B] =
    candidateHead ::< ScoredNil[B]

  def flatMap[B](f: A => ScoredStream[B]): ScoredStream[B] =
    ScoredNil[B]
  protected[parsing]def flatMapAux[B](f: A => ScoredStream[B], candidateHeadStream: ScoredStream[B]): ScoredStream[B] =
    candidateHeadStream

  override def filter(p: A => Boolean) =
    this
  override def merge(other: ScoredStream[A]) =
    other
  override def take(n: Int) =
    this
  override def takeWhile(f: A => Boolean) =
    this
  override def drop(n: Int) =
    this
  override def dropWhile(f: A => Boolean) =
    this
  override def removeFirst(p: A => Boolean) =
    this
  // override def collect[B](f: PartialFunction[A, B]) =
  //   ScoredNil[B]
  override def collect[B](f: PartialFunction[A, Scored[B]]) =
    ScoredNil[B]

  override def toList = Nil
  override def toListScored = Nil
  override def toStream = Stream.empty[A]
  override def toStreamScored = Stream.empty[Scored[A]]

  override def toString = s"ScoredNil"

  // override protected[parsing]def append(other: => ScoredStream[A]): ScoredStream[A] =
  //   other
}

object ScoredNil {
  def apply[A] = new ScoredNil[A]
  def unapply(sc: ScoredNil[_]): Boolean = true
}

// assumes head is lower order than everything in tail
class ScoredCons[A] protected[parsing] (
  val head: Scored[A],
  _tail: => ScoredStream[A]) extends ScoredStream[A] {
  lazy val tail = _tail

  def headItem = head.item
  def headScore = head.score

  override def headOption = Some(head)
  override def headItemOption = Some(headItem)
  override def tailOption = Some(tail)
  override def isEmpty = false
  override def ifNonEmpty = Some(this)

  override def adjust(adjustment: Double): ScoredCons[A] =
    head.addScore(adjustment) ::< tail.adjust(adjustment)

  override def find(p: A => Boolean) = if(p(headItem)) {
    Some(head)
  } else {
    tail.find(p)
  }

  override def insert(a: Scored[A]): ScoredCons[A] = {
    if(a.score <= headScore) a ::< this
    else head ::< tail.insert(a)
  }

  override def map[B](f: A => B): ScoredCons[B] =
    head.map(f) ::< tail.map(f)

  override def mapScored[B](f: A => Scored[B]): ScoredCons[B] =
    tail.mapScoredAux(f, head.flatMap(f))
  protected[parsing]def mapScoredAux[B](f: A => Scored[B], candidateHead: Scored[B]): ScoredCons[B] =
    if(candidateHead.score < head.score) candidateHead ::< mapScored(f)
    else {
      val fhead = head.flatMap(f)
      val (newCandidate, otherElement) =
        if(candidateHead.score < fhead.score) (candidateHead, fhead)
        else (fhead, candidateHead)
      tail.mapScoredAux(f, newCandidate).insert(otherElement)
    }

  override def flatMap[B](f: A => ScoredStream[B]): ScoredStream[B] =
    tail.flatMapAux(f, f(head.item).adjust(head.score))
  protected[parsing]def flatMapAux[B](f: A => ScoredStream[B], candidateHeadStream: ScoredStream[B]): ScoredStream[B] =
    candidateHeadStream match {
      case ScoredNil() => flatMap(f)
      case candHeadHead ::< candHeadTail =>
        if(candHeadHead.score < head.score) candHeadHead ::< flatMap(f).merge(candHeadTail())
        else {
          val fHeadStream = f(head.item).adjust(head.score)
          fHeadStream match {
            case ScoredNil() => tail.flatMapAux(f, candidateHeadStream)
            case fHeadHead ::< _ =>
              val (newCandidateHeadStream, otherStream) =
                if(candHeadHead.score < fHeadHead.score) (candidateHeadStream, fHeadStream)
                else (fHeadStream, candidateHeadStream)
              tail.flatMapAux(f, newCandidateHeadStream).merge(otherStream)
          }
        }
    }

  override def filter(p: A => Boolean) = if(p(headItem)) {
    head ::< tail.filter(p)
  } else {
    tail.filter(p)
  }

  override def merge(other: ScoredStream[A]) = other match {
    case ScoredNil() => this
    case h ::< t => if(head.score < h.score) {
      head ::< tail.merge(other)
    } else {
      h ::< t().merge(this)
    }
  }

  override def take(n: Int) = if(n <= 0) {
    empty[A]
  } else {
    head ::< tail.take(n - 1)
  }

  override def takeWhile(p: A => Boolean) = if(p(headItem)) {
    head ::< tail.takeWhile(p)
  } else {
    empty[A]
  }

  override def drop(n: Int) = tail.drop(n - 1)

  override def dropWhile(p: A => Boolean) = if(p(headItem)) {
    tail.dropWhile(p)
  } else {
    this
  }

  override def removeFirst(p: A => Boolean) = if(p(headItem)) {
    tail
  } else {
    head ::< tail.removeFirst(p)
  }

  // TODO implement natively
  override def collect[B](f: PartialFunction[A, Scored[B]]) = {
    val easyWayOut = (a: A) => f.lift(a).fold(empty[B])(unit[B])
    flatMap(easyWayOut)
  }
  //   head.map(f.lift).commuteOption[Scored[B]].map(_.flatten[B]) match {
  //   case None => tail.collect(f)
  //   case Some(newHead) => tail match {
  //     case ScoredNil() => newHead ::< ScoredNil[B]
  //     case t @ (second ::< _) =>
  //       if(newHead.score < second.score) newHead ::< t.collect(f)
  //       else t.collect(f).insert(newHead)
  //   }
  // }

  override def toList: List[A] = headItem :: tail.toList
  override def toListScored: List[Scored[A]] = head :: tail.toListScored
  override def toStream: Stream[A] = headItem #:: tail.toStream
  override def toStreamScored: Stream[Scored[A]] = head #:: tail.toStreamScored

  override def toString = s"$head ::< ?"

  // protected util methods

  // override protected[parsing]def append(other: => ScoredStream[A]): ScoredStream[A] =
  //   head ::< tail.append(other)
}

// don't evaluate the tail
object ::< {
  def unapply[A](sc: ScoredCons[A]): Option[(Scored[A], () => ScoredStream[A])] = Some((sc.head, () => sc.tail))
}
// evaluate the tail
object ::<+ {
  def unapply[A](sc: ScoredCons[A]): Option[(Scored[A], ScoredStream[A])] = Some((sc.head, sc.tail))
}

trait ScoredStreamInstances {
  implicit def streamOrdering[A]: Ordering[ScoredStream[A]] = new Ordering[ScoredStream[A]] {
    def compare(a: ScoredStream[A], b: ScoredStream[A]): Int = (a.headOption, b.headOption) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
      case (Some(Scored(_, x)), Some(Scored(_, y))) => x.compare(y)
    }
  }
  implicit def consOrdering[A]: Ordering[ScoredCons[A]] = new Ordering[ScoredCons[A]] {
    def compare(a: ScoredCons[A], b: ScoredCons[A]): Int = a.head.score.compare(b.head.score)
  }
  implicit val scoredStreamMonoidK: MonoidK[ScoredStream] = new MonoidK[ScoredStream] {
    def empty[A]: ScoredStream[A] = ScoredStream.empty[A]
    def combineK[A](x: ScoredStream[A], y: ScoredStream[A]): ScoredStream[A] = x merge y
  }

  implicit def scoredStreamMonoid[A]: Monoid[ScoredStream[A]] = scoredStreamMonoidK.algebra[A]
}

object ScoredStreamExamples {
  def intsFrom(x: Int) = ScoredStream.recurrenceAccumulating(Scored.unit(x), ((y: Int) => Scored(y + 1, 1.0)))
}
