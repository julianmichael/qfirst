package qfirst.frame.math

import qfirst.frame.util.SetUnionFind

import cats.kernel.CommutativeMonoid
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.Lens

/** Represents a paired consistent equivalence relation and apartness relation.
  * An equivalence relation is a relation ~ which is:
  * - Reflexive: x ~ x
  * - Symmetric: x ~ y --> y ~ x
  * - Transitive: x ~ y & y ~ z --> x ~ z
  * An apartness relation is a relation ^ which is:
  * - Irreflexive: !(x ^ x)
  * - Symmetric: x ^ y --> y ^ x
  * - Cotransitive: x ^ z --> x ^ y | y ^ z
  * The complement of an equivalence relation is an apartness relation and vice versa.
  * We say an equivalence relation and apartness relation are _consistent_
  * if each is contained in the other's complement.
  * Consistency gets us further:
  * - Transport: x ~ y & y ^ z --> x ^ z
  * Proof:
  * 1. ~ and ^ are consistent | Assumption.
  * 2. x ~ y                  | Assumption.
  * 3. y ^ z                  | Assumption.
  * 4. y ^ x | x ^ z          | by cotransitivity and 3.
  * 5. y ~ x                  | by symmetry and 2.
  * 6. !(y ^ x)               | by consistency and 5.
  * 5. x ^ z                  | by resolution and 4.
  * qed.
  * Transport implies that apartness relations over X that are consistent with ~
  * are in one-to-one correspondence with arbitrary apartness relations over X/~.
  * We regardless represent ~ via X/~ in a union-find data structure.
  * So we may represent the paired relations by storing an apartness relation over
  * the class representatives in X/~ and updating them appropriately.
  */
@JsonCodec sealed trait EquivalenceWithApartness[A] {
  def equal(x: A, y: A): Boolean
  def equivalenceClass(x: A): Set[A]
  def apart(x: A, y: A): Boolean
  def apartSet(x: A): Set[A]
  def equate(x: A, y: A): EquivalenceWithApartness[A]
  def unequate(x: A, y: A): EquivalenceWithApartness[A]
  def separate(x: A, y: A): EquivalenceWithApartness[A]
  def unseparate(x: A, y: A): EquivalenceWithApartness[A]
}
object EquivalenceWithApartness {
  // Invariants:
  // * apartness only contains relations on representatives in the SUF.
  @JsonCodec private[EquivalenceWithApartness] case class EquivalenceWithApartnessImpl[A](
    equivalenceClasses: SetUnionFind[A],
    apartness: FiniteSymmetricRelation[A]
  ) extends EquivalenceWithApartness[A] {
    // def equivalence = EquivalenceRelation(equivalenceClasses)
    // TODO scrap boilerplate with mapN from Applicative
    def equal(x: A, y: A): Boolean = {
      x == y || equivalenceClasses.find(x)
        .product(equivalenceClasses.find(y))
        .exists(Function.tupled(_ == _))
    }
    def equivalenceClass(x: A): Set[A] = equivalenceClasses.sets.find(_.contains(x)).getOrElse(Set(x))
    def apart(x: A, y: A): Boolean = {
      !(x == y) && equivalenceClasses.find(x)
        .product(equivalenceClasses.find(y))
        .exists(Function.tupled(apartness.contains(_, _)))
    }
    def apartSet(x: A): Set[A] = {
      equivalenceClasses.find(x).foldMap(xRep =>
        apartness.image(xRep).toList.foldMap(equivalenceClass(_))
      )
    }

    def equate(x: A, y: A) = {
      val inclClasses = equivalenceClasses.add(x).add(y)
      val (xRep, yRep) = (inclClasses.find(x).get, inclClasses.find(y).get)
      val (apartFromX, apartFromY) = (
        apartness.image(xRep).filterNot(_ == yRep),
        apartness.image(yRep).filterNot(_ == xRep)
      )
      val oldAparts = (apartFromX.map(xRep -> _) ++ apartFromY.map(yRep -> _)).toList
      val unionedSUF = inclClasses.union(x, y)
      val newRep = unionedSUF.find(x).get // wlog, y is same now
      val newAparts = (apartFromX ++ apartFromY)
        .filter(p => p != (xRep -> yRep) && p != (yRep -> xRep)) // cancel old apartness
        .map(newRep -> _).toList
      EquivalenceWithApartnessImpl(
        unionedSUF,
        apartness -- oldAparts ++ newAparts
      )
    }
    // directional: drops y from the equivalence if necessary
    def unequate(x: A, y: A) = {
      if(x == y) {
        System.err.println(s"Warning: tried to unequate equal values $x. Doing a no-op instead.")
        this
      } else {
        val inclClasses = equivalenceClasses.add(x).add(y)
        val (xRep, yRep) = (inclClasses.find(x).get, inclClasses.find(y).get)
        if(xRep != yRep) EquivalenceWithApartnessImpl(inclClasses, apartness)
        else if(y != yRep) EquivalenceWithApartnessImpl(inclClasses.remove(y), apartness)
        else { // So xRep == yRep == y != x. We need to shift the apartness relation to the new representative
          val apartFromY = apartness.image(y)
          val relWithoutY = inclClasses.remove(y)
          val newRep = relWithoutY.find(x).get
          EquivalenceWithApartnessImpl(
            relWithoutY,
            apartness -- apartFromY.map(y -> _).toList ++ apartFromY.map(newRep -> _).toList
          )
        }
      }
    }

    // directional: drops y (but not x) from the equivalence if necessary
    def separate(x: A, y: A) = {
      val inclClasses = equivalenceClasses.add(x).add(y)
      val (xRep, yRep) = (inclClasses.find(x).get, inclClasses.find(y).get)
      if(xRep != yRep) {
        EquivalenceWithApartnessImpl(
          inclClasses, apartness + (xRep, yRep)
        )
      } else if(y != yRep) { // xRep == yRep != y; apartness is safe after splitting off y
        EquivalenceWithApartnessImpl(
          inclClasses.remove(y).add(y), apartness + (xRep, y)
        )
      } else if(x != xRep) { // so y == yRep == xRep != x. and we must separate them
        // then we can safely remove y, but must recompute xRep
        val splitSUF = inclClasses.remove(y).add(y)
        val newXRep = splitSUF.find(x).get
        EquivalenceWithApartnessImpl(
          splitSUF, apartness + (newXRep, y)
        )
      } else { // x == y == xRep == yRep. Invalid; can't separate when x == y.
        System.err.println(s"Warning: tried to separate equal values $x and $y. No-op instead.")
        this
      }
    }
    def unseparate(x: A, y: A) = {
      val inclClasses = equivalenceClasses.add(x).add(y)
      val (xRep, yRep) = (inclClasses.find(x).get, inclClasses.find(y).get)
      EquivalenceWithApartnessImpl(
        inclClasses, apartness - (xRep, yRep)
      )
    }
  }
  def empty[A]: EquivalenceWithApartness[A] = EquivalenceWithApartnessImpl[A](SetUnionFind.empty[A], FiniteSymmetricRelation.empty[A])
  // def single[A](x: A, y: A): Equivalence[A] = EquivalenceImpl[A](Map(x -> Set(y)), Map(x -> Set(y)))

  // not really a lawful lens. useful anyway
  def equal[A](x: A, y: A) = Lens[EquivalenceWithApartness[A], Boolean](
    e => e.equal(x, y))(b => e => if(b) e.equate(x, y) else e.unequate(x, y)
  )
  def apart[A](x: A, y: A) = Lens[EquivalenceWithApartness[A], Boolean](
    e => e.apart(x, y))(b => e => if(b) e.separate(x, y) else e.unseparate(x, y)
  )
}
