package qfirst

import cats.kernel.CommutativeMonoid
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.Lens

@JsonCodec sealed trait EquivalenceWithApartness[A] {
  def equal(x: A, y: A): Boolean
  def equivalenceClass(x: A): Set[A]
  def apart(x: A, y: A): Boolean
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
