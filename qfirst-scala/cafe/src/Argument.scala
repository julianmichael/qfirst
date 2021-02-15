package qfirst.cafe

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import cats.Monoid
import cats.MonoidK
import cats.data.Validated

sealed trait Argument
object Argument {
  case object Subj extends Argument
  case class Object(index: Int) extends Argument
  case class Oblique(index: Int) extends Argument
  case class Adverbial(index: Int) extends Argument
}

sealed trait LabeledTreeChild[+Label, +A]
case class LabeledTree[+Label, +A](
  branches: Vector[(Label, LabeledTreeChild[Label, A])]
) extends LabeledTreeChild[Label, A]
case class LabeledTreeLeaf[+A](value: A) extends LabeledTreeChild[Nothing, A]
object LabeledTree {
  def apply[Label, A](branches: (Label, LabeledTreeChild[Label, A])*): LabeledTree[Label, A] =
    LabeledTree(branches.toVector)

  def leaf[A](value: A) =
    LabeledTreeLeaf(value)

  def leaves[Label, A](leaves: (Label, A)*) =
    LabeledTree(leaves.toVector.mapSecond(leaf[A](_)))

  implicit def labeledTreeMonoidK[Label]: MonoidK[LabeledTree[Label, *]] =
    new MonoidK[LabeledTree[Label, *]] {
      def empty[A]: LabeledTree[Label, A] = LabeledTree[Label, A]()
      def combineK[A](x: LabeledTree[Label, A], y: LabeledTree[Label, A]): LabeledTree[Label, A] =
        LabeledTree(x.branches ++ y.branches)
    }
  implicit def labeledTreeMonoid[Label, A]: Monoid[LabeledTree[Label, A]] =
    labeledTreeMonoidK[Label].algebra[A]
}

sealed trait Predication {
  def render(
    spec: Predication.ArgumentSpecification
  ): Validated[
    Predication.ArgumentSpecification.Error, LabeledTree[String, String]
  ] = ???
}
object Predication {
  case class Preposition(form: LowerCaseString)
  case class Complementizer(form: LowerCaseString)
  case class Subordinator(form: LowerCaseString)
  case class Adjective(form: LowerCaseString)
  case class Verb(forms: InflectedForms)

  sealed trait Oblique extends Predication
  sealed trait NonVerb extends Predication
  sealed trait RelaxedNominal extends NonVerb
  sealed trait Nominal extends RelaxedNominal
  sealed trait Adverbial

  sealed trait ArgumentSpecification
  object ArgumentSpecification {
    case class Error(
      predication: Predication,
      spec: ArgumentSpecification,
      message: String = "Invalid specification."
    )

    case object Gap extends ArgumentSpecification
    case class SurfaceString(form: String) extends ArgumentSpecification
    case class PlaceholderNoun(animate: Boolean) extends ArgumentSpecification
    case class PrepSpec(obj: Option[ArgumentSpecification]) extends ArgumentSpecification

  }

  import ArgumentSpecification._

  // NOTE: could abstract out even more from the predication structure;
  // it could then be added back piecemeal on a case-by-case basis as part of
  // the drive towards 'simpler syntax' --- which will involve partial specification of
  // full frames to allow for MWEs, tunable granularity, etc.

  case object Noun extends Nominal {
    override def render(spec: ArgumentSpecification): Validated[Error, LabeledTree[String, String]] = {
      def validLeaf(x: String) = Validated.valid(LabeledTree.leaves("noun" -> x))
      spec match {
        case Gap => validLeaf("")
        case SurfaceString(form) => validLeaf(form)
        case PlaceholderNoun(animate) =>
          val form = if(animate) "someone" else "something"
          validLeaf(form)
        case other => Validated.invalid(Error(this, spec))
      }
    }
  }
  case class PrepositionalPhrase(
    preposition: Preposition,
    obj: Option[Nominal]
  ) extends Oblique with NonVerb {
    override def render(spec: ArgumentSpecification): Validated[Error, LabeledTree[String, String]] = {
      def invalid(x: String = "Invalid specification.") =
        Validated.invalid(Error(this, spec, x))
      def validTree(x: LabeledTreeChild[String, String]) =
        Validated.valid(LabeledTree("pp" -> x))
      spec match {
        case Gap => validTree(LabeledTree.leaf(""))
        case PrepSpec(pobj) => (obj, pobj) match {
          case (None, None) =>
            validTree(LabeledTree.leaves("prep" -> preposition.toString))
          case (Some(obj), Some(pobj)) =>
            obj.render(pobj).map { nomResult =>
              LabeledTree(
                "prep" -> LabeledTree.leaf(preposition.toString),
                "pobj" -> nomResult
              )
            }
          case (Some(_), None) => invalid("Specification missing prepositional object.")
          case (None, Some(_)) => invalid("Specification has extra prepositional object.")
        }
        case _ => invalid()
      }
    }
  }
  case class AdjectivePhrase(
    // adjective: Adjective,
    obliques: Vector[Oblique],
    adverbials: Vector[Adverbial],
    ) extends Oblique with NonVerb {
  }
  case class VerbPhrase(
    // verb: Verb,
    particle: Option[Preposition],
    objects: Vector[Nominal],
    obliques: Vector[Oblique],
    adverbials: Vector[Adverbial]
  )
  sealed trait Clausal extends Predication
  sealed trait VoicedVerb extends Oblique with Clausal
  case class Active(verb: Verb) extends VoicedVerb
  case class Passive(verb: Verb) extends VoicedVerb
  case class Copula(predicate: NonVerb) extends VoicedVerb
  case class Gerund(verb: VoicedVerb) extends Oblique with Nominal with Clausal
  case class Infinitive(verb: Verb) extends Oblique with RelaxedNominal with Clausal
  case class Finitive(verb: Verb) extends Clausal
  case class Clause(
    subject: RelaxedNominal,
    predicate: Clausal
  )
  case class Complement(
    complementizer: Complementizer,
    clause: Clause
  )

  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  // maybe should subclass nominal?
  case object SimpleAdverbial extends Adverbial
  case class Subordinate(
    subordinator: Subordinator,
    clause: Clause
  ) extends Adverbial

}
