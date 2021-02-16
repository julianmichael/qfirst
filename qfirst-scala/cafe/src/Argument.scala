package qfirst.cafe

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import cats.Monoid
import cats.MonoidK
import cats.data.Validated
import cats.data.ValidatedNec
import cats.data.NonEmptyChain
import cats.implicits._
import mouse.all._

// S4: Simple Surrogate Syntactic Semantics


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
  def render: ValidatedNec[
    Predication.Error, LabeledTree[String, String]
  ] = ???

  protected[Predication] def error(msg: String) = Predication.Error(this, msg)
  protected[Predication] def invalid(
    msg: String = "Incomplete predication."
  ): ValidatedNec[Predication.Error, LabeledTree[String, String]] =
    Validated.invalid(NonEmptyChain.one(Predication.Error(this, msg)))
}
object Predication {
  case class Expletive(form: LowerCaseString) // it, there
  case class Preposition(form: LowerCaseString)
  case class Complementizer(form: LowerCaseString)
  case class InfinitiveComplementizer(form: LowerCaseString)
  case class Subordinator(form: LowerCaseString)
  case class Adjective(form: LowerCaseString)
  case class Verb(forms: InflectedForms)

  sealed trait PhraseType {
    def wh: LowerCaseString
    def placeholderOpt: Option[LowerCaseString]
  }
  case class ObliqueType(
    wh: LowerCaseString, placeholderOpt: Option[LowerCaseString]
  ) extends PhraseType
  case class NounType(
    wh: LowerCaseString, placeholder: LowerCaseString
  ) extends PhraseType {
    def placeholderOpt: Some[LowerCaseString] = Some(placeholder)
  }

  object Wh {
    val what = NounType("what".lowerCase, "something".lowerCase)
    val who = NounType("who".lowerCase, "someone".lowerCase)
    val where = ObliqueType("where".lowerCase, Some("somewhere".lowerCase))
    // TODO: remainder of wtypes
  }
  import Wh._

  sealed trait Person
  object Person {
    case object First extends Person
    case object Second extends Person
    case object Third extends Person
  }

  sealed trait Number
  object Number {
    case object Singular extends Number
    case object Plural extends Number
  }

  case class Error(predication: Predication, msg: String)

  // anything that can be combined with a subject to produce a clause
  // (finite or non-finite)
  sealed trait VerbalPredication extends Predication
  // anything right side of adjective
  sealed trait NonNominalPredication extends Predication

  // includes adverbs ('quickly', 'happily' etc.), obliques (PPs),
  // adjective phrases (as they can act predicatively),
  sealed trait Adverbial extends NonNominalPredication
  // gerunds and participials which may appear after subordinators
  sealed trait Subordinable extends Adverbial

  // anything that can appear as subject. includes:
  //  - infinitives (to err is human)
  //  - that-comps (That he's still smoking distresses me greatly.)
  //  - for-np-to-vp (For him to stop would [be a momentous shift] / [thrill me].)
  sealed trait RelaxedNominal extends Predication
  // anything that can be object of a preposition,
  // i.e., nouns and gerunds, gerunds with subject
  // (not represented at all: gerunds with possessive subject)
  sealed trait Nominal extends RelaxedNominal

  // just nouns or obliques, to help with defining copulas
  sealed trait NounOrOblique extends Predication

  // sealed trait Noun extends NounOrOblique

  // predicate structure
  sealed trait Predication {
    def subject: Option[Subject]
  }
  // argument structure (with slots etc.)
  sealed trait Argument {
  }
  sealed trait Subject extends Argument {
    def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]
  }
  sealed trait NonSubjectArgument extends Argument
  sealed trait NonNominalArgument extends NonSubjectArgument
  sealed trait Complement extends NonNominalArgument
  sealed trait NounOrOblique extends Argument
  sealed trait Nominal extends NominalOrOblique {
    def animate: Option[Boolean]
  }
  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case object SimpleAdverbial extends NonNominalArgument
  case class Gerund(
    pred: Predication, includeSubject: Boolean
  ) extends Subject with Nominal {
    def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
  }
  case class Infinitive(
    pred: Predication, includeSubject: Boolean) extends Complement with Subject

  case class PresentParticipial(
    pred: Predication, includeSubject: Boolean) extends Complement

  case class PassiveParticipial(
    pred: Predication, includeSubject: Boolean) extends Complement

  case class FiniteClause(
    pred: Predication) extends Complement

  case class ComplementClause(
    pred: Predication, complementizer: Complementizer) extends Complement with Subject

  case class SubordinateClause(
    subordinator: Subordinator, clause: Complement) extends NonNominalArgument

  case class ExpletiveNoun(expletive: Expletive) extends Subject {
    def typ = None
    def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
    override def render: ValidatedNec[Error, LabeledTree[String, String]] = {
      Validated.valid(LabeledTree.leaves("noun[expl]" -> expletive.form.toString))
    }
  }

  case class NounRealization(
    form: String,
    person: Option[Person],
    number: Option[Number]
  )

  case class NounPhrase(
    realization: Option[NounRealization],
    val animate: Option[Boolean]
  ) extends Subject with Nominal {
    override def render: ValidatedNec[Error, LabeledTree[String, String]] = {
      def validLeaf(x: String) = Validated.valid(LabeledTree.leaves("noun" -> x))
      realization.map(_.form).map(validLeaf)
        .orElse(animate.map(_.fold("someone", "something")).map(validLeaf(_)))
        .getOrElse(invalid())
    }
    def typ: Option[NounType] = animate.map(_.fold(who, what))
    def person = realization.map(_.person)
      .getOrElse(animate.map(_ => Person.Third))
    def number: Option[Number] = realization.map(_.number)
      .getOrElse(animate.map(_ => Number.Singular))
  }

  sealed trait PrepObjRealization
  // prep should not take an object, i.e., is a particle
  case object NoPrepObject extends PrepObjRealization
  // prep should take an object, which may not be specified
  case class PrepObject(obj: Option[Nominal]) extends PrepObjRealization

  case class PrepPhrase(
    preposition: Preposition, // TODO NonEmptyList of prepositions?
    obj: PrepObjRealization
  )

  case class Oblique(
    typ: Option[ObliqueType],
    prepPhrase: Option[PrepPhrase]
  ) extends NounOrOblique with NonNominalArgument {
    override def render: ValidatedNec[Error, LabeledTree[String, String]] = {
      def validTree(x: LabeledTreeChild[String, String]) =
        Validated.valid(LabeledTree("pp" -> x))
      def validLeaf(x: String) =
        Validated.valid(LabeledTree.leaves("pp" -> x))
      prepPhrase match {
        case None => typ.flatMap(_.placeholderOpt).fold(invalid())(p => validLeaf(p.toString))
        case Some(PrepPhrase(prep, obj)) => obj match {
          case NoPrepObject => validTree(LabeledTree.leaves("prt" -> prep.toString))
          case PrepObject(None) => invalid()
          case PrepObject(Some(nominal)) =>
            nominal.render.map(nominal =>
              LabeledTree(
                "pp" -> LabeledTree(
                  "prep" -> LabeledTree.leaf(prep.toString),
                  "pobj" -> nominal
                )
              )
            )
        }
      }
    }
  }

  case class Copular(
    subject: Option[Subject],
    predicate: NounOrOblique,
    complements: Vector[NonNominalArgument]
  ) extends Predication

  case class Adjectival(
    subject: Option[Subject],
    adjective: Option[Adjective],
    arguments: Vector[NonNominalArgument]
  ) extends Predication {
    override def render: ValidatedNec[Error, LabeledTree[String, String]] = {
      def validLeaf(x: String) =
        Validated.valid(LabeledTree.leaves("adjp" -> x))
      def validTree(x: LabeledTreeChild[String, String]) =
        Validated.valid(LabeledTree("adjp" -> x))
      adjective match {
        case None => invalid()
          // typ.flatMap(_.placeholderOpt).fold(invalid())(p => validLeaf(p.toString))
        case Some(adjective) =>
          arguments.traverse(_.render).map { args =>
            LabeledTree(
              "adjp" -> (
                LabeledTree.leaves("adj" -> adjective.form.toString) +: args
              ).combineAll
            )
          }
      }
    }
  }

  // TODO:
  // verb/clause rendering
  // gapping
  // rendering questions, filling gaps, adding rich info to errors
  // relative clauses, free relatives, embedded questions

  // @JsonCodec @Lenses case class TAN(
  //   tense: Tense,
  //   isPerfect: Boolean,
  //   isProgressive: Boolean,
  //   isNegated: Boolean
  // ) {
  //   def getVerbPrefixAndForm(
  //     isPassive: Boolean,
  //     subjectPresent: Boolean
  //   ): (List[LowerCaseString], VerbForm) = {
  //     val dummyFrame = Frame(
  //       ArgStructure(DependentMap.empty[ArgumentSlot.Aux, Id], isPassive),
  //       InflectedForms.generic, this)
  //     val initVerbStack = dummyFrame.getVerbStack
  //     val verbStack = if(subjectPresent) {
  //       dummyFrame.splitVerbStackIfNecessary(initVerbStack)
  //     } else initVerbStack
  //     val verbPrefix = verbStack.init.map(_.lowerCase)
  //     val verbForm = dummyFrame.getVerbConjugation(subjectPresent)
  //     verbPrefix -> verbForm
  //   }
  // }
  // object TAN

  // argument: 'he helped (me/_) do the laundry / finish the homework.'
  // argument: 'he helped (me/*_) be designated as leader.'
  // argument: 'he helped (me/*_) be the man i wanted to be.'
  // verbs: help, make (make him do his hw), go (go swim).
  // not included in NonNominalPredication bc can't appear as an adjective argument (?)
  // sealed trait VoicedVerbPhrase extends VerbalPredication with Subordinable

  // aspects: unmarked (go), perfect (have gone), progressive (be going),
  // perfect progressive (have been going)
  // case class Active(verb: VerbPhrase) extends VoicedVerbPhrase

  // aspects: unmarked (be broken), perfect (have been broken), progressive (being broken),
  // no perfect progressive? (having been being broken?)
  // case class Passive(verb: VerbPhrase) extends VoicedVerbPhrase

  // aspects: unmarked (be happy), perfect (have been happy), progressive (being happy),
  // no perfect progressive? (having been being happy?)
  // case class Copula(predicate: NonVerbalPredication) extends VoicedVerbPhrase

  // nominal?: I gave killing flies to him. ...?
  // can appear:
  //  - anywhere a noun can
  //  - argument of a verb: 'he loves doing yardwork' / having done yardwork / having been doing yw
  //  - adverbial of a verb: 'he walked to the store carrying his suitcase'/having done bad stuff/etc
  //  - adverbial of a verb: 'he is perfectly happy keeping to himself'/having gone only once
  // aspects (active): unmarked (going), perfect (having gone), perfect progressive (having been going)
  // aspects (passive): unmarked (being broken), perfect (having been broken)
  // aspects (passive): unmarked (being broken), perfect (having been broken)
  // case class Gerund(
  //   verb: VoicedVerbPhrase // TODO aspect
  // ) extends Nominal with VerbalPredication with NonNominalPredication with Subordinable

  // can appear as:
  //  - subject of finite clause: to err is human
  //  - open complement of verb: he wants to help
  //  - open complement of adjective: he is happy to help
  // case class Infinitive(
  //   verb: VoicedVerbPhrase
  // ) extends RelaxedNominal with VerbalPredication with NonNominalPredication

  // many types of clause:
  //  - active 'he go to the store': any use? no? helped him go to the store. 2 args
  //    - complement: that he go to the store
  //  - passive 'him be taken advantage of': made me be taken advantage of. 2 args
  //    - complement: that he be taken advantage of
  //  - copula 'me be happy': he helped me be happy. 2 args. all the same.
  //    - complement: that he be happy
  //  - gerund
  //     - active 'him swimming': nominal, NOT non-nominal
  //     - passive 'him being taken advantage of': nominal, NOT non-nominal
  //     - copula 'him being a leader in his field': nominal, NOT non-nominal
  //   -- TODO: aspectual possibilities
  //  - infinitive: 'he to go to the store': again 2 args.

  // for-np-to-vp

  // types of subordinate clause:
  //  - finite: because he didn't know what he was doing
  //  - --- gerund: despite him knowing what he was doing --- can model as PP
  //  - ??? bare: lest he be scolded for his actions. very rare / archaic(?)
  //  - xxx infinitive: because him to know what he was doing .. no
  // only finite

  // types of subordinate clause:
  //  - gerund: when knowing what he was doing?, while walking away
  //    - although, though, while, since, after, as soon as, whenever, when, until
  //  - both kinds of participial
  //    - although (worried by the news / having talked through tears)
  //    - while (worried by the news / having talked through tears)
  //  - be: lest be scolded for his actions. no
  //  - infinitive: in order to know what he was doing
  //    are there any others that look like this?
  // only finite

  // participial modifiers:
  //  - present participial (the bomb exploded, destroying the building.) // covered by gerund
  //  - passive/past participial (worried by the news, she called the hospital.)
  //  - perfect participial (having gotten dressed, he slowly went downstairs.)

  // DONE?
  // maybe do something with internal gapping (e.g., 'he felt taken advantage of')
  //  - actually no: this is fine. the 'gap' was already moved to subj position.
  //  - 'non-subj' gaps or whatever may only be a problem in actual relative clauses


}
