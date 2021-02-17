package qfirst.cafe

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import cats.data.Validated
import cats.data.ValidatedNec
import cats.data.NonEmptyChain
import cats.implicits._
import mouse.all._
import cats.kernel.Monoid

// S4: Simple Surrogate Syntactic Semantics

// SASS: Semantic Annotation via Surrogate Syntax

object Lexicon {

  case class Expletive(form: LowerCaseString) // it, there
  case class Preposition(form: LowerCaseString)
  case class Complementizer(form: LowerCaseString)
  case class InfinitiveComplementizer(form: LowerCaseString)
  case class Subordinator(form: LowerCaseString)
  case class Adjective(form: LowerCaseString)
  case class Verb(forms: InflectedForms)
}

sealed trait PhraseType {
  def wh: LowerCaseString
  def placeholderOpt: Option[LowerCaseString]
}
object PhraseType {
  sealed trait NominalType extends PhraseType {
    final def placeholderOpt: Some[LowerCaseString] = Some(placeholder)
    def placeholder: LowerCaseString
  }
  sealed trait AdverbialType extends PhraseType

  object What extends NominalType {
    def wh = "what".lowerCase
    def placeholder = "something".lowerCase
  }
  object Who extends NominalType {
    def wh = "who".lowerCase
    def placeholder = "someone".lowerCase
  }
  object Where extends AdverbialType {
    def wh = "where".lowerCase
    def placeholderOpt = Some("somewhere".lowerCase)
  }
}

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

sealed trait Component {
  protected[Component] def leaf(label: String, content: String) = {
    Validated.valid(LabeledTree.leaves(label -> content))
  }
  protected[Component] def error(msg: String) = Component.RenderError(this, msg)
  protected[Component] def invalid(
    msg: String = "Incomplete component."
  ): Component.RenderResult =
    Validated.invalid(NonEmptyChain.one(error(msg)))
}
object Component {
  case class RenderError(component: Component, msg: String)
  type RenderResult = ValidatedNec[RenderError, LabeledTree[String, String]]
}

// argument structure (with slots etc.)
sealed trait Argument extends Component {
  def render: Component.RenderResult
}
object Argument {
  import Component._
  import Lexicon._
  import PhraseType._
  // can appear as subject
  sealed trait Subject extends Argument {
    // def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]
  }
  // can appear as non-subject argument of a verb
  sealed trait NonSubjectArgument extends Argument
  // can appear as argument/adjunct of an adjective or adjunct of a copula
  // (includes obliques, complements, and adverbials)
  sealed trait NonNominalArgument extends NonSubjectArgument
  // can be turned into a subordinate clause
  sealed trait Complement extends NonNominalArgument
  // can be used predicatively with a copula (is not verb or adjective)
  sealed trait NounOrOblique extends NonSubjectArgument
  // can appear as the object of a preposition
  sealed trait Nominal extends NonSubjectArgument {
    // def animate: Option[Boolean]
  }

  // TODO: RenderContext:
  // tells us whether we are rendering an argument...
  // -- in subject position (for nom/accus, preventing in-context subj use of complements)
  // -- in an answer (for bare vp answers to 'what..to do' questions
  // -- in an argument position?


  // expletive 'it' or 'there'; can only appear as subject
  case class ExpletiveNoun(expletive: Expletive) extends Subject {
    def typ = None
    def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
    override def render: RenderResult = {
      leaf("noun[expl]", expletive.form.toString)
    }
  }

  case class NounRealization(
    form: String,
    person: Option[Person],
    number: Option[Number]
  )

  case class NounPhrase(
    realization: Option[NounRealization],
    animate: Option[Boolean]
  ) extends Subject with Nominal {
    override def render: RenderResult = {
      def validLeaf(x: String) = Validated.valid(LabeledTree.leaves("noun" -> x))
      realization.map(_.form).map(validLeaf)
        .orElse(animate.map(_.fold("someone", "something")).map(validLeaf(_)))
        .getOrElse(invalid())
    }
    def typ: Option[NominalType] = animate.map(_.fold(Who, What))
    def person = realization.map(_.person)
      .getOrElse(animate.map(_ => Person.Third))
    def number: Option[Number] = realization.map(_.number)
      .getOrElse(animate.map(_ => Number.Singular))
  }
  object NounPhrase {
    val something = NounPhrase(None, Some(false))
    val someone = NounPhrase(None, Some(true))
  }
  import NounPhrase._

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
    typ: Option[AdverbialType],
    prepPhrase: Option[PrepPhrase]
  ) extends NounOrOblique with NonNominalArgument {
    override def render: RenderResult = {
      def validTree(x: LabeledTreeChild[String, String]) =
        Validated.valid(LabeledTree("pp" -> x))
      prepPhrase match {
        case None => typ.flatMap(_.placeholderOpt).fold(invalid())(p => leaf("pp", p.toString))
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


  // nominal use of gerund/progressive; can be object of prep or subject
  case class Gerund(
    pred: Predication, includeSubject: Boolean
      // TODO possessive subject option
  ) extends Subject with Nominal {

    override def render: Component.RenderResult =
      pred.render(ClauseType.Progressive, includeSubject)

    // def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
  }
  // various possible forms of complements:
  // may be specific to a predicate's (or subordinator's!) subcat frame.
  case class Infinitive(
    pred: Predication, includeSubject: Boolean
  ) extends Complement with Subject {

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def render: Component.RenderResult =
      pred.render(ClauseType.Infinitive, includeSubject)
  }

  case class Progressive(
    pred: Predication, includeSubject: Boolean
  ) extends Complement {
    def render: Component.RenderResult =
      pred.render(ClauseType.Progressive, includeSubject)
  }

  case class Attributive(
    pred: Predication.NonCopular, includeSubject: Boolean
  ) extends Complement {
    def render: Component.RenderResult =
      pred.render(ClauseType.Attributive, includeSubject)
  }

  case class Finite(
    pred: Predication
  ) extends Complement { // with Subject? but cannot appear in subj except as an answer
    def render: Component.RenderResult =
      pred.render(ClauseType.Finite, true)
  }

  case class FiniteComplement(
    pred: Predication, complementizer: Complementizer
  ) extends Complement with Subject {

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def render: Component.RenderResult = List(
      Validated.valid(LabeledTree.leaves("comp" -> complementizer.form.toString)),
      pred.render(ClauseType.Finite, true)
    ).foldA
  }

  // TODO: add predication structure to subordinators/adverbials
  case class SubordinateClause(
    subordinator: Subordinator, clause: Complement
  ) extends NonNominalArgument {
    def render: Component.RenderResult = List(
      Validated.valid(LabeledTree.leaves("sub" -> subordinator.form.toString)),
      clause.render
    ).foldA
  }

  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    form: Option[String]
  ) extends NonNominalArgument {
    override def render: Component.RenderResult = {
      form.fold(invalid("Missing adverbial form."))(leaf("adv", _))
    }
  }

  // only used in 'do' pro-form.
  case class StandinBareVerbPhrase(
    pred: Option[Predication]
  ) extends Argument {
    def render: Component.RenderResult =
      pred.fold(someone.render)(
        // TODO: only allow in answer. otherwise, perhaps error asking for resolution.
        _.render(ClauseType.Bare, includeSubject = false)
      )
  }
}

sealed trait ClauseType
object ClauseType {
 // past participle form --- passive/adjective
  case object Attributive extends ClauseType
  sealed trait VerbalClauseType extends ClauseType
  case object Bare extends VerbalClauseType
  case object Infinitive extends VerbalClauseType
  case object Progressive extends VerbalClauseType
  case object Finite extends VerbalClauseType
}

sealed trait Predication extends Component {
  def subject: Option[Argument.Subject]
  def render(clauseType: ClauseType, includeSubject: Boolean): Component.RenderResult
  def tan: TAN

  def renderSubject(includeSubject: Boolean) = {
    if(!includeSubject) {
      Validated.valid(Monoid[LabeledTree[String, String]].empty)
    } else subject match {
      case None => invalid("Missing subject.")
      case Some(subj) => subj.render
    }
  }
}
object Predication {
  import Component._
  import Lexicon._
  import PhraseType._
  import Argument._

  // 'someone does something'
  // not sure if i should keep this in as a general predication.
  // can it appear everywhere?
  // maybe it should be an argument instead.
  case class ActiveDoProForm(
    subject: Option[Subject],
    argument: StandinBareVerbPhrase, // standin just for this purpose; has 'what' and 'something'
    modifiers: Vector[NonNominalArgument],
    tan: TAN
  )
  // maybe also add a copular pro-form? already handled for Copula
  // but missing for adjective and passive. not sure how gapping should work with it.

  // 'something is done'.
  // maybe can add this in later.
  // case class PassiveProForm(
  //   subject: Subject, --- seems too general; maybe not best to use as pro-form?
  //   arguments: Vector[NonSubjectArgument] = Noun()
  //   tan: TAN
  // ) {
  //   // verb: Verb = Verb(InflectedForms.doForms)
  // }
  // maybe also 'what happened _?' type of pro-form?
  // not as necessary since it isn't needed for constructing questions in existing framework

  case class Copular(
    subject: Option[Subject],
    argument: NounOrOblique,
    modifiers: Vector[NonNominalArgument],
    tan: TAN
  ) extends Predication {

    def render(clauseType: ClauseType, includeSubject: Boolean): RenderResult = {
      val subj = renderSubject(includeSubject)
      val arguments = argument +: modifiers
      val args = arguments.foldMapA(_.render)
      clauseType match {
        case ClauseType.Attributive =>
          invalid("Cannot construct attributive clause from a copula.")
        case otherType: ClauseType.VerbalClauseType =>
          val aux = Validated.fromEither(
            tan.getCopulaAuxChain(otherType, subject).left.map(_.map(error(_)))
          ).map(auxes => LabeledTree.leaves("aux" -> auxes.toList.mkString(" ")))
          List(subj, aux, args).foldA
      }
    }
  }

  sealed trait NonCopular extends Predication

  case class Adjectival(
    subject: Option[Subject],
    adjective: Adjective,
    arguments: Vector[NonNominalArgument],
    tan: TAN
  ) extends NonCopular {
    def render(clauseType: ClauseType, includeSubject: Boolean): RenderResult = {
      val adj = Validated.valid(LabeledTree.leaves("adj" -> adjective.form.toString))
      val subj = renderSubject(includeSubject)
      val args = arguments.foldMapA(_.render)
      clauseType match {
        case ClauseType.Attributive =>
          List(subj, adj, args).foldA
        case otherType: ClauseType.VerbalClauseType =>
          val aux = Validated.fromEither(
            tan.getCopulaAuxChain(otherType, subject).left.map(_.map(error(_)))
          ).map(auxes => LabeledTree.leaves("aux" -> auxes.toList.mkString(" ")))
          List(subj, aux, adj, args).foldA
      }
    }
  }

  case class Verbal(
    subject: Option[Subject],
    verb: Verb,
    isPassive: Boolean,
    arguments: Vector[NonSubjectArgument],
    tan: TAN
  ) extends NonCopular {
    def render(clauseType: ClauseType, includeSubject: Boolean): RenderResult = {
      val subj = renderSubject(includeSubject)
      val args = arguments.foldMapA(_.render)
      def pastParticiple = Validated.valid(
        LabeledTree.leaves("verb" -> verb.forms.pastParticiple.toString)
      )
      clauseType match {
        case ClauseType.Attributive =>
          if(!isPassive) {
            invalid("Cannot construct attributive clause from active form.")
          } else List(subj, pastParticiple, args).foldA
        case otherType: ClauseType.VerbalClauseType =>
          if(isPassive) {
            val aux = Validated.fromEither(
              tan.getCopulaAuxChain(otherType, subject).left.map(_.map(error(_)))
            ).map(auxes => LabeledTree.leaves("aux" -> auxes.toList.mkString(" ")))
            List(subj, aux, pastParticiple, args).foldA
          } else {
            val verbChain = Validated.fromEither(
              tan.getAuxChain(verb.forms, otherType, subject).left.map(_.map(error(_)))
            ).map { chain =>
              val vb = LabeledTree.leaves("verb" -> chain.last)
              val auxChain = chain.init
              if(auxChain.isEmpty) vb
              else LabeledTree.leaves("aux" -> auxChain.toList.mkString(" ")) |+| vb
            }
            List(subj, verbChain, args).foldA
          }
      }
    }
  }

  // TODO:
  // gapping
  // rendering questions, filling gaps, adding rich info to errors
  // using rich errors to feed back into filling in slots
  // aligning to QA-SRL questions
  // relative clauses, free relatives, embedded questions

  // ---------------------------------------------------------------

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
