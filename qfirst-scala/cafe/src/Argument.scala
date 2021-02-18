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

sealed trait ClauseType
object ClauseType {
  // past participle form --- passive/adjective
  case object Attributive extends ClauseType
  sealed trait VerbalClauseType extends ClauseType
  case object Bare extends VerbalClauseType
  case object Infinitive extends VerbalClauseType
  case object Progressive extends VerbalClauseType
  case object Finite extends VerbalClauseType

  val all = List(Attributive, Bare, Infinitive, Progressive, Finite)
}

case class ArgumentPosition[+A <: Argument](
  val pro: Option[ArgumentProForm[A]],
  val arg: Option[A]
) {

  def symbol = {
    val proSym = pro.flatMap(_.wh).foldMap(wh => s"{$wh}")
    val argSym = arg.foldMap(a => s"${a.symbol}")
    s"$argSym $proSym".trim
  }
  def render: Component.RenderResult = {
    def noArg = Validated.invalid(
      NonEmptyChain.one(
        Component.RenderError(???, "No argument information present.")
      )
    )
    // TODO: maybe fall back from arg errors to render pro-form? idk.
    // maybe also choose between them using some kind of more comprehensive policy...
    arg.map(_.render).orElse(pro.map(_.render)).map { res =>
      res.map(tree => LabeledTree(symbol -> tree))
    }.getOrElse(noArg)
  }
  def person(implicit ev: A <:< Argument.Subject): Option[Person] = arg.flatMap(_.person)
    .orElse(pro.flatMap(_.placeholder).flatMap(_.person))
  def number(implicit ev: A <:< Argument.Subject): Option[Number] = arg.flatMap(_.number)
    .orElse(pro.flatMap(_.placeholder).flatMap(_.number))
}

sealed trait ArgumentProForm[+A <: Argument] {
  // TODO: instead of Option, have each of these take a context and return a RenderResult
  def wh: Option[LowerCaseString]
  def placeholder: Option[A]
  // def isNominal: Boolean
  def render: Component.RenderResultOf[LabeledTreeChild[String, String]] = {
    placeholder.map(_.render).getOrElse(
      Validated.invalid(
        NonEmptyChain.one(
          Component.RenderError(???, "No placeholder for pro-form.")
        )
      )
    )
  }
}
object ArgumentProForm {
  sealed trait Expletive extends ArgumentProForm[Argument.Expletive]
  // TODO consider expanding to a 'relaxed nominal' type which includes full complements, etc.
  // if necessary. depends whether type param must correspond to arg param or just placeholder.
  sealed trait IndefinitePronoun extends ArgumentProForm[Argument.Nominal]
  // TODO consider expanding to a RelaxedAdverbial to include obliques, some complements, etc.
  // if necessary. depends whether type param must correspond to arg param or just placeholder.
  sealed trait IndefiniteAdverbial extends ArgumentProForm[Argument.Adverbial]

  object it extends Expletive {
    def wh = None
    def placeholder = Some(Argument.Expletive.there)
  }

  object there extends Expletive {
    def wh = None
    def placeholder = Some(Argument.Expletive.there)
  }

  object what extends IndefinitePronoun {
    def wh = Some("what".lowerCase)
    def placeholder = Some(Argument.NounPhrase.something)
  }

  object who extends IndefinitePronoun {
    def wh = Some("who".lowerCase)
    def placeholder = Some(Argument.NounPhrase.someone)
  }

  object where extends IndefiniteAdverbial {
    def wh = Some("where".lowerCase)
    def placeholder = Some(Argument.Adverbial.somewhere)
  }

  object why extends IndefiniteAdverbial {
    def wh = Some("why".lowerCase)
    def placeholder = None
  }

  // TODO rest of adverbials

  case class GerundiveDoingSomething(
    includeSubject: Boolean, // TODO add possessive version
    subject: ArgumentPosition[Argument.Subject],
    modifiers: Vector[ArgumentPosition[Argument.NonNominal]],
    tan: TAN
  ) extends ArgumentProForm[Argument.Gerund] {
    // TODO: need to be able to render the 'gap'
    def wh = Some("what".lowerCase)
    def placeholder = {
      val pred = Predication.Verbal.doSomething(subject, modifiers, tan)
      Some(Argument.Gerund(includeSubject, Some(pred)))
    }
  }

  // 'someone does something'
  case class DoSomething(
    clauseType: ClauseType,
    includeSubject: Boolean,
    subject: ArgumentPosition[Argument.Subject],
    modifiers: Vector[ArgumentPosition[Argument.NonNominal]],
    tan: TAN
  ) extends ArgumentProForm[Argument.Complement] {
    // TODO: need to be able to render the 'gap'
    def wh = Some("what".lowerCase)
    def placeholder = {
      val pred = Predication.Verbal.doSomething(subject, modifiers, tan)
      import ClauseType._
      clauseType match {
        case Attributive =>
          if(includeSubject) None // not allowed for attributives
          else Some(Argument.Attributive(Some(pred)))
        case Infinitive => Some(Argument.Infinitive(includeSubject, Some(pred)))
        case Progressive => Some(Argument.Progressive(includeSubject, Some(pred)))
        case Finite =>
          if(includeSubject) Some(Argument.Finite(Some(pred)))
          else None // need subject for finite complements
        case Bare =>
          if(includeSubject) None // not allowed for bare verbs; no 'small clauses'
          else Some(Argument.Bare(Some(pred)))
      }
    }
  }
  // maybe also add a copular pro-form? already handled for Copula
  // but missing for adjective and passive. not sure how gapping should work with it.
  // could also add a 'happen' pro-form
}

// TODO: probably break apart the hierarchy once I know what error reporting
// will actually look like.
sealed trait Component {
  protected[Component] def leafBranch(pair: (String, String)): Component.RenderResult = {
    Validated.valid(LabeledTree.leaves(pair))
  }
  protected[Component] def leaf(leaf: String): Component.RenderResultOf[LabeledTreeChild[String, String]] = {
    Validated.valid(LabeledTreeLeaf(leaf))
  }
  protected[Component] def branch(label: String, content: Component.RenderResult) = {
    content.map(res => LabeledTree(label -> res))
  }
  protected[Component] def error(msg: String) = Component.RenderError(this, msg)
  protected[Component] def invalid[A](
    msg: String = "Incomplete component."
  ): Component.RenderResultOf[A] =
    Validated.invalid(NonEmptyChain.one(error(msg)))
}
object Component {
  case class RenderError(component: Component, msg: String)
  type RenderResultOf[A] = ValidatedNec[RenderError, A]
  type RenderResult = ValidatedNec[RenderError, LabeledTree[String, String]]
}

// argument structure (with slots etc.)
sealed trait Argument extends Component {
  def symbol: String
  def render: Component.RenderResultOf[LabeledTreeChild[String, String]]
}
object Argument {
  import Component._
  import Lexicon._
  import ArgumentProForm._
  // can appear as subject
  sealed trait Subject extends Argument {
    // def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]
  }
  // can appear as non-subject argument of a verb
  sealed trait NonSubject extends Argument
  // can appear as argument/adjunct of an adjective or adjunct of a copula
  // (includes obliques, complements, and adverbials)
  sealed trait NonNominal extends NonSubject
  // can be turned into a subordinate clause
  sealed trait Complement extends NonNominal
  // can be used predicatively with a copula (is not verb or adjective)
  sealed trait NounOrOblique extends NonSubject
  // can appear as the object of a preposition
  sealed trait Nominal extends Subject with NonSubject
  // can appear with an adverbial wh-word: excludes full complements from NonNominal
  // sealed trait Adverbial extends NonNominal

  // TODO: RenderContext:
  // tells us whether we are rendering an argument...
  // -- in subject position (for nom/accus, preventing in-context subj use of complements)
  // -- in an answer (for bare vp answers to 'what..to do' questions
  // -- in an argument position?

  // expletive 'it' or 'there'; can only appear as subject
  case class Expletive(expletive: Lexicon.Expletive) extends Subject {
    def symbol = s"np[${expletive.form}]"
    // def typ = None
    // def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
    override def render = leaf(expletive.form.toString)
  }
  object Expletive {
    val it = Expletive(Lexicon.Expletive.it)
    val there = Expletive(Lexicon.Expletive.there)
  }

  case class NounPhrase(
    pred: Option[Predication.Nominal]
    // animate: Option[Boolean]
  ) extends Nominal {
    def symbol = "np"
    override def render = {
      // def validLeaf(x: String) = Validated.valid(LabeledTree.leaves("np" -> x))
      pred.map(_.form).map(leaf)
        // .orElse(animate.map(_.fold("someone", "something")).map(validLeaf(_)))
        .getOrElse(invalid())
    }
    // def typ: Option[IndefinitePronoun] = animate.map(_.fold(Who, What))
    def person: Option[Person] = pred.flatMap(_.person)
      // .getOrElse(animate.map(_ => Person.Third))
    def number: Option[Number] = pred.flatMap(_.number)
      // .getOrElse(animate.map(_ => Number.Singular))
  }
  object NounPhrase {
    def something = NounPhrase(Some(Predication.Nominal.something))
    def someone = NounPhrase(Some(Predication.Nominal.someone))
  }

  case class Oblique(
    pred: Option[Predication.Oblique]
  ) extends NounOrOblique with NonNominal {
    def symbol = "pp"
    override def render: RenderResult = {
      pred match {
        case None => invalid()
        case Some(Predication.Particulate(prep)) =>
          leafBranch("prt" -> prep.toString)
        case Some(Predication.Prepositional(prep, obj)) => obj match {
          case None => invalid()
          case Some(nominal) => List(
            leafBranch("prep" -> prep.toString),
            nominal.render
          ).foldA
        }
      }
    }
  }


  // nominal use of gerund/progressive; can be object of prep or subject
  case class Gerund(
    includeSubject: Boolean,
    pred: Option[Predication]
      // TODO possessive subject option
  ) extends Nominal {
    def symbol = if(includeSubject) "s[g]" else "vp[g]"

    override def render: Component.RenderResult =
      pred.map(_.render(ClauseType.Progressive, includeSubject))
        .getOrElse(invalid())
        // .getOrElse(pro.render.map(p => LabeledTree.leaves(symbol -> p)))

    // def animate = Some(false)
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
  }

  case class Bare(
    // includeSubject: Boolean,
    pred: Option[Predication.NonCopular]
  ) extends Complement {
    def symbol = "vp[b]"
    def render =
      pred.map(_.render(ClauseType.Bare, false))
        .getOrElse(invalid())
        // .orElse(pro.map(_.render.map(p => LabeledTree.leaves(symbol -> p))))
  }

  // various possible forms of complements:
  // may be specific to a predicate's (or subordinator's!) subcat frame.
  case class Infinitive(
    includeSubject: Boolean,
    // pro: Option[IndefiniteProForm],
    pred: Option[Predication]
  ) extends Complement with Subject {
    // TODO add for-complementizer
    def symbol = if(includeSubject) "s[to]" else "vp[to]"

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def render =
      pred.map(_.render(ClauseType.Infinitive, includeSubject))
        .getOrElse(invalid())
        // .orElse(pro.map(_.render.map(p => LabeledTree.leaves(symbol -> p))))
  }

  case class Progressive(
    includeSubject: Boolean,
    // pro: Option[IndefiniteAdverbial],
    pred: Option[Predication]
  ) extends Complement {
    def symbol = if(includeSubject) "s[ng]" else "vp[ng]"
    def render =
      pred.map(_.render(ClauseType.Progressive, includeSubject))
        .getOrElse(invalid())
        // .orElse(pro.map(_.render.map(p => LabeledTree.leaves(symbol -> p))))
  }

  // NOTE: maybe 'how' is the only acceptable pro-form here?
  // i guess 'what' appears for adjectives...
  case class Attributive(
    // includeSubject: Boolean,
    pred: Option[Predication.NonCopular]
    // pro: Option[IndefiniteProForm],
  ) extends Complement {
    // def symbol = if(includeSubject) "s[pt]" else "vp[pt]"
    def symbol = "vp[pt]"
    def render =
      pred.map(_.render(ClauseType.Attributive, false))
        .getOrElse(invalid())
        // .orElse(pro.map(_.render.map(p => LabeledTree.leaves(symbol -> p))))
  }

  case class Finite(
    pred: Option[Predication]
  ) extends Complement { // with Subject? but cannot appear in subj except as an answer
    // def pro = What
    def symbol = "s[dcl]"
    def render =
      pred.map(_.render(ClauseType.Finite, true))
        .getOrElse(invalid())
        // .getOrElse(pro.render.map(p => LabeledTree.leaves(symbol -> p)))
  }

  // is this always necessarily 'that'? maybe remove complementizer slot?
  case class FiniteComplement(
    complementizer: Complementizer,
    pred: Option[Predication],
  ) extends Complement with Subject {

    // def pro = What
    def symbol = "s[comp]"

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def render =
      pred.map(p =>
        List(
          leafBranch("comp" -> complementizer.form.toString),
          p.render(ClauseType.Finite, true)
        ).foldA.map(children => LabeledTree(symbol -> children))
      ).getOrElse(invalid())
      // ).getOrElse(pro.render.map(p => LabeledTree.leaves(symbol -> p)))
  }

  // TODO: add predication structure to subordinators/adverbials
  case class SubordinateClause(
    subordinator: Subordinator,
    clause: ArgumentPosition[Complement]
  ) extends NonNominal {
    def symbol = "advcl"
    def render = List(
      Validated.valid(LabeledTree.leaves("sub" -> subordinator.form.toString)),
      clause.render
    ).foldA
  }

  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    form: Option[String]
  ) extends NonNominal {
    def symbol = "adv"
    override def render = {
      form.map(leaf).getOrElse(invalid("Missing adverbial form."))
    }
  }
  object Adverbial {
    val somewhere = Adverbial(Some("somewhere"))
  }
}

sealed trait Predication extends Component {
  def subject: ArgumentPosition[Argument.Subject]
  def render(clauseType: ClauseType, includeSubject: Boolean): Component.RenderResult
  def tan: TAN

  def renderSubject(includeSubject: Boolean) = {
    if(!includeSubject) {
      Validated.valid(Monoid[LabeledTree[String, String]].empty)
    } else subject.render
  }
}
object Predication {
  import Component._
  import Lexicon._
  import ArgumentProForm._
  import Argument._

  // doesn't extend Predication because we don't need to use it that way yet.
  // in the future, with proper nominal predicates, might use it this way.
  // but it would be a separate (non-clausal) type of predication.
  case class Nominal(
    form: String,
    person: Option[Person],
    number: Option[Number]
  )
  object Nominal {
    val something = Nominal("something", Some(Person.Third), Some(Number.Singular))
    val someone = Nominal("someone", Some(Person.Third), Some(Number.Singular))
  }

  sealed trait Oblique
  // prep should not take an object, i.e., is a particle
  case class Particulate(form: Particle) extends Oblique
  // prep should take an object, which may not be specified
  case class Prepositional(
    prep: Preposition,
    obj: Option[ArgumentPosition[Argument.Nominal]]
  ) extends Oblique

  // 'something is done'.
  // maybe can add this in later.
  // case class PassiveProForm(
  //   subject: Subject, --- seems too general; maybe not best to use as pro-form?
  //   arguments: Vector[NonSubject] = Noun()
  //   tan: TAN
  // ) {
  //   // verb: Verb = Verb(InflectedForms.doForms)
  // }
  // maybe also 'what happened _?' type of pro-form?
  // not as necessary since it isn't needed for constructing questions in existing framework

  case class Copular(
    subject: ArgumentPosition[Argument.Subject],
    argument: ArgumentPosition[NounOrOblique],
    modifiers: Vector[ArgumentPosition[NonNominal]],
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
    subject: ArgumentPosition[Argument.Subject],
    adjective: Adjective,
    arguments: Vector[ArgumentPosition[NonNominal]],
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
    subject: ArgumentPosition[Argument.Subject],
    verb: Verb,
    isPassive: Boolean,
    arguments: Vector[ArgumentPosition[NonSubject]],
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
  object Verbal {
    def doSomething(
      subject: ArgumentPosition[Argument.Subject],
      modifiers: Vector[ArgumentPosition[NonNominal]],
      tan: TAN
    ) = Verbal(
      subject, Verb(InflectedForms.doForms), false,
      ArgumentPosition(Some(ArgumentProForm.what), None) +: modifiers,
      tan
    )
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
