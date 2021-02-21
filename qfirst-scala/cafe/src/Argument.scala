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
import cats.data.NonEmptyList

// S4: Simple Surrogate Syntactic Semantics

// SASS: Semantic Annotation via Surrogate Syntax

sealed trait ClauseType
object ClauseType {
  // past participle form --- passive/adjective
  case object Attributive extends ClauseType
  sealed trait VerbalClauseType extends ClauseType
  case object BareInfinitive extends VerbalClauseType
  case object ToInfinitive extends VerbalClauseType
  case object Progressive extends VerbalClauseType
  case object Finite extends VerbalClauseType
  case object Inverted extends VerbalClauseType

  val all = List(Attributive, BareInfinitive, ToInfinitive, Progressive, Finite, Inverted)
}


// tells us whether we are rendering an argument...
// TODO: in an answer (for bare vp answers to 'what..to do' questions
// TODO: 'Fronted' position? something for when extracted
sealed trait ArgPosition
object ArgPosition {
  // for wh-words/phrases
  // case object Focus extends RenderContext
  // for extraction gaps
  // case object Gap extends RenderContext

  // sealed trait InPlace extends RenderContext
  // normal argument position (accusative if nominal)
  case class Arg(index: Int) extends ArgPosition
  // for nominative case with subjects
  case object Subj extends ArgPosition
}

sealed trait ExtractionPath {
  import ExtractionPath._
  def hasExtraction: Boolean = this match {
    case Descent(_, next) => next.hasExtraction
    case Focus => false
    case Extraction(_) => true
  }
}
object ExtractionPath {
  case class Descent(position: ArgPosition, next: ExtractionPath) extends ExtractionPath
  case object Focus extends ExtractionPath
  case class Extraction(next: ExtractionPath) extends ExtractionPath

  def descend(path: Option[Descent], pos: ArgPosition): Option[ExtractionPath] =
    path.collect { case Descent(`pos`, next) => next }
}

// Do-pro-forms.
// work these in by tying frames together.

  // case class GerundiveDoingSomething(
  //   includeSubject: Boolean, // TODO add possessive version
  //   subject: Argument.Subject,
  //   modifiers: Vector[Argument.NonNominal],
  //   tan: TAN
  // ) extends ArgumentProForm[Argument.Gerund] {
  //   // TODO: need to be able to render the 'gap'
  //   // def wh = Some("what".lowerCase)
  //   def clause = {
  //     val pred = Predication.Verbal.doSomething(subject, modifiers, tan)
  //     Argument.Gerund(includeSubject, Some(pred))
  //   }
  //   def arg = Some(clause)
  //   import RenderContext._
  //   def render(ctx: RenderContext) = ctx match {
  //     // TODO: maybe disallow in subject position?
  //     case Arg | Subj => clause.render(ctx)
  //     case Focus => leaf("what".lowerCase)
  //     case Gap => ??? // clause.render(???) // TODO: propagate gap down
  //   }
  // }

  // // 'someone does something'
  // case class DoSomething(
  //   clauseType: ClauseType,
  //   includeSubject: Boolean,
  //   subject: Argument.Subject,
  //   modifiers: Vector[Argument.NonNominal],
  //   tan: TAN
  // ) extends ArgumentProForm[Argument.Complement] {
  //   def clause = {
  //     val pred = Predication.Verbal.doSomething(subject, modifiers, tan)
  //     import ClauseType._
  //     clauseType match {
  //       case Attributive =>
  //         if(includeSubject) invalid("Attributives cannot appear with a subject.")
  //         else Validated.valid(Argument.Attributive(Some(pred)))
  //       case ToInfinitive =>
  //         Validated.valid(Argument.ToInfinitive(includeSubject, Some(pred)))
  //       case Progressive =>
  //         Validated.valid(Argument.Progressive(includeSubject, Some(pred)))
  //       case Finite =>
  //         if(includeSubject) Validated.valid(Argument.Finite(Some(pred)))
  //         else invalid("Finite complements require a subject.")
  //       case BareInfinitive =>
  //         if(includeSubject) invalid("BareInfinitive do-clauses cannot appear with a subject.")
  //         else Validated.valid(Argument.BareInfinitive(Some(pred)))
  //     }
  //   }
  //   def arg = clause.toOption
  //   import RenderContext._
  //   def render(ctx: RenderContext) = ctx match {
  //     // TODO: maybe disallow in subject position?
  //     case Arg | Subj => clause.toEither.flatMap(_.render(ctx).toEither).toValidated
  //     case Focus => leaf("what".lowerCase)
  //     case Gap => ??? // clause.render(???) // TODO: propagate gap down
  //   }
  // }
  // // maybe also add a copular pro-form? already handled for Copula
  // // but missing for adjective and passive. not sure how gapping should work with it.
  // // could also add a 'happen' pro-form

// TODO: probably break apart the hierarchy once I know what error reporting
// will actually look like.
sealed trait Component extends Product with Serializable {
  // protected[Component] def leafBranch(
  //   pair: (String, String)
  // ): Component.RenderResultOf[LabeledTree[String, String]] = {
  //   Validated.valid(LabeledTree.leaves(pair))
  // }
  // protected[Component] def leaf(leaf: String): Component.RenderResultOf[LabeledTree[String, String]] = {
  //   Validated.valid(LabeledTree.Leaf(leaf))
  // }
  // protected[Component] def branch(label: String, content: Component.RenderResult) = {
  //   content.map(res => LabeledTree.node(label -> res))
  // }
  protected[Component] def error(msg: String) = Component.RenderError(this, msg)
  protected[Component] def invalid[A](
    msg: String = "Incomplete component."
  ): ValidatedNec[Component.RenderError, A] =
    Validated.invalid(NonEmptyChain.one(error(msg)))
}
object Component {
  case class WithExtraction[+A](
    value: A,
    extractions: Vector[(Argument, ExtractionPath)] = Vector()
  ) {
    def map[B](f: A => B) = WithExtraction(f(value), extractions)
  }
  object WithExtraction {
    implicit def withExtractionMonoid[A: Monoid] = new Monoid[WithExtraction[A]] {
      def empty = WithExtraction(Monoid[A].empty)
      def combine(x: WithExtraction[A], y: WithExtraction[A]): WithExtraction[A] = {
        WithExtraction(x.value |+| y.value, x.extractions ++ y.extractions)
      }
    }
  }
  case class RenderError(component: Component, msg: String)
  type RenderResultOf[A] = ValidatedNec[RenderError, A]
  // type RenderResult = ValidatedNec[RenderError, LabeledTree.Node[String, String]]
  type RenderResult = RenderResultOf[WithExtraction[LabeledTree.Node[String, String]]]
}

// argument structure (with slots etc.)
sealed trait Argument extends Component {
  def render(pos: ArgPosition, path: Option[ExtractionPath]): Component.RenderResult
  def render(pos: ArgPosition): ValidatedNec[Component.RenderError, LabeledTree.Node[String, String]] =
    render(pos, None).map(_.value) // assume there are no extractions
}
object Argument {
  import Component._
  import Lexicon._
  import LabeledTree.{leaf, leaves, node}
  import Validated.valid

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

  sealed trait ProForm extends Argument {
    def wh(pos: ArgPosition): String
    def placeholder: Option[String]

    def symbol = s"{${wh(ArgPosition.Subj)}}"
    // def indefinite = Some(this)

    import ArgPosition._, ExtractionPath._
    def render(pos: ArgPosition, path: Option[ExtractionPath]): RenderResult = path match {
      case Some(Extraction(path)) =>
        valid(
          WithExtraction(
            // leaves(symbol -> ""),
            leaves(),
            Vector(this -> path)
          )
        )
      case Some(Focus) =>
        valid(WithExtraction(leaves(symbol -> wh(pos))))
      case Some(Descent(_, _)) => invalid("Cannot extract from inside a pro-form.")
      case None => placeholder
          .map(p => valid(WithExtraction(leaves(symbol -> p))))
          .getOrElse(invalid("No placeholder exists for pro-form."))
    }
  }
  object ProForm {
    sealed trait Noun extends ProForm with Nominal with NounOrOblique {
      // def wh: Lexicon.Wh.Noun
    }
    case object who extends Noun {
      def wh(pos: ArgPosition) = pos match {
        case ArgPosition.Subj => "who"
        case _ => "whom"
      }
      val placeholder = Some("someone")
      val number = Some(Number.Singular)
      val person = Some(Person.Third)
    }
    case object what extends Noun with Complement {
      def wh(pos: ArgPosition) = "what"
      val placeholder = Some("something")
      val number = Some(Number.Singular)
      val person = Some(Person.Third)
    }
    sealed trait Adverb extends ProForm with NonNominal with NounOrOblique {
      def wh(pos: ArgPosition) = whWord.form.toString
      def whWord: Lexicon.Wh.Adverb
    }
    case object where extends Adverb {
      val whWord = Wh.where
      val placeholder = Some("somewhere")
    }
    case object when extends Adverb {
      val whWord = Wh.when
      val placeholder = None
    }
    object Adverb {
      def fromWh(wh: Lexicon.Wh.Adverb) = all.find(_.whWord == wh).get
      def all = List(where, when)
    }
    // TODO: consider adding 'do' as a pro-form, AFTER figuring out
    // extraction, question formation, AND answer filling for the simple case.
  }

  sealed trait Concrete extends Argument {
    def category: String
    def proForms: Set[Argument.ProForm]
    // TODO might not need parameter
    def allowExtraction(path: ExtractionPath): Boolean

    final def symbol = {
      val pros = proForms.map(_.wh(ArgPosition.Subj)).toList.sortBy(_.toString).mkString("/")
      if(pros.isEmpty) category
      else s"$category {$pros}"
    }

    import ArgPosition._, ExtractionPath._
    def render(pos: ArgPosition, path: Option[ExtractionPath]): RenderResult = path match {
      case Some(Extraction(path)) =>
        if(allowExtraction(path)) valid(
          WithExtraction(
            leaves(symbol -> ""),
            Vector(this -> path)
          )
        ) else invalid("Extraction of argument not allowed.")
      case Some(Focus) => invalid("Focal element must be a pro-form.")
      case Some(d @ Descent(_, _)) =>
        renderStrict(pos, Some(d)).map(_.map(tree => node(symbol -> tree)))
      case None =>
        renderStrict(pos, None).map(_.map(tree => node(symbol -> tree)))
    }

    def renderStrict(
      pos: ArgPosition, path: Option[Descent]
    ): RenderResultOf[Component.WithExtraction[LabeledTree[String, String]]]
  }

  // expletive 'it' or 'there'; can only appear as subject
  case class Expletive(expletive: Lexicon.Expletive) extends Concrete with Subject {

    def allowExtraction(path: ExtractionPath): Boolean = false
    def category = s"np[${expletive.form}]"
    def proForms = Set()
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) = path match {
      case Some(_) => invalid("Cannot extract from inside an expletive.")
      case None => pos match {
        case Subj => valid(WithExtraction(leaf(expletive.form.toString)))
        case Arg(_) => invalid("Can only render expletives in subject position.")
      }
    }
  }
  object Expletive {
    val it = Expletive(Lexicon.Expletive.it)
    val there = Expletive(Lexicon.Expletive.there)
  }

  // NOTE: predications are optional because we can use arguments
  // to specify general syntactic subcategorization frames.

  case class NounPhrase(
    pred: Option[Predication.Nominal],
  ) extends Concrete with Nominal {

    override def allowExtraction(path: ExtractionPath): Boolean = true

    def category = "np"
    // TODO: change to account for multiple possible animacies
    def proForms =
      animate.map(anim => if(anim) ProForm.who else ProForm.what)
        .toSet

    def animate: Option[Boolean] = pred.flatMap(_.animate)
    def person: Option[Person] = pred.flatMap(_.person)
    def number: Option[Number] = pred.flatMap(_.number)

    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) = {
      val c = pos match {
        case Subj => Case.Nominative
        case Arg(_) => Case.Accusative
      }
      pred.map(_.render(c, path)).getOrElse(invalid())
    }
  }
  object NounPhrase

  case class Oblique(
    pred: Option[Predication.Oblique],
    adverbials: Set[ProForm.Adverb]
  ) extends Concrete with NounOrOblique with NonNominal {

    override def allowExtraction(path: ExtractionPath): Boolean = true

    def category = "pp"
    def proForms = adverbials.map(x => x: ProForm)

      // TODO: allowed in Fronted position. should we put this back in? idk
      // case Subj => invalid("Oblique argument cannot appear in subject position.")
      // case Arg(_) =>
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(_.render(path)).getOrElse(invalid())
  }

  // TODO: island constraints
  case class Subordinate(
    pred: Option[Predication.Subordinate],
    adverbials: Set[ProForm.Adverb]
  ) extends Concrete with NonNominal {
    override def category = "adv[comp]"
    override def proForms = adverbials.map(x => x: ProForm)
    // TODO might depend on the predicate?
    override def allowExtraction(path: ExtractionPath) = true
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(_.render(path)).getOrElse(invalid())
  }

  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    pred: Option[Predication.Adverbial],
    adverbials: Set[ProForm.Adverb]
  ) extends Concrete with NonNominal {
    override def category = "adv"
    override def proForms = adverbials.map(x => x: ProForm)
    override def allowExtraction(path: ExtractionPath) = false
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(_.render(path)).getOrElse(invalid())
  }

  sealed trait Clausal extends Concrete {
    def pred: Option[Predication]
    def clauseType: ClauseType
    def includeSubject: Boolean
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(_.render(clauseType, includeSubject, path))
        .getOrElse(invalid())
  }

  // nominal use of gerund/progressive; can be object of prep or subject
  // TODO possessive subject option
  case class Gerund(
    pred: Option[Predication],
    includeSubject: Boolean
  ) extends Concrete with Nominal with Clausal {
    override def clauseType = ClauseType.Progressive
    override def category = if(includeSubject) "s[g]" else "vp[g]"
    override def proForms = Set(ProForm.what)
    override def allowExtraction(path: ExtractionPath) = true
    override def person = Some(Person.Third)
    override def number = Some(Number.Singular)
  }

  // includeSubject: Boolean,
  case class BareInfinitive(
    pred: Option[Predication.NonCopular]
  ) extends Concrete with Complement with Clausal {
    override def clauseType = ClauseType.BareInfinitive
    override def includeSubject = false
    override def category = "vp[b]"
    override def proForms = Set()
    override def allowExtraction(path: ExtractionPath) = false
  }

  // various possible forms of complements:
  // may be specific to a predicate's (or subordinator's!) subcat frame.
  // TODO: fix up infinitives. all sorts of syntactic possibilities here.
  // Not sure what will be best.
  case class ToInfinitive(
    pred: Option[Predication],
    includeSubject: Boolean,
    // pro: Option[ProFormProForm],
    proForms: Set[ProForm]
  ) extends Concrete with Complement with Clausal {
    // TODO: with Subject. or, just add a new argument type for subj inf?
    def complementizer: InfinitiveComplementizer = Lexicon.InfinitiveComplementizer.`for`

    override def clauseType = ClauseType.ToInfinitive
    override def category = if(includeSubject) "s[for-to]" else "vp[to]"
    // def indefinite = if(includeSubject) ProForm.what else ...
    override def allowExtraction(path: ExtractionPath) = false

    // override def person = Some(Person.Third)
    // override def number = Some(Number.Singular)

    // TODO clausal override not that useful here. maybe separate out versions
    // with and without subject? idk
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(p =>
        List( // TODO path
          if(includeSubject) valid(WithExtraction(leaves("comp" -> complementizer.form.toString)))
          else Validated.Valid(WithExtraction(LabeledTree.Node[String, String](Vector()))),
          p.render(ClauseType.ToInfinitive, includeSubject, path)
        ).foldA
      ).getOrElse(invalid())
  }

  case class Progressive(
    pred: Option[Predication],
    includeSubject: Boolean,
    adverbials: Set[ProForm.Adverb]
  ) extends Concrete with Complement with Clausal {
    override def clauseType = ClauseType.Progressive
    override def category = if(includeSubject) "s[ng]" else "vp[ng]"
    override def proForms = adverbials.map(x => x: ProForm)
    override def allowExtraction(path: ExtractionPath) = false
  }

  // NOTE: maybe 'how' is the only acceptable pro-form here?
  // i guess 'what' appears for adjectives...
  // can subjects appear? she wants Ben happy. What does she want? Ben happy. idk.
  // TODO: probably add subjects back in. maybe be lenient.
  // includeSubject: Boolean,
  case class Attributive(
    pred: Option[Predication.NonCopular],
    proForms: Set[ProForm]
  ) extends Concrete with Complement with Clausal {
    override def clauseType = ClauseType.Attributive
    override def includeSubject = false
    // def category = if(includeSubject) "s[pt]" else "vp[pt]"
    override def category = "vp[adj/pt]"
    override def allowExtraction(path: ExtractionPath) = false
  }

  case class Finite(
    pred: Option[Predication]
  ) extends Concrete with Complement with Clausal {
    // with Subject? but cannot appear in subj except as an answer
    override def clauseType = ClauseType.Finite
    override def includeSubject = true
    override def category = "s[dcl]"
    override def proForms = Set(ProForm.what)
    override def allowExtraction(path: ExtractionPath) = false
  }

  // is this always necessarily 'that'? maybe remove complementizer slot?
  // TODO add 'that'
  case class FiniteComplement(
    pred: Option[Predication],
  ) extends Concrete with Complement with Subject with Clausal {
    override def clauseType = ClauseType.Finite
    override def includeSubject = true

    override def category = "s[comp]"
    override def proForms = Set(ProForm.what)
    override def allowExtraction(path: ExtractionPath) = false

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def complementizer: Complementizer = Lexicon.Complementizer.that

    // not sure if Clausal override is useful here
    import ArgPosition._, ExtractionPath.Descent
    override def renderStrict(pos: ArgPosition, path: Option[Descent]) =
      pred.map(p =>
        List(  // TODO path
          valid(WithExtraction(leaves("comp" -> complementizer.form.toString))),
          p.render(clauseType, includeSubject, path)
        ).foldA.map(_.map(tree => LabeledTree.Node(symbol -> tree)))
      ).getOrElse(invalid())
  }
}

sealed trait Predication extends Component {
  def subject: Argument.Subject
  def tan: TAN
  def arguments: Vector[Argument]

  def predPOS: String
  // TODO: change this to 1) take a param to possibly ensure
  // do-support, and 2) return a NonEmptyList verb chain instead.
  // this will allow us to do auxiliary flip.
  def renderVerbChain(
    clauseType: ClauseType,
    needsFlippable: Boolean
  ): Either[NonEmptyChain[String], NonEmptyList[String]]
  // tan.getCopulaAuxChain(otherType, subject)
          // LabeledTree.leaves("aux" -> verbChain.init.mkString(" ")) |+|
          //   LabeledTree.leaves("verb" -> verbChain.last)

  import Validated.valid
  import LabeledTree.{leaf, leaves, node}
  import Component.WithExtraction

  def renderQuestion(
    path: Option[ExtractionPath.Descent]
  ): Component.RenderResultOf[LabeledTree[String, String]] = {
    val clauseType =
      if(path.forall(_.hasExtraction)) ClauseType.Inverted
      else ClauseType.Finite
    render(clauseType, includeSubject = true, path = path).andThen {
      case WithExtraction(tree, extractions) =>
        extractions.headOption match {
          case None => valid(tree)
          case Some((arg, path)) =>
            arg.render(ArgPosition.Subj, Some(path)).andThen {
              case WithExtraction(argTree, argExtractions) =>
                if(argExtractions.nonEmpty) invalid("Should not have a nested extraction.")
                else valid(argTree |+| tree)
            }
        }
    }
  }

  def render(
    clauseType: ClauseType,
    includeSubject: Boolean,
  ): Component.RenderResultOf[LabeledTree[String, String]] = {
    render(clauseType, includeSubject, None).map(_.value)
  }

  def render(
    clauseType: ClauseType,
    includeSubject: Boolean,
    path: Option[ExtractionPath.Descent],
  ): Component.RenderResult = {
    val frontAuxiliary = clauseType == ClauseType.Inverted
    validatePath(includeSubject, path) *> {
      renderSubject(includeSubject, path).andThen { subjValue =>
        val flipAuxiliary = frontAuxiliary && subjValue.value.nonEmpty
        val subj = valid(subjValue)
        val args = renderArguments(path)
        def makeVerbTree(chain: NonEmptyList[String]) = {
          if(chain.size == 1) leaves(predPOS -> chain.head)
          else leaves(
            "aux" -> chain.init.mkString(" "),
            predPOS -> chain.last
          )
        }
        // TODO: consider result of rendering subj (i.e., if it's a gap)
        // when deciding whether to invert the auxiliary
        Validated.fromEither(
          renderVerbChain(clauseType, flipAuxiliary).left.map(_.map(error(_)))
        ).andThen { verbChain =>
          if(flipAuxiliary) { // aux flip
            val aux = valid(WithExtraction(leaves("aux" -> verbChain.head)))
            NonEmptyList.fromList(verbChain.tail) match {
              case None =>
                List(aux, subj, args).foldA
              case Some(verbTail) =>
                val verb = valid(WithExtraction(makeVerbTree(verbTail)))
                List(aux, subj, verb, args).foldA
            }
          } else {
            val verb = valid(WithExtraction(makeVerbTree(verbChain)))
            if(includeSubject) List(subj, verb, args).foldA
            else List(verb, args).foldA
          }
        }.andThen(res =>
          if(res.extractions.size > 1) {
            invalid("Somehow produced multiple extractions.")
          } else if(res.extractions.nonEmpty && path.isEmpty) {
            invalid("Produced spurious extractions.")
          } else valid(res)
        )
      }
    }
  }

  def validatePath(
    includeSubject: Boolean,
    path: Option[ExtractionPath.Descent]
  ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
    case None => valid(())
    case Some(ArgPosition.Subj) if includeSubject => valid(())
    case Some(ArgPosition.Arg(i)) if arguments.size > i => valid(())
    case Some(pos) => invalid(s"Cannot descent into argument position for extraction: $pos")
  }

  def renderSubject(includeSubject: Boolean, path: Option[ExtractionPath.Descent]): Component.RenderResult = {
    if(!includeSubject) {
      Validated.valid(Monoid[Component.WithExtraction[LabeledTree.Node[String, String]]].empty)
    } else subject.render(ArgPosition.Subj, ExtractionPath.descend(path, ArgPosition.Subj))
  }

  def renderArguments(path: Option[ExtractionPath.Descent]): Component.RenderResult = {
    arguments.zipWithIndex.foldMapA { case (arg, index) =>
      arg.render(ArgPosition.Arg(index), ExtractionPath.descend(path, ArgPosition.Arg(index)))
    }
  }

}
object Predication {
  import Component._
  import Lexicon._
  import Argument._
  import Validated.valid
  import LabeledTree.{node, leaves, leaf}

  // doesn't extend Predication because we don't need to use it that way yet.
  // in the future, with proper nominal predicates, might use it this way.
  // but it would be a separate (non-clausal) type of predication.
  sealed trait Nominal extends Component {
    def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]

    // def arguments = Vector()

    def getForm(`case`: Case): String

    def render(
      `case`: Case,
      path: Option[ExtractionPath.Descent]
    ): Component.RenderResultOf[WithExtraction[LabeledTree[String, String]]] = path match {
      case Some(_) => invalid("Cannot descend into a nominal predication for extraction (for now).")
      case None => valid(WithExtraction(leaf(getForm(`case`))))
    }
  }

  case class NounPhrase(
    form: String,
    animate: Option[Boolean],
    person: Option[Person],
    number: Option[Number]
  ) extends Nominal {
    def getForm(c: Case): String = c match {
      case Case.Genitive => form + "'s"
      case _ => form
    }
  }
  object NounPhrase
  // TODO: add definite pronouns

  sealed trait Oblique extends Component {
    import ArgPosition._, ExtractionPath.Descent
    def render(path: Option[Descent]): Component.RenderResult
  }
  // prep should not take an object, i.e., is a particle
  case class Particulate(form: Particle) extends Oblique {
    // def arguments = Vector()
    import ArgPosition._, ExtractionPath.Descent
    override def render(path: Option[Descent]): Component.RenderResult = path match {
      case Some(_) => invalid("Cannot extract from inside a particle.")
      case None => valid(WithExtraction(leaves("prt" -> form.toString)))
    }
  }
  // prep should take an object, which may not be specified
  case class Prepositional(
    prep: Preposition,
    obj: Argument.Nominal
  ) extends Oblique {
    // def arguments = Vector(obj)
    import ArgPosition._, ExtractionPath.Descent
    override def render(path: Option[Descent]): Component.RenderResult = {
      path match {
        case Some(pos) if pos != Arg(0) =>
          invalid(s"Can't descend into argument for extraction from oblique: $pos")
        case otherPath => List(
          valid(WithExtraction(leaves("prep" -> prep.toString))),
          obj.render(Arg(0), ExtractionPath.descend(path, Arg(0)))
        ).foldA
      }
    }
  }

  case class Adverbial(form: String) extends Component {
    def render(
      path: Option[ExtractionPath.Descent]
    ): Component.RenderResultOf[WithExtraction[LabeledTree[String, String]]] = path match {
      case Some(_) => invalid("Cannot descend into an adverbial for extraction (for now).")
      case None => valid(WithExtraction(leaf(form)))
    }
  }

  case class Subordinate(
    subordinator: Subordinator,
    clause: Argument.Complement
  ) extends Component {
    import ExtractionPath.Descent, ArgPosition._
    def render(path: Option[Descent]): Component.RenderResult = {
      path match {
        case Some(pos) if pos != Arg(0) =>
          invalid(s"Can't descend into argument for extraction from oblique: $pos")
        case otherPath => List(
          valid(WithExtraction(leaves("sub" -> subordinator.form.toString))),
          clause.render(Arg(0), ExtractionPath.descend(path, Arg(0)))
        ).foldA
      }
    }
  }

  // 'something is done'.
  // maybe can add this in later.
  // case class PassiveProForm(
  //   subject: Subject, --- seems too general; maybe not best to use as pro-form?
  //   arguments: Vector[NonSubject] = Noun()
  //   tan: TAN
  // ) {
  //   // verb: Verb = Verb(InflectedForms.doForms)
  // }
  // not as necessary since it isn't needed for constructing questions in existing framework

  case class Copular(
    subject: Argument.Subject,
    argument: NounOrOblique,
    modifiers: Vector[NonNominal],
    tan: TAN
  ) extends Predication {
    def arguments = argument +: modifiers
    def predPOS = "be"

    override def renderVerbChain(
      clauseType: ClauseType,
      needsFlippable: Boolean
    ): Either[NonEmptyChain[String], NonEmptyList[String]] = clauseType match {
      case ClauseType.Attributive =>
        Left(NonEmptyChain.one("Cannot construct attributive clause from a copula."))
      case otherType: ClauseType.VerbalClauseType =>
        tan.getCopulaAuxChain(otherType, subject)
          // .left.map(_.map(error(_)))
          // .map(auxes => WithExtraction(LabeledTree.leaves("aux" -> auxes.toList.mkString(" "))))
    }
  }

  sealed trait NonCopular extends Predication

  case class Adjectival(
    subject: Argument.Subject,
    adjective: Adjective,
    arguments: Vector[NonNominal],
    tan: TAN
  ) extends NonCopular {
    override def predPOS = "adj"
    override def renderVerbChain(clauseType: ClauseType, needsFlippable: Boolean) = {
      val adj = adjective.form.toString
      clauseType match {
        case ClauseType.Attributive => Right(NonEmptyList.of(adj))
        case otherType: ClauseType.VerbalClauseType =>
          val aux = tan.getCopulaAuxChain(otherType, subject)
          aux.map(_.append(adj))
      }
    }
  }

  case class Verbal(
    subject: Argument.Subject,
    verb: Verb,
    isPassive: Boolean,
    arguments: Vector[NonSubject],
    tan: TAN
  ) extends NonCopular {
    override def predPOS = "verb"
    override def renderVerbChain(clauseType: ClauseType, needsFlippable: Boolean) = {
      def pastParticiple = verb.forms.pastParticiple.toString
      clauseType match {
        case ClauseType.Attributive =>
          if(!isPassive) {
            Left(NonEmptyChain.one("Cannot construct attributive clause from active form."))
          } else Right(NonEmptyList.of(pastParticiple))
        case otherType: ClauseType.VerbalClauseType =>
          if(isPassive) {
            val aux = tan.getCopulaAuxChain(otherType, subject)
            aux.map(_ append pastParticiple)
          } else {
            tan.getAuxChain(
              verb.forms, otherType, subject,
              ensureDoSupport = needsFlippable
            )
          }
      }
    }
  }
  object Verbal {
    // maybe also 'what happened _?' type of pro-form?
    // XXX change to a pro-form
    def doSomething(
      subject: Argument.Subject,
      modifiers: Vector[NonNominal],
      tan: TAN
    ) = Verbal(
      subject, Verb(InflectedForms.doForms), false,
      Argument.ProForm.what +: modifiers,
      tan
    )
  }

  // TODO:
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
  // case class ToInfinitive(
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


}
