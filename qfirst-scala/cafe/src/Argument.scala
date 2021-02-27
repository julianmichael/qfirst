package qfirst.cafe

import jjm.Dot
import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import cats.Id
import cats.Show
import cats.data.Validated
import cats.data.ValidatedNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.implicits._
import mouse.all._
import jjm.ling.Span
import cats.kernel.Semigroup

// S4: Simple Surrogate Syntactic Semantics

// SASS: Semantic Annotation via Surrogate Syntax

case class ArgText(
  text: Option[String] = None,
  pos: Option[String] = None
)
object ArgText {
  // def gap = ArgText(None, None)
  def apply(text: String): ArgText = ArgText(Some(text))
  def apply(text: String, pos: Option[String]): ArgText = ArgText(Some(text), pos)
}

// tells us whether we are rendering an argument...
// TODO: in an answer (for bare vp answers to 'what..to do' questions
// TODO: 'Fronted' position? something for when extracted
sealed trait ArgPosition extends Product with Serializable
object ArgPosition {
  // normal argument position (accusative if nominal). also used for answers
  case class Arg(index: Int) extends ArgPosition
  // for nominative case with subjects. also used for fronted items
  case object Subj extends ArgPosition
}

case class Extraction[A](
  extractee: Argument,
  innerPath: ArgumentPath[ProFormExtraction],
  swapOut: SwapOutArg[A]
)

case class ProFormExtraction[A](
  extractee: Argument.ProForm,
  swapOut: SwapOutArg[A]
)

case class ArgSnapshot[A](
  target: Argument,
  swapOut: SwapOutArg[A]
)

case class SubstitutionFailure(
  arg: Argument,
  path: ArgumentPath[ArgSnapshot]
) {
  def ascend(pos: ArgPosition) = this.copy(path = path.ascend(pos))
}

case class SwapOutArg[A](
  // replacementTemplates: Set[Argument],
  replace: Either[SubstitutionFailure, Argument] => Either[SubstitutionFailure, A]
)
// Left is for replacement arguments constructed via proxies
case class SwapOutPred[-P <: Predication, A](
  replace: Either[Argument, P] => Either[SubstitutionFailure, A]
)

sealed trait ArgumentPath[F[_]] extends Product with Serializable {
  import ArgumentPath._
  // def isExtraction: Boolean = this match {
  //   case ExtractionDescent(_, _) => true
  //   case Extraction(_) => true
  //   case FocalDescent(_, _) => false
  //   case Focus => false
  // }
  def ascend(position: ArgPosition): Descent[F] = Descent(position, this)
  def descent: Option[Descent[F]] = this match {
    case d: Descent[F] => Some(d)
    case _ => None
  }
}
object ArgumentPath {

  case class Descent[F[_]](
    position: ArgPosition,
    next: ArgumentPath[F]
  ) extends ArgumentPath[F]
  case class End[F[_]](processor: Processor[F]) extends ArgumentPath[F]

  sealed trait Processor[F[_]] {
    def process[A](
      arg: Argument, position: ArgPosition, swap: SwapOutArg[A]
    ): Component.RenderResultOf[(SyntaxTree[Argument, ArgText], F[A])]
  }

  import Validated.valid
  import SyntaxTree.{node, leaf}
  import Component.WithExtraction
  case class Extract(
    innerPath: ArgumentPath[ProFormExtraction]
  ) extends Processor[Extraction] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgText], Extraction[A])
    ] = arg match {
      case pro: Argument.ProForm =>
        valid(node(arg, NonEmptyList.of(leaf(ArgText()))) -> Extraction(pro, innerPath, swap))
      case arg: Argument.Expletive =>
        arg.invalid("Cannot extract an expletive.")
      case arg: Argument.Semantic =>
        if(arg.allowPiedPiping(innerPath)) {
          valid(node(arg, NonEmptyList.of(leaf(ArgText()))) -> Extraction(arg, innerPath, swap))
        } else arg.invalid("Extraction of concrete argument not allowed.")
    }
  }

  type Focus = Focus.type
  case object Focus extends Processor[ProFormExtraction] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgText], ProFormExtraction[A])
    ] = arg match {
      case pro: Argument.ProForm =>
        val newSwap = SwapOutArg(arg => swap.replace(arg.flatMap(pro.instantiate)))
        valid(node(arg, NonEmptyList.of(leaf(ArgText(pro.wh(pos))))) ->
                ProFormExtraction(pro, newSwap))
      case arg: Argument.Concrete =>
        arg.invalid("Focal element must be a pro-form.")
    }
  }

  type Target = Target.type
  case object Target extends Processor[ArgSnapshot] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgText], ArgSnapshot[A])
    ] = arg.render(pos, swap).map(_ -> ArgSnapshot(arg, swap))
  }

  object Processor {
    implicit def processorShow[F[_]]: Show[Processor[F]] = new Show[Processor[F]] {
      def show(p: Processor[F]) = p match {
        case Extract(innerPath) => s"{${argumentPathShow.show(innerPath)}}"
        case Focus => "<focus>"
        case Target => "<target>"
      }
    }
  }

  def descend[F[_]](path: Option[Descent[F]], pos: ArgPosition): Option[ArgumentPath[F]] =
    path.collect { case Descent(`pos`, next) => next }

  implicit def argumentPathShow[F[_]]: Show[ArgumentPath[F]] = new Show[ArgumentPath[F]] {
    def show(path: ArgumentPath[F]): String = path match {
      case Descent(position, next) => s"${position} -> ${show(next)}"
      case End(a) => Processor.processorShow.show(a)
    }
  }

  // NOTE: shouldn't this be unnecessary bc of contravariance?
  implicit def descentShow[F[_]] = argumentPathShow[F]
    .contramap[Descent[F]](x => x: ArgumentPath[F])
}

// TODO: probably break apart the hierarchy once I know what error reporting
// will actually look like.
sealed trait Component extends Product with Serializable {
  def error(msg: String) = Component.RenderError(this, msg)
  def invalid[A](
    msg: String = "Incomplete component."
  ): ValidatedNec[Component.RenderError, A] =
    Validated.invalid(NonEmptyChain.one(error(msg)))
}
object Component {
  type Branches = NonEmptyList[SyntaxTree[Argument, ArgText]]
  type Tree = SyntaxTree[Argument, ArgText]
  case class WithExtraction[+A, +Extraction](
    value: A,
    extractions: Vector[Extraction] = Vector()
  ) {
    def map[B](f: A => B) = WithExtraction(f(value), extractions)
  }
  object WithExtraction {
    implicit def withExtractionSemigroup[A: Semigroup, E] = new Semigroup[WithExtraction[A, E]] {
      // def empty = WithExtraction(Monoid[A].empty)
      def combine(x: WithExtraction[A, E], y: WithExtraction[A, E]): WithExtraction[A, E] = {
        WithExtraction(x.value |+| y.value, x.extractions ++ y.extractions)
      }
    }
  }
  case class RenderError(component: Component, msg: String)
  type RenderResultOf[A] = ValidatedNec[RenderError, A]
  type RenderTree[A] = RenderResultOf[WithExtraction[Tree, A]]
  type RenderBranches[A] = RenderResultOf[WithExtraction[Branches, A]]
}

// argument structure (with slots etc.)
sealed trait Argument extends Component {
  import Component._
  import SyntaxTree.{leaf,node}

  def symbol: String

  def renderLax[F[_], A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A],
    path: Option[ArgumentPath.Descent[F]]
  ): RenderBranches[F[A]]

  import ArgumentPath._
  def renderLaxWithPath[F[_], A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A],
    path: Option[ArgumentPath[F]]
  ): RenderTree[F[A]] = {
    path match {
      case Some(End(pathEnd)) =>
        pathEnd.process(this, pos, swapOutArg).andThen { case (tree, res) =>
          Validated.valid(WithExtraction(tree, Vector(res)))
        }
      case Some(d: Descent[F]) =>
        renderLax(pos, swapOutArg, Some(d))
          .map(_.map(branches => node(this, branches)))
      case None =>
        renderLax(pos, swapOutArg, None: Option[ArgumentPath.Descent[F]])
          .map(_.map(branches => node(this, branches)))
    }
  }

  def render[F[_], A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A],
    path: Option[ArgumentPath[F]]
  ): RenderResultOf[(Tree, Option[F[A]])] =
    // TODO add swap out arg? or do something else?
    renderLaxWithPath(pos, swapOutArg, path).map { case WithExtraction(tree, items) =>
      require(
        (path.isEmpty && items.isEmpty) ||
          (path.nonEmpty && items.size == 1)
      ) // require correct number of extractions
      tree -> items.headOption
    }

  def render[F[_], A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A],
    path: ArgumentPath[F]
  ): RenderResultOf[(Tree, F[A])] =
    render(pos, swapOutArg, Some(path)).map { case (x, y) => x -> y.get } // require 1 extraction

  def render[A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A]
  ): RenderResultOf[Tree] = {
    // ascribe a type to the None to help out type inference
    render(pos, swapOutArg, None: Option[ArgumentPath[ProFormExtraction]]).map { case (tree, itemOpt) =>
      require(itemOpt.isEmpty) // require no extractions
      tree
    }
  }

  def argumentPaths: Set[ArgumentPath[ProFormExtraction]]
  def extractionPaths: Set[ArgumentPath[Extraction]]
}
object Argument {

  import Component._
  import Lexicon._
  // import LabeledTree.{leaf, leaves, node}
  import SyntaxTree.{leaf, node}
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

    import ArgPosition._, ArgumentPath._
    def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[ArgumentPath.Descent[F]]
    ): RenderTree[F[A]] = path match {
      case Some(_) => invalid("Cannot extract from inside a pro-form.")
      case None => placeholder
          .map(p => valid(WithExtraction(leaf(ArgText(p)))))
          .getOrElse(invalid("No placeholder exists for pro-form."))
    }

    // def argumentPaths: Set[ArgumentPath] = Set(Extraction(Focus), Focus)
    def argumentPaths: Set[ArgumentPath[ProFormExtraction]] = Set(End(Focus))
    def extractionPaths: Set[ArgumentPath[Extraction]] = Set(End(Extract(End(Focus))))

    def instantiateConcrete(arg: Concrete): Option[Concrete]

    def instantiate(arg: Argument): Either[SubstitutionFailure, Argument] = arg match {
      case pro: ProForm => Left(SubstitutionFailure(pro, End(Target)))
      case concrete: Concrete => instantiateConcrete(concrete).toRight(
        SubstitutionFailure(concrete, End(Target))
      )
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
      // val instances = Set(NounPhrase(None, Some(true)))
      def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        case np @ NounPhrase(_, _) => Some(np)
        case _ => None
      }
    }
    case object what extends Noun with Complement {
      def wh(pos: ArgPosition) = "what"
      val placeholder = Some("something")
      val number = Some(Number.Singular)
      val person = Some(Person.Third)
      // val instances = Set(
      //   NounPhrase(None, Some(false)),
      //   Gerund(None, false),
      //   Gerund(None, true),
      //   ToInfinitive(None, false, Set(what)),
      //   ToInfinitive(None, true, Set(what)),
      //   Attributive(None, Set(what)),
      //   Finite(None),
      //   FiniteComplement(None)
      // )
      def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        case arg @ NounPhrase(_, _) => Some(arg)
        case arg @ Gerund(_, _) => Some(arg)
        case arg @ ToInfinitive(_, _, _) => Some(arg)
        case arg @ Attributive(_, _) => Some(arg)
        case arg @ Finite(_) => Some(arg)
        case arg @ FiniteComplement(_) => Some(arg)
        case _ => None
      }
    }
    sealed trait Adverb extends ProForm with NonNominal with NounOrOblique {
      def wh(pos: ArgPosition) = whWord.form.toString
      def whWord: Lexicon.Wh.Adverb
      // val instances = Set(
      //   Oblique(None, Set(this)),
      //   Subordinate(None, Set(this)),
      //   Adverbial(None, Set(this)),
      //   ToInfinitive(None, false, Set(this)),
      //   ToInfinitive(None, true, Set(this)),
      //   Progressive(None, false, Set(this)),
      //   Progressive(None, true, Set(this)),
      //   Attributive(None, Set(this))
      // )
      def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        case arg @ Oblique(_, _) => Some(arg)
        case arg @ Subordinate(_, _) => Some(arg)
        case arg @ Adverbial(_, _) => Some(arg)
        case arg @ ToInfinitive(_, _, _) => Some(arg)
        case arg @ Progressive(_, _, _) => Some(arg)
        case arg @ Attributive(_, _) => Some(arg)
        case _ => None
      }
    }
    case object when extends Adverb {
      val whWord = Wh.when
      val placeholder = None
    }
    case object where extends Adverb {
      val whWord = Wh.where
      val placeholder = Some("somewhere")
    }
    case object how extends Adverb {
      val whWord = Wh.how
      val placeholder = None
    }
    case object why extends Adverb {
      val whWord = Wh.why
      val placeholder = None
    }
    // TODO: how long, how much etc.: should use pied piping
    // or whatever other general facility I can come up with
    object Adverb {
      def fromWh(wh: Lexicon.Wh.Adverb) = all.find(_.whWord == wh).get
      def all = List(when, where, how, why)
    }
    // TODO: consider adding 'do' as a pro-form, AFTER figuring out
    // extraction, question formation, AND answer filling for the simple case.
  }

  sealed trait Concrete extends Argument {
    def category: String
    def proForms: Set[Argument.ProForm]

    final def symbol = {
      val pros = proForms.map(_.wh(ArgPosition.Subj)).toList.sortBy(_.toString).mkString("/")
      if(pros.isEmpty) category
      else s"$category {$pros}"
    }
  }

  // expletive 'it' or 'there'; can only appear as subject
  case class Expletive(expletive: Lexicon.Expletive) extends Concrete with Subject {
    def category = s"np[${expletive.form}]"
    def proForms = Set()
    def person = Some(Person.Third)
    def number = Some(Number.Singular)
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = path match {
      case Some(_) => invalid("Cannot focus or descend into an expletive.")
      case None => pos match {
        case Subj => valid(WithExtraction(NonEmptyList.of(leaf(ArgText(expletive.form.toString)))))
        case Arg(_) => invalid("Can only render expletives in subject position.")
      }
    }

    def argumentPaths: Set[ArgumentPath[ProFormExtraction]] = Set()
    def extractionPaths: Set[ArgumentPath[Extraction]] = Set()
  }
  object Expletive {
    val it = Expletive(Lexicon.Expletive.it)
    val there = Expletive(Lexicon.Expletive.there)
  }

  sealed trait Semantic extends Concrete {
    def pred: Option[Predication]

    // determine which nested args we pied-pipe with
    def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = false
    // enforce island constraints
    def blockExtraction(path: ArgumentPath[Extraction]): Boolean = false

    import ArgumentPath._
    def argumentPaths: Set[ArgumentPath[ProFormExtraction]] = {
      // widening to deal with invariance of Set
      pred.unorderedFoldMap(_.argumentPaths.map(p => p: ArgumentPath[ProFormExtraction]))
    }
    def extractionPaths: Set[ArgumentPath[Extraction]] = {
      argumentPaths.collect {
        case path if allowPiedPiping(path) => End(Extract(path)): ArgumentPath[Extraction]
      } ++ pred.unorderedFoldMap(_.extractionPaths.filterNot(blockExtraction))
    }
  }

  // NOTE: predications are optional because we can use arguments
  // to specify general syntactic subcategorization frames.

  case class NounPhrase(
    pred: Option[Predication.Nominal],
    animacyConstraint: Option[Boolean]
  ) extends Semantic with Nominal {

    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = true

    def category = "np"
    // TODO: change to account for multiple possible animacies
    def proForms =
      animate.map(anim => if(anim) ProForm.who else ProForm.what)
        .toSet

    def animate: Option[Boolean] = pred.flatMap(_.animate).orElse(animacyConstraint)
    def person: Option[Person] = pred.flatMap(_.person)
    def number: Option[Number] = pred.flatMap(_.number)

    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val c = pos match {
        case Subj => Case.Nominative
        case Arg(_) => Case.Accusative
      }
      val swapOutPred = SwapOutPred[Predication.Nominal, A] {
        case Right(pred) => swapOutArg.replace(Right(this.copy(pred = Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
          // TODO: perhaps consider erroring if there's a problem with animacy
        // case x => invalid(s"Trying to replace nominal pred of NP arg with: $x")
      }
      pred.map(_.renderLax(c, swapOutPred, path)).getOrElse(invalid())
    }

  }
  object NounPhrase

  // TODO: perhaps just collapse this together with noun phrase
  // and have some 'arbitrary span' type of argument.
  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    pred: Option[Predication.Adverbial],
    adverbials: Set[ProForm.Adverb]
  ) extends Semantic with NonNominal {
    override def category = "adv"
    override def proForms = adverbials.map(x => x: ProForm)
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Predication.Adverbial, A] {
        case Right(pred) => swapOutArg.replace(Right(this.copy(pred = Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(_.renderLax((), swapOutPred, path)).getOrElse(invalid())
    }
  }

  case class Oblique(
    pred: Option[Predication.Oblique],
    adverbials: Set[ProForm.Adverb] = Set()
  ) extends Semantic with NounOrOblique with NonNominal {

    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = true

    def category = "pp"
    def proForms = adverbials.map(x => x: ProForm)

    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Predication.Oblique, A] {
        case Right(pred) => swapOutArg.replace(Right(this.copy(pred = Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(_.renderLax((), swapOutPred, path)).getOrElse(invalid())
    }
  }

  // TODO: island constraints
  case class Subordinate(
    pred: Option[Predication.Subordinate],
    adverbials: Set[ProForm.Adverb]
  ) extends Semantic with NonNominal {
    override def category = "adv[comp]"
    override def proForms = adverbials.map(x => x: ProForm)
    // TODO might depend on the predicate?
    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]) = true
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Predication.Subordinate, A] {
        case Right(pred) => swapOutArg.replace(Right(this.copy(pred = Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(_.renderLax((), swapOutPred, path)).getOrElse(invalid())
    }
  }

  sealed trait Clausal extends Semantic {
    // TODO: this would probably be way easier to do with typeclasses lmao
    type Self <: Clausal.Aux[Pred]
    type Pred <: Predication.Clausal { type Self <: Pred }
    def pred: Option[Pred]
    def withPred(pred: Option[Pred]): Self // for SwapOutPred
    def clauseType: ClauseType
    def includeSubject: Boolean
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Pred, A] {
        case Right(pred) => swapOutArg.replace(Right(this.withPred(Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(
        _.renderLax(
          Predication.ClauseFeats(clauseType, includeSubject),
          swapOutPred,
          path)
      ).getOrElse(invalid())
    }

    // filter out subject extractions in case our subject is not included
    override def argumentPaths: Set[ArgumentPath[ProFormExtraction]] =
      super.argumentPaths.filter(x =>
        includeSubject || x.descent.forall(_.position != ArgPosition.Subj)
      )
    override def extractionPaths: Set[ArgumentPath[Extraction]] =
      super.extractionPaths.filter(x =>
        includeSubject || x.descent.forall(_.position != ArgPosition.Subj)
      )
  }
  object Clausal {
    type Aux[A] = Clausal { type Pred = A }
  }

  // nominal use of gerund/progressive; can be object of prep or subject
  // TODO possessive subject option
  case class Gerund(
    pred: Option[Predication.Clausal],
    includeSubject: Boolean
  ) extends Clausal with Nominal {
    type Self = Gerund
    type Pred = Predication.Clausal
    override def withPred(pred: Option[Predication.Clausal]) = this.copy(pred = pred)
    override def clauseType = ClauseType.Progressive
    override def category = if(includeSubject) "s[g]" else "vp[g]"
    override def proForms = Set(ProForm.what)
    // TODO should allow with 'whose' on subject?
    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]) = false
    override def person = Some(Person.Third)
    override def number = Some(Number.Singular)
  }

  // includeSubject: Boolean,
  case class BareInfinitive(
    pred: Option[Predication.Clausal]
  ) extends Clausal with Complement {
    type Self = BareInfinitive
    type Pred = Predication.Clausal
    override def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred)
    override def clauseType = ClauseType.BareInfinitive
    override def includeSubject = false
    override def category = "vp[b]"
    override def proForms = Set()
  }

  // various possible forms of complements:
  // may be specific to a predicate's (or subordinator's!) subcat frame.
  // TODO: fix up infinitives. all sorts of syntactic possibilities here.
  // Not sure what will be best.
  case class ToInfinitive(
    pred: Option[Predication.Clausal],
    includeSubject: Boolean,
    // pro: Option[ProFormProForm],
    proForms: Set[ProForm]
  ) extends Clausal with Complement {
    type Self = ToInfinitive
    type Pred = Predication.Clausal
    def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred)
    // TODO: with Subject. or, just add a new argument type for subj inf?
    def complementizer: InfinitiveComplementizer = Lexicon.InfinitiveComplementizer.`for`

    override def clauseType = ClauseType.ToInfinitive
    override def category = if(includeSubject) "s[for-to]" else "vp[to]"

    // override def person = Some(Person.Third)
    // override def number = Some(Number.Singular)

    // TODO clausal override not that useful here. maybe separate out versions
    // with and without subject? idk
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Predication.Clausal, A] {
        case Right(pred) =>
          swapOutArg.replace(Right(this.withPred(Some(pred))))
        case Left(arg) =>
          swapOutArg.replace(Right(arg))
      }
      pred.map { p =>
        val predBranches = p.renderLax(
          Predication.ClauseFeats(ClauseType.ToInfinitive, includeSubject),
          swapOutPred,
          path)
        val branches = if(!includeSubject) predBranches else {
          val comp = valid(
            WithExtraction(
              NonEmptyList.of(leaf(ArgText(complementizer.form.toString, pos = Some("comp"))))
            )
          )
          NonEmptyList.of(comp, predBranches).reduceA[
            ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]
          ]
          // (comp, predBranches).mapN(_ |+| _)
        }
        branches

        // List(
        //   if(includeSubject) valid(
        //     WithExtraction()
        //   ) else valid(
        //     WithExtraction(Vector[SyntaxTree[Argument, ArgText]](Vector()))
        //   ),
        // )
      }.getOrElse(invalid())
    }
  }

  case class Progressive(
    pred: Option[Predication.Clausal],
    includeSubject: Boolean,
    adverbials: Set[ProForm.Adverb]
  ) extends Clausal with Complement {
    type Self = Progressive
    type Pred = Predication.Clausal
    def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred)
    override def clauseType = ClauseType.Progressive
    override def category = if(includeSubject) "s[ng]" else "vp[ng]"
    override def proForms = adverbials.map(x => x: ProForm)
  }

  // NOTE: maybe 'how' is the only acceptable pro-form here?
  // i guess 'what' appears for adjectives...
  // can subjects appear? she wants Ben happy. What does she want? Ben happy. idk.
  // TODO: probably add subjects back in. maybe be lenient.
  // includeSubject: Boolean,
  case class Attributive(
    pred: Option[Predication.NonCopular],
    proForms: Set[ProForm]
  ) extends Clausal with Complement {
    type Self = Attributive
    type Pred = Predication.Clausal
    // TODO: add error case to this, or (preferably?) split out attributive predications
    def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred.asInstanceOf[Option[Predication.NonCopular]])
    override def clauseType = ClauseType.Attributive
    override def includeSubject = false
    // def category = if(includeSubject) "s[pt]" else "vp[pt]"
    override def category = "vp[adj/pt]"
  }

  case class Finite(
    pred: Option[Predication.Clausal]
  ) extends Clausal with Complement {
    type Self = Finite
    type Pred = Predication.Clausal
    // with Subject? but cannot appear in subj except as an answer
    def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred)
    override def clauseType = ClauseType.Finite
    override def includeSubject = true
    override def category = "s[dcl]"
    override def proForms = Set(ProForm.what)
  }

  // is this always necessarily 'that'? maybe remove complementizer slot?
  // TODO add 'that'
  case class FiniteComplement(
    pred: Option[Predication.Clausal],
  ) extends Clausal with Complement with Subject {
    type Self = FiniteComplement
    type Pred = Predication.Clausal
    def withPred(pred: Option[Predication.Clausal]): Self = this.copy(pred = pred)
    override def clauseType = ClauseType.Finite
    override def includeSubject = true

    override def category = "s[comp]"
    override def proForms = Set(ProForm.what)

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def complementizer: Complementizer = Lexicon.Complementizer.that

    // not sure if Clausal override is useful here
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Predication.Clausal, A] {
        case Right(pred) => swapOutArg.replace(Right(this.withPred(Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(p =>
        NonEmptyList.of(
          valid(
            WithExtraction(
              NonEmptyList.of(leaf(ArgText(complementizer.form.toString, pos = Some("comp"))))
            )
          ),
          p.renderLax(Predication.ClauseFeats(clauseType, includeSubject), swapOutPred, path)
        ).reduceA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
      ).getOrElse(invalid())
    }
  }
}


case class ProxyArgument[P <: Predication](
  path: ArgumentPath[ArgSnapshot],
  construct: Argument => Option[Either[Argument, P]]
) {
  def recover(failure: SubstitutionFailure): Option[Either[Argument, P]] = {
    if(failure.path == path) construct(failure.arg) else None
  }
}

sealed trait Predication extends Component {

  type Self <: Predication

  def swapOutSelf: SwapOutPred[Self, Self] = SwapOutPred {
    case Right(pred) => Right(pred)
    // TODO: do something for 'do' pro-verb to get it to resolve out as root?
    case Left(arg) => Left(SubstitutionFailure(arg, ArgumentPath.End(ArgumentPath.Target)))
  }
  def proxy: Option[ProxyArgument[Self]]

  import Component._

  type GramFeats // grammatical features

  def renderLax[F[_], A](
    features: GramFeats,
    swapOutPred: SwapOutPred[Self, A],
    path: Option[ArgumentPath.Descent[F]]
  ): RenderResultOf[WithExtraction[Branches, F[A]]]

  def arguments: Vector[Argument]

  final def render[F[_]](
    feats: GramFeats,
    path: Option[ArgumentPath.Descent[F]]
  ): RenderResultOf[(Branches, Option[F[Self]])] =
    renderLax(feats, swapOutSelf, path)
      .map { case WithExtraction(tree, items) =>
        require(
          (path.isEmpty && items.isEmpty) ||
            (path.nonEmpty && items.size == 1)
        ) // require correct number of extractions
        tree -> items.headOption
      }

  final def render[F[_]](
    feats: GramFeats, path: ArgumentPath.Descent[F]
  ): RenderResultOf[(Branches, F[Self])] =
    render(feats, Some(path)).map { case (x, y) => x -> y.get } // require 1 extraction

  final def render(feats: GramFeats): RenderResultOf[Branches] = {
    // ascribe a type to the None to help out type inference
    render(feats, None: Option[ArgumentPath.Descent[ProFormExtraction]]).map { case (tree, itemOpt) =>
      require(itemOpt.isEmpty) // require no extractions
      tree
    }
  }

  def argumentPaths: Set[ArgumentPath.Descent[ProFormExtraction]] =
    arguments.zipWithIndex.flatMap { case (arg, index) =>
      arg.argumentPaths.map(_.ascend(ArgPosition.Arg(index)))
    }.toSet
  def extractionPaths: Set[ArgumentPath.Descent[Extraction]] =
    arguments.zipWithIndex.flatMap { case (arg, index) =>
      arg.extractionPaths.map(_.ascend(ArgPosition.Arg(index)))
    }.toSet
}
object Predication {
  import Component._
  import Lexicon._
  import Argument._
  import Validated.valid
  import SyntaxTree.{node, leaf}

  // doesn't extend Predication because we don't need to use it that way yet.
  // in the future, with proper nominal predicates, might use it this way.
  // but it would be a separate (non-clausal) type of predication.
  sealed trait Nominal extends Predication {

    type GramFeats = Case
    type Self <: Nominal
    override def proxy: Option[ProxyArgument[Self]] = None

    def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]
    def arguments = Vector()

    def getForm(c: Case): String

    override def renderLax[F[_], A](
      features: Case,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): RenderBranches[F[A]] = path match {
      case Some(_) => invalid("Cannot descend into a nominal predication for extraction (for now).")
      case None => valid(WithExtraction(NonEmptyList.of(leaf(ArgText(getForm(features))))))
    }
  }

  case class NounPhrase(
    span: Option[Span],
    form: String,
    animate: Option[Boolean],
    person: Option[Person],
    number: Option[Number]
  ) extends Nominal {
    type Self = NounPhrase
    def getForm(c: Case): String = c match {
      case Case.Genitive => form + "'s"
      case _ => form
    }
  }
  object NounPhrase
  // TODO: add definite pronouns

  sealed trait Oblique extends Predication {
    type Self <: Oblique
    type GramFeats = Unit
    import ArgPosition._, ArgumentPath.Descent
  }
  // prep should not take an object, i.e., is a particle
  case class Particulate(
    span: Option[Span],
    prts: NonEmptyList[Particle]
  ) extends Oblique {
    type Self = Particulate
    override def proxy: Option[ProxyArgument[Self]] = None
    def arguments = Vector()

    import ArgPosition._, ArgumentPath.Descent

    override def renderLax[F[_], A](
      feats: Unit,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[Descent[F]]
    ): RenderBranches[F[A]] = path match {
      case Some(_) => invalid("Cannot extract from inside a particle.")
      case None => valid(
        WithExtraction(
          NonEmptyList.of(leaf(ArgText(prts.map(_.form).toList.mkString(" "), pos = Some("prt"))))
        )
      )
    }
  }

  case class Prepositional(
    span: Option[Span],
    preps: NonEmptyList[Preposition],
    obj: Argument.Nominal,
    proxy: Option[ProxyArgument[Prepositional]] = None
  ) extends Oblique {
    type Self = Prepositional
    def arguments = Vector(obj)
    import ArgPosition._, ArgumentPath._
    override def renderLax[F[_], A](
      feats: Unit,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[Descent[F]]
    ): RenderBranches[F[A]] = {
      if(path.exists(_.position != Arg(0))) {
        invalid(s"Can't descend into argument for extraction from oblique: ${path.get}")
      } else NonEmptyList.of(
        valid(
          WithExtraction(
            leaf(ArgText(preps.map(_.form).toList.mkString(" "), pos = Some("prep")))
          )
        ),
        obj.renderLaxWithPath(
          Arg(0),
          SwapOutArg {
            case Right(arg: Argument.Nominal) => swapOutPred.replace(Right(this.copy(obj = arg)))
            case Right(otherArg) => Left(SubstitutionFailure(otherArg, Descent(Arg(0), End(Target))))
            case Left(failure) =>
              val newFailure = failure.ascend(Arg(0))
              proxy.flatMap(_.recover(newFailure))
                .map(swapOutPred.replace)
                .getOrElse(Left(newFailure))
          },
          ArgumentPath.descend(path, Arg(0))
        )
      ).map(_.map(_.map(x => NonEmptyList.of(x))))
        .reduceA
        // .nonEmptySequence[ValidatedNec[RenderError, *], WithExtraction[Tree, F[A]]]
        
        // .sequenceA
    }
  }

  case class Adverbial(
    span: Option[Span],
    form: String
  ) extends Predication {
    type Self = Adverbial
    override def proxy: Option[ProxyArgument[Self]] = None
    type GramFeats = Unit
    def arguments = Vector()

    import ArgumentPath.Descent
    override def renderLax[F[_], A](
      feats: Unit,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[Descent[F]]
    ): RenderBranches[F[A]] = path match {
      case Some(_) => invalid("Cannot descend into an adverbial for extraction (for now).")
      case None => valid(WithExtraction(NonEmptyList.of(leaf(ArgText(form)))))
    }
  }

  case class Subordinate(
    index: Option[Int],
    subordinator: Subordinator,
    clause: Argument.Complement,
    proxy: Option[ProxyArgument[Subordinate]] = None
  ) extends Predication {
    type Self = Subordinate
    type GramFeats = Unit
    def arguments = Vector(clause)
    import ArgumentPath._, ArgPosition._
    override def renderLax[F[_], A](
      feats: Unit,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[Descent[F]]
    ): RenderBranches[F[A]] = {
      path match {
        case Some(pos) if pos != Arg(0) =>
          invalid(s"Can't descend into argument for extraction from subordinate: $pos")
        case otherPath => NonEmptyList.of(
          valid(WithExtraction(leaf(ArgText(subordinator.form.toString, pos = Some("sub"))))),
          clause.renderLaxWithPath(
            Arg(0),
            SwapOutArg {
              case Right(arg: Argument.Complement) =>
                swapOutPred.replace(Right(this.copy(clause = arg)))
              case Right(otherArg) =>
                Left(SubstitutionFailure(otherArg, Descent(Arg(0), End(Target))))
              case Left(failure) =>
                val newFailure = failure.ascend(Arg(0))
                proxy.flatMap(_.recover(newFailure))
                  .map(swapOutPred.replace)
                  .getOrElse(Left(newFailure))
            },
            ArgumentPath.descend(path, Arg(0))
          )
        ).map(_.map(_.map(x => NonEmptyList.of(x)))).reduceA
            // .foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
      }
    }
  }

  case class ClauseFeats(
    clauseType: ClauseType,
    includeSubject: Boolean
  )

  sealed trait Clausal extends Predication {
    def index: Option[Int]
    type Self <: Clausal
    // type Self = Clausal
    type GramFeats = ClauseFeats
    def subject: Argument.Subject
    def withSubject(subject: Argument.Subject): Self
    def tan: TAN
    def arguments: Vector[Argument]

    def predPOS: String

    def renderVerbChain(
      clauseType: ClauseType,
      needsFlippable: Boolean
    ): Either[NonEmptyChain[String], NonEmptyList[String]]

    import Validated.valid
    // import LabeledTree.{leaf, leaves, node}
    import SyntaxTree.{node, leaf}
    import Component.WithExtraction

    override def renderLax[F[_], A](
      feats: ClauseFeats,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): Component.RenderBranches[F[A]] = {
      import feats.{clauseType, includeSubject}
      val frontAuxiliary = clauseType == ClauseType.Inverted
      validatePath(includeSubject, path) *> {
        val args: RenderBranches[F[A]] = renderArguments(path, swapOutPred)
        def makeVerbTree(chain: NonEmptyList[String]) = {
          if(chain.size == 1) NonEmptyList.of(
            leaf(ArgText(chain.head, pos = Some(predPOS)))
          ) else NonEmptyList.of(
            leaf(ArgText(chain.init.mkString(" "), pos = Some("aux"))),
            leaf(ArgText(chain.last, pos = Some(predPOS)))
          )
        }
        if(includeSubject) {
          renderSubject(includeSubject, swapOutPred, path).andThen { subjValue =>
            val flipAuxiliary = frontAuxiliary && subjValue.value
              .reduceMap(_.text.combineAll)
              .nonEmpty
              // .foldMap(_.text.combineAll)
            Validated.fromEither(
              renderVerbChain(clauseType, flipAuxiliary).left.map(_.map(error(_)))
            ).andThen { verbChain =>
              val subj: RenderBranches[F[A]] = valid(subjValue.map(x => NonEmptyList.of(x)))
              if(flipAuxiliary) { // aux flip
                val aux: RenderBranches[F[A]] = valid(
                  WithExtraction(
                    NonEmptyList.of(leaf(ArgText(verbChain.head, pos = Some("aux"))))))
                NonEmptyList.fromList(verbChain.tail) match {
                  case None =>
                    NonEmptyList.of(
                      aux, subj, args
                    ).reduceA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
                  case Some(verbTail) =>
                    val verb = valid(WithExtraction(makeVerbTree(verbTail)))
                    NonEmptyList.of(
                      aux, subj, verb, args
                    ).reduceA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
                      // .foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
                }
              } else {
                val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
                NonEmptyList.of(
                  subj, verb, args
                ).reduceA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
              }
            }
          }
        } else {
          Validated.fromEither(
            renderVerbChain(clauseType, false).left.map(_.map(error(_)))
          ).andThen { verbChain =>
            val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
            NonEmptyList.of(
              verb, args
            ).reduceA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
          }
          // [ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
          // .foldA
        }
      }.andThen(res =>
          if(res.extractions.size > 1) {
            invalid("Somehow produced multiple extractions.")
          } else if(res.extractions.nonEmpty && path.isEmpty) {
            invalid("Produced spurious extractions.")
          } else valid(res)
        )
    }

    def validatePath[F[_]](
      includeSubject: Boolean,
      path: Option[ArgumentPath.Descent[F]]
    ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
      case None => valid(())
      case Some(ArgPosition.Subj) if includeSubject => valid(())
      case Some(ArgPosition.Arg(i)) if arguments.size > i => valid(())
      case Some(pos) => invalid(s"Cannot descent into argument position for extraction: $pos")
    }

    def renderSubject[F[_], A](
      includeSubject: Boolean,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): Component.RenderTree[F[A]] = {
      import ArgumentPath._, ArgPosition._
      subject.renderLaxWithPath(
        Subj,
        SwapOutArg {
          case Right(arg: Argument.Subject) =>
            swapOutPred.replace(Right(this.withSubject(arg)))
          case Right(otherArg) =>
            Left(SubstitutionFailure(otherArg, Descent(Subj, End(Target))))
          case Left(failure) =>
            val newFailure = failure.ascend(Subj)
            proxy.flatMap(_.recover(newFailure))
              .map(swapOutPred.replace)
              .getOrElse(Left(newFailure))
        },
        ArgumentPath.descend(path, Subj)
      )
    }

    def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A]

    def renderArguments[F[_], A](
      path: Option[ArgumentPath.Descent[F]],
      swapOutPred: SwapOutPred[Self, A]
    ): Component.RenderResultOf[WithExtraction[Vector[SyntaxTree[Argument, ArgText]], F[A]]] = {
      arguments.zipWithIndex.foldMapA[
        ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]
      ] { case (arg, index) =>
          Vector(
            arg.renderLaxWithPath(
              ArgPosition.Arg(index),
              swapOutArg(swapOutPred, index),
              ArgumentPath.descend(path, ArgPosition.Arg(index))
            )
          )
      }
    }

    override def argumentPaths: Set[ArgumentPath.Descent[ProFormExtraction]] = {
      import ArgPosition._
      subject.argumentPaths.map(_.ascend(Subj)) ++ super.argumentPaths
      // arguments.zipWithIndex.flatMap { case (arg, index) =>
      //   arg.argumentPaths.map(_.ascend(Arg(index)))
      // }
    }
    // TODO island constraints?
    override def extractionPaths: Set[ArgumentPath.Descent[Extraction]] =
      subject.extractionPaths.map(_.ascend(ArgPosition.Subj)) ++ super.extractionPaths
  }

  case class Copular(
    index: Option[Int],
    subject: Argument.Subject,
    argument: NounOrOblique,
    modifiers: Vector[NonNominal],
    tan: TAN,
    proxy: Option[ProxyArgument[Copular]] = None
  ) extends Clausal {
    type Self = Copular
    def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
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
    }
    import ArgPosition._, ArgumentPath._
    def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
      SwapOutArg {
        case Right(arg: NounOrOblique) if index == 0 =>
          swapOutPred.replace(Right(this.copy(argument = arg)))
        case Right(arg: NonNominal) if index > 0 =>
          swapOutPred.replace(Right(this.copy(modifiers = modifiers.updated(index, arg))))
        case Right(otherArg) =>
          Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
        case Left(failure) =>
          val newFailure = failure.ascend(Arg(index))
          proxy.flatMap(_.recover(newFailure))
            .map(swapOutPred.replace)
            .getOrElse(Left(newFailure))
      }
  }

  sealed trait NonCopular extends Clausal {
    // type Self <: NonCopular
  }

  case class Adjectival(
    index: Option[Int],
    subject: Argument.Subject,
    adjective: Adjective,
    arguments: Vector[NonNominal],
    tan: TAN,
    proxy: Option[ProxyArgument[Adjectival]] = None
  ) extends NonCopular {
    type Self = Adjectival
    def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
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
    import ArgPosition._, ArgumentPath._
    def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
      SwapOutArg {
        case Right(arg: NonNominal) =>
          swapOutPred.replace(Right(this.copy(arguments = arguments.updated(index, arg))))
        case Right(otherArg) =>
          Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
        case Left(failure) =>
          val newFailure = failure.ascend(Arg(index))
          proxy.flatMap(_.recover(newFailure))
            .map(swapOutPred.replace)
            .getOrElse(Left(newFailure))
      }
  }

  case class Verbal(
    index: Option[Int],
    subject: Argument.Subject,
    verb: Verb,
    isPassive: Boolean,
    arguments: Vector[NonSubject],
    tan: TAN,
    proxy: Option[ProxyArgument[Clausal]] = None
  ) extends NonCopular {
    type Self = Clausal
    def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
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
    import ArgPosition._, ArgumentPath._
    def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
      SwapOutArg {
        case Right(arg: NonSubject) =>
          swapOutPred.replace(Right(this.copy(arguments = arguments.updated(index, arg))))
        case Right(otherArg) =>
          Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
        case Left(failure) =>
          val newFailure = failure.ascend(Arg(index))
          proxy.flatMap(_.recover(newFailure))
            .map(swapOutPred.replace)
            .getOrElse(Left(newFailure))
      }
  }
  object Verbal {
    // maybe also 'what happened _?' type of pro-form?
    // XXX change to a pro-form
    import ArgumentPath._
    import ArgPosition._
    def doSomething(
      subject: Argument.Subject,
      // modifiers: Vector[NonNominal],
      tan: TAN
    ) = Verbal(
      None,
      subject, Verb(InflectedForms.doForms), false,
      Vector(Argument.ProForm.what),// +: modifiers,
      tan,
      Some(
        ProxyArgument(
          Descent(Arg(0), End(Target)),
          arg => arg match {
            case bare: Argument.BareInfinitive =>
              bare.pred.map(Right(_))
            case _ => None
          }
        )
      )
    )
  }
}
