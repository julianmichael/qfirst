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
import jjm.ling.ESpan
import cats.kernel.Semigroup

// S4: Simple Surrogate Syntactic Semantics

// SASS: Semantic Annotation via Surrogate Syntax

sealed trait SForm {
  import SForm._
  def getVPForm(subject: Argument.Subject): VPForm = this match {
    case ForToInfinitive => VPForm.ToInfinitive
    case Progressive => VPForm.Progressive
    case Finite(tense) => VPForm.Finite(tense, subject.number, subject.person)
    case FiniteComplement(_, tense) => VPForm.Finite(tense, subject.number, subject.person)
    case Inverted(tense) => VPForm.Finite(tense, subject.number, subject.person)
    // case FiniteQuestion(tense, _) => VPForm.Finite(tense, subject.number, subject.person)
  }
  def getComplementizer: Option[String] = this match {
    case ForToInfinitive => Some("for")
    case FiniteComplement(comp, _) => Some(comp.form.toString)
    case _ => None
  }
}
object SForm {
  import qasrl.Tense
  // NOTE: could include small clauses (bare inf) as well as inverted ones
  // e.g., "be he a rascal, ..."
  // case class BareInfinitive(aspect: Aspect) extends VPForm
  case object ForToInfinitive extends SForm
  // TODO: genitive subject
  case object Progressive extends SForm
  case class Finite(
    tense: qasrl.Tense.Finite
  ) extends SForm
  case class FiniteComplement(
    complementizer: Lexicon.Complementizer,
    tense: Tense.Finite
  ) extends SForm
  case class Inverted(
    tense: Tense.Finite
  ) extends SForm
  // case class FiniteQuestion(
  //   tense: Tense.Finite,
  //   path: Option[ArgumentPath[Extraction]]
  // ) extends SForm
}

sealed trait ArgContent {
  import ArgContent._
  def text: String
  def nonEmpty = this match {
    case Blank => true
    case Gap => false
    case Text(text, _) => true // text.nonEmpty
  }
}
object ArgContent {

  def blank: ArgContent = Blank
  def gap: ArgContent = Gap
  def text(text: String, pos: Option[String] = None): ArgContent = Text(text, pos)

  case object Blank extends ArgContent {
    def text = "?"
  }
  case object Gap extends ArgContent {
    def text = "_"
  }
  case class Text(
    text: String,
    pos: Option[String] = None
  ) extends ArgContent
  object Text {
    // def gap = ArgText(None, None)
    // def apply(text: String): ArgText = ArgText(Some(text))
    // def apply(text: String, pos: Option[String]): ArgText = ArgText(Some(text), pos)
  }
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
  def map[G[_]](f: Processor[F] => Processor[G]): ArgumentPath[G]
}
object ArgumentPath {

  case class Descent[F[_]](
    position: ArgPosition,
    next: ArgumentPath[F]
  ) extends ArgumentPath[F] {
    def map[G[_]](f: Processor[F] => Processor[G]): Descent[G] = {
      Descent(position, next.map(f))
    }
  }
  case class End[F[_]](processor: Processor[F]) extends ArgumentPath[F] {
    def map[G[_]](f: Processor[F] => Processor[G]): End[G] = {
      End(f(processor))
    }
  }

  sealed trait Processor[F[_]] {
    def process[A](
      arg: Argument, position: ArgPosition, swap: SwapOutArg[A]
    ): Component.RenderResultOf[(SyntaxTree[Argument, ArgContent], F[A])]
  }

  import Validated.valid
  import SyntaxTree.{node, leaf}
  import Component.WithExtraction
  case class Extract(
    innerPath: ArgumentPath[ProFormExtraction]
  ) extends Processor[Extraction] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgContent], Extraction[A])
    ] = arg match {
      case pro: Argument.ProForm =>
        valid(node(arg, Vector(leaf(ArgContent.gap))) -> Extraction(pro, innerPath, swap))
      case arg: Argument.Expletive =>
        arg.invalid("Cannot extract an expletive.")
      case arg: Argument.Semantic =>
        if(arg.allowPiedPiping(innerPath)) {
          valid(node(arg, Vector(leaf(ArgContent.gap))) -> Extraction(arg, innerPath, swap))
        } else arg.invalid("Extraction of concrete argument not allowed.")
    }
  }

  type Focus = Focus.type
  case object Focus extends Processor[ProFormExtraction] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgContent], ProFormExtraction[A])
    ] = arg match {
      case pro: Argument.ProForm =>
        val newSwap = SwapOutArg(arg => swap.replace(arg.flatMap(pro.instantiate)))
        valid(node(arg, Vector(leaf(ArgContent.text(pro.wh(pos))))) ->
                ProFormExtraction(pro, newSwap))
      case arg: Argument.Concrete =>
        arg.invalid("Focal element must be a pro-form.")
    }
  }

  type Target = Target.type
  case object Target extends Processor[ArgSnapshot] {
    def process[A](arg: Argument, pos: ArgPosition, swap: SwapOutArg[A]): Component.RenderResultOf[
      (SyntaxTree[Argument, ArgContent], ArgSnapshot[A])
    ] = arg.render(pos).map(_ -> ArgSnapshot(arg, swap))
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
  def ascend[F[_], G[_]](
    path: Option[Either[ArgumentPath[F], ArgumentPath[G]]], pos: ArgPosition
  ): Option[Either[ArgumentPath[F], ArgumentPath[G]]] =
    path.map {
      case Left(p) => Left(Descent(pos, p))
      case Right(p) => Right(Descent(pos, p))
    }

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
  type Branches = Vector[SyntaxTree[Argument, ArgContent]]
  type Tree = SyntaxTree[Argument, ArgContent]
  case class WithExtraction[+A, +Extraction](
    value: A,
    extractions: Vector[Extraction] = Vector()
  ) {
    def map[B](f: A => B) = WithExtraction(f(value), extractions)
  }
  object WithExtraction {
    implicit def withExtractionMonoid[A: Monoid, E] = new Monoid[WithExtraction[A, E]] {
      def empty = WithExtraction(Monoid[A].empty)
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

  def swapOutSelf: SwapOutArg[Argument] = SwapOutArg {
    case Right(arg) => Right(arg)
    case Left(err) => Left(err)
  }

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

  def render[F[_]](
    pos: ArgPosition,
    path: Option[ArgumentPath[F]]
  ): RenderResultOf[(Tree, Option[F[Argument]])] =
    render(pos, swapOutSelf, path)

  def render[F[_], A](
    pos: ArgPosition,
    swapOutArg: SwapOutArg[A],
    path: ArgumentPath[F]
  ): RenderResultOf[(Tree, F[A])] =
    render(pos, swapOutArg, Some(path)).map { case (x, y) => x -> y.get } // require 1 extraction

  def render[F[_]](
    pos: ArgPosition,
    path: ArgumentPath[F]
  ): RenderResultOf[(Tree, F[Argument])] =
    render(pos, swapOutSelf, path)

  def render[A](
    pos: ArgPosition,
  ): RenderResultOf[Tree] = {
    // ascribe a type to the None to help out type inference
    render(pos, swapOutSelf, None: Option[ArgumentPath[ProFormExtraction]]).map {
      case (tree, itemOpt) =>
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
  // can appear as the complement of a preposition
  sealed trait Complement extends NonSubject
  // can be used predicatively with a copula (is not verb or adjective)
  sealed trait NounOrOblique extends Complement
  // can appear as the object of a preposition
  sealed trait Nominal extends Subject with Complement
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
    ): RenderBranches[F[A]] = path match {
      case Some(_) => invalid("Cannot extract from inside a pro-form.")
      case None => placeholder
          .map(p => valid(WithExtraction(Vector(leaf(ArgContent.text(p))))))
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
      def number = Some(Number.Singular)
      def person = Some(Person.Third)
    }
    case object who extends Noun {
      def wh(pos: ArgPosition) = pos match {
        case ArgPosition.Subj => "who"
        case _ => "whom"
      }
      val placeholder = Some("someone")
      // val instances = Set(NounPhrase(None, Some(true)))
      def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        case np @ NounPhrase(_) => Some(np)
        case _ => None
      }
    }
    case object what extends Noun {
      def wh(pos: ArgPosition) = "what"
      val placeholder = Some("something")
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
        // case arg @ NounPhrase(_) => Some(arg)
        // case arg @ Gerund(_, _) => Some(arg)
        // case arg @ ToInfinitive(_, _) => Some(arg)
        // case arg @ Attributive(_) => Some(arg)
        // case arg @ Finite(_) => Some(arg)
        // case arg @ FiniteComplement(_) => Some(arg)
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
        // case arg @ Prepositional(_, _) => Some(arg)
        // case arg @ Adverbial(_, _) => Some(arg)
        // case arg @ ToInfinitive(_, _) => Some(arg)
        // case arg @ Progressive(_, _, _) => Some(arg)
        // case arg @ Attributive(_) => Some(arg)
        case _ => None
      }
    }
    case object when extends Adverb {
      val whWord = Wh.when
      val placeholder = None
    }
    case object where extends Adverb with Noun {
      val whWord = Wh.where
      val placeholder = Some("somewhere")
      override def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        case arg @ NounPhrase(_) => Some(arg)
        // case arg @ Gerund(_, _) => Some(arg)
        // case arg @ Attributive(_) => Some(arg)
        case arg @ Prepositional(_, _) => Some(arg)
        case arg @ Adverbial(_, _) => Some(arg)
        // case arg @ ToInfinitive(_, _) => Some(arg)
        // case arg @ Progressive(_, _, _) => Some(arg)
        // case arg @ Attributive(_) => Some(arg)
        case _ => None
      }
    }
    case object how extends Adverb {
      val whWord = Wh.how
      val placeholder = None
      override def instantiateConcrete(arg: Concrete): Option[Concrete] = arg match {
        // case arg @ NounPhrase(_) => Some(arg)
        case arg @ Prepositional(_, _) => Some(arg)
        case arg @ Adverbial(_, _) => Some(arg)
        // case arg @ ToInfinitive(_, _) => Some(arg)
        case arg @ Verbal.Progressive(_, _) => Some(arg)
        case arg @ Predicative(_) => Some(arg)
        // case _ => Some(arg)
        case _ => None
      }
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
    // def proForms: Set[Argument.ProForm]

    final def symbol = {
      category
      // val pros = proForms.map(_.wh(ArgPosition.Subj)).toList.sortBy(_.toString).mkString("/")
      // if(pros.isEmpty) category
      // else s"$category {$pros}"
    }
  }

  // expletive 'it' or 'there'; can only appear as subject
  case class Expletive(expletive: Lexicon.Expletive) extends Concrete with Subject {
    def category = s"np[${expletive.form}]"
    // def proForms = Set()
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
        case Subj => valid(WithExtraction(Vector(leaf(ArgContent.text(expletive.form.toString)))))
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
    type Feats
    type Pred <: Predication { type GramFeats = Feats; type Self <: Pred }
    type Self <: Semantic.Aux[Feats, Pred]
    def pred: Option[Pred]
    def feats: Feats
    def withPred(p: Pred): Self

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

    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[F[_], A](
      pos: ArgPosition,
      swapOutArg: SwapOutArg[A],
      path: Option[Descent[F]]
    ) = {
      val swapOutPred = SwapOutPred[Pred, A] {
        case Right(pred) => swapOutArg.replace(Right(this.withPred(pred)))
        case Left(arg) => swapOutArg.replace(Right(arg))
      }
      pred.map(_.renderLax(feats, swapOutPred, path))
        .getOrElse(valid(WithExtraction(Vector(leaf(ArgContent.blank)))))
    }
  }
  object Semantic {
    type Aux[F, P] = Semantic { type Feats = F; type Pred = P }
  }

  case class NounPhrase(
    pred: Option[Predication.NounPhrase]
    // animacyConstraint: Option[Boolean]
  ) extends Semantic with Nominal {
    type Self = NounPhrase
    type Feats = Case
    type Pred = Predication.NounPhrase // { type GramFeats = Feats }
    def feats: Feats = Case.Accusative // NOTE: not used. ?

    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = true

    override def category = "np"
    override def withPred(p: Pred): Self = this.copy(pred = Some(p))

    // TODO: change to account for multiple possible animacies
    // def proForms =
    //   animate.map(anim => if(anim) ProForm.who else ProForm.what)
    //     .toSet

    // def animate: Option[Boolean] = pred.flatMap(_.animate).orElse(animacyConstraint)

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
      val swapOutPred = SwapOutPred[Predication.NounPhrase, A] {
        case Right(pred) => swapOutArg.replace(Right(this.copy(pred = Some(pred))))
        case Left(arg) => swapOutArg.replace(Right(arg))
          // TODO: perhaps consider erroring if there's a problem with animacy
        // case x => invalid(s"Trying to replace nominal pred of NP arg with: $x")
      }
      pred.map(_.renderLax(c, swapOutPred, path))
        .getOrElse(valid(WithExtraction(Vector(leaf(ArgContent.blank)))))
    }
  }
  object NounPhrase

  // NOTE: predications are optional because we can use arguments
  // to specify general syntactic subcategorization frames.

  // sealed trait NounPhrase extends Semantic with Nominal {
  //   type Feats <: Case
  //   type Pred = Predication.Nominal { type GramFeats = Feats }
  //   override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = true
  //   def person: Option[Person] = pred.flatMap(_.person)
  //   def number: Option[Number] = pred.flatMap(_.number)
  // }
  // object NounPhrase {
  //   case class Nom(pred: Predication.Nominal { type GramFeats = Case.Nominative }) {
  //     type Feats = Case.Nominative.type
  //     def category = "np[nom]"
  //   }
  //   case class Acc(pred: Predication.Nominal { type GramFeats = Case.Accusative }) {
  //     type Feats = Case.Accusative.type
  //     def category = "np[acc]"
  //   }
  //   case class Gen(pred: Predication.Nominal { type GramFeats = Case.Genitive }) {
  //     type Feats = Case.Genitive.type
  //     def category = "np[gen]"
  //   }
  // }

  // TODO: perhaps just collapse this together with noun phrase
  // and have some 'arbitrary span' type of argument.
  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    pred: Option[Predication.Adverbial],
    adverbials: Set[ProForm.Adverb]
  ) extends Semantic with NonNominal {
    type Self = Adverbial
    type Feats = Unit
    type Pred = Predication.Adverbial
    override def category = "adv"
    override def feats: Feats = ()
    override def withPred(p: Pred) = this.copy(pred = Some(p))
  }

  case class Prepositional(
    pred: Option[Predication.Prepositional],
    adverbials: Set[ProForm.Adverb] = Set()
  ) extends Semantic with NounOrOblique with NonNominal {
    type Self = Prepositional
    type Feats = Unit
    type Pred = Predication.Prepositional
    override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]): Boolean = true
    override def category = "pp"
    override def feats: Feats = ()
    override def withPred(p: Pred) = this.copy(pred = Some(p))
  }

  // NOTE: maybe 'how' is the only acceptable pro-form here?
  // i guess 'what' appears for adjectives...
  // can subjects appear? she wants Ben happy. What does she want? Ben happy. idk.
  // TODO: probably add subjects back in. maybe be lenient.
  case class Predicative(
    pred: Option[Predication.NonCopularVerbLike]
  ) extends Semantic with Complement {
    type Self = Predicative
    type Form = VPForm.Predicative.type
    type Feats = (VPForm, Aspect)
    type Pred = Predication.NonCopularVerbLike
    def form = VPForm.Predicative
    def aspect = Aspect.default
    override def feats = (form, aspect)
    // TODO: add error case to this, or (preferably?) split out attributive predications
    override def category = "vp[adj/pt]"
    override def withPred(pred: Predication.NonCopularVerbLike): Self = this.copy(pred = Some(pred))
  }

  sealed trait Verbal extends Semantic {
    // TODO: this would probably be way easier to do with typeclasses lmao
    type Self <: Verbal.Aux[Form, Pred]
    type Form <: VPForm
    type Feats = (VPForm, Aspect)
    type Pred <: Predication.VerbLike { type Self <: Pred }
    // def pred: Option[Pred]
    // def withPred(pred: Option[Pred]): Self // for SwapOutPred
    def feats: Feats = (form, aspect)
    def form: Form
    def aspect: Aspect
  }
  object Verbal {
    type Aux[Form0, Pred0] = Verbal { type Form = Form0; type Pred = Pred0 }
    case class Gerund(
      pred: Option[Predication.VerbLike],
      aspect: Aspect
    ) extends Verbal with Nominal {
      type Self = Gerund
      type Form = VPForm.Progressive.type
      // type Pred = Predication.VerbLike
      type Pred = Predication.VerbLike
      override def category = "vp[g]"
      override def form = VPForm.Progressive
      override def withPred(pred: Predication.VerbLike) = this.copy(pred = Some(pred))
      // TODO should allow with 'whose' on subject?
      override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]) = false
      override def person = Some(Person.Third)
      override def number = Some(Number.Singular)
    }
    // includeSubject: Boolean,
    case class BareInfinitive(
      pred: Option[Predication.VerbLike],
      aspect: Aspect
    ) extends Verbal with Complement {
      type Self = BareInfinitive
      type Form = VPForm.BareInfinitive.type
      type Pred = Predication.VerbLike
      override def category = "vp[b]"
      override def form = VPForm.BareInfinitive
      override def withPred(pred: Predication.VerbLike): Self = this.copy(pred = Some(pred))
    }

    // TODO: fix up infinitives. all sorts of syntactic possibilities here?
    case class ToInfinitive(
      pred: Option[Predication.VerbLike],
      aspect: Aspect
    ) extends Verbal with Complement {
      type Self = ToInfinitive
      type Form = VPForm.ToInfinitive.type
      type Pred = Predication.VerbLike
      override def category = "vp[to]"
      override def form = VPForm.ToInfinitive
      override def withPred(pred: Predication.VerbLike): Self = this.copy(pred = Some(pred))
    }

    case class Progressive(
      pred: Option[Predication.VerbLike],
      aspect: Aspect
      // adverbials: Set[ProForm.Adverb]
    ) extends Verbal with Complement {
      type Self = Progressive
      type Form = VPForm.Progressive.type
      type Pred = Predication.VerbLike
      override def category = "vp[ng]"
      override def form = VPForm.Progressive
      override def withPred(pred: Predication.VerbLike): Self = this.copy(pred = Some(pred))
      // override def proForms = adverbials.map(x => x: ProForm)
    }
  }

  // TODO infinitive embedded question, finite embedded question, free relative clause.
  // these may be weird due to corresponding to multiple possible argument types.
  // but maybe they aren't weird. idk. might be fine.

  sealed trait Clausal extends Semantic {
    // TODO: this would probably be way easier to do with typeclasses lmao
    type Self <: Clausal.Aux[Form]
    type Form <: SForm
    type Feats = (SForm, Aspect)
    type Pred = Predication.Clausal
    // def pred: Option[Pred]
    // def withPred(pred: Option[Pred]): Self // for SwapOutPred
    def feats: (Form, Aspect) = (form, aspect)
    def form: Form
    def aspect: Aspect
  }
  object Clausal {
    type Aux[Form0] = Clausal { type Form = Form0 }

    // nominal use of gerund/progressive; can be object of prep or subject
    // TODO possessive subject option
    case class Gerund(
      pred: Option[Predication.Clausal],
      aspect: Aspect
    ) extends Clausal with Nominal {
      type Self = Gerund
      type Form = SForm.Progressive.type
      override def category = "s[g]"
      override def form = SForm.Progressive
      override def withPred(pred: Predication.Clausal) = this.copy(pred = Some(pred))
      // override def proForms = Set(ProForm.what)
      // TODO should allow with 'whose' on subject?
      override def allowPiedPiping(path: ArgumentPath[ProFormExtraction]) = false
      override def person = Some(Person.Third)
      override def number = Some(Number.Singular)
    }

    // various possible forms of complements:
    // may be specific to a predicate's (or subordinator's!) subcat frame.
    // TODO: fix up infinitives. all sorts of syntactic possibilities here.
    // Not sure what will be best.
    case class ForToInfinitive(
      pred: Option[Predication.Clausal],
      aspect: Aspect
    ) extends Clausal with Nominal { // TODO can be 'why' as well? does this get it?
      type Self = ForToInfinitive
      type Form = SForm.ForToInfinitive.type
      def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      // TODO: with Subject. or, just add a new argument type for subj inf?
      // def complementizer: InfinitiveComplementizer = Lexicon.InfinitiveComplementizer.`for`

      override def form = SForm.ForToInfinitive
      override def category = "s[for-to]"

      override def person = Some(Person.Third)
      override def number = Some(Number.Singular)
    }

    case class Progressive(
      pred: Option[Predication.Clausal],
      aspect: Aspect
      // adverbials: Set[ProForm.Adverb]
    ) extends Clausal with Complement {
      type Self = Progressive
      type Form = SForm.Progressive.type
      override def form = SForm.Progressive
      override def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      override def category = "s[ng]"
      // override def proForms = adverbials.map(x => x: ProForm)
    }

    // with Subject? but cannot appear in subj except as an answer
    case class Finite(
      pred: Option[Predication.Clausal],
      override val form: SForm.Finite,
      aspect: Aspect
    ) extends Clausal with Complement {
      type Self = Finite
      type Form = SForm.Finite
      def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      override def category = "s[dcl]"
      // override def proForms = Set(ProForm.what)
    }

    case class Inverted(
      pred: Option[Predication.Clausal],
      override val form: SForm.Inverted,
      aspect: Aspect
    ) extends Clausal {
      type Self = Inverted
      type Form = SForm.Inverted
      def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      override def category = "s[inv]"
    }

    // is this always necessarily 'that'? maybe remove complementizer slot?
    // TODO add 'that'
    case class FiniteComplement(
      pred: Option[Predication.Clausal],
      form: SForm.FiniteComplement,
      aspect: Aspect
    ) extends Clausal with Complement with Subject {
      type Self = FiniteComplement
      type Form = SForm.FiniteComplement
      def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      override def category = "s[comp]"

      def person = Some(Person.Third)
      def number = Some(Number.Singular)
      def complementizer: Complementizer = Lexicon.Complementizer.that
    }

    case class FiniteQuestion(
      pred: Option[Predication.Clausal],
      form: SForm.Inverted,
      aspect: Aspect,
      target: Option[ArgumentPath.Descent[Extraction]]
    ) extends Clausal {
      // whItemPath: Option[ArgumentPath.Descent[Extraction]]
      type Self = FiniteQuestion
      type Form = SForm.Inverted
      override def category = "s[q]"
      override def withPred(pred: Predication.Clausal): Self = this.copy(pred = Some(pred))
      // override def clauseType = ClauseType.Inverted
      // override def proForms = Set()

      import ArgPosition._, ArgumentPath.Descent
      override def renderLax[F[_], A](
        pos: ArgPosition,
        swapOutArg: SwapOutArg[A],
        path: Option[Descent[F]]
      ) = {
        // TODO: could change rendering to take multiple paths but only allow one extraction.
        if(path.nonEmpty) invalid("Cannot descend into a question") else {
          super.renderLax(pos, swapOutArg, target).andThen {
            case wext @ WithExtraction(branches, extractions) =>
              if(target.isEmpty) {
                require(extractions.size == 0)
                valid(WithExtraction(branches))
              } else {
                require(extractions.size == 1)
                val Extraction(arg, focusPath, swap) = extractions.head
                // TODO can change to another arg position later maybe. Spec?
                arg.render(ArgPosition.Subj, swap, focusPath).map {
                  case (whItemTree, ProFormExtraction(pro, swap)) =>
                    WithExtraction(whItemTree +: branches)
                }
              }
          }
        }
      }

      // blocks extraction.
      override def extractionPaths: Set[ArgumentPath[Extraction]] = Set()
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
      .map { case WithExtraction(trees, items) =>
        // require correct number of extractions
        val isValid = (path.isEmpty && items.isEmpty) ||
          (path.nonEmpty && items.size == 1)
        if(!isValid) {
          println("Data:")
          println(this)
          println(feats)
          println(path)
          println("Branches:")
          trees.foreach { tree =>
            println(SyntaxTree.gloss[Argument, ArgContent](tree, _.symbol, _.text))
          }
          println("Extractions:")
          items.foreach { item =>
            println(item)
          }
        }
        require(isValid)
        trees -> items.headOption
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
      case None => valid(WithExtraction(Vector(leaf(ArgContent.text(getForm(features))))))
    }
  }

  case class NounPhrase(
    span: Option[ESpan],
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

  case class Prepositional(
    index: Option[Int],
    prep: Preposition,
    arg: Option[Argument.Complement],
    proxy: Option[ProxyArgument[Prepositional]] = None
  ) extends Predication {
    type Self = Prepositional
    type GramFeats = Unit
    def arguments = arg.toVector
    import ArgPosition._, ArgumentPath._
    override def renderLax[F[_], A](
      feats: Unit,
      swapOutPred: SwapOutPred[Self, A],
      path: Option[Descent[F]]
    ): RenderBranches[F[A]] = {
      if(path.exists(_.position != Arg(0))) {
        invalid(s"Can't descend into argument for extraction from oblique: ${path.get}")
      } else Vector(
        Option(
          valid(
            WithExtraction(
              leaf(ArgContent.text(prep.form, pos = Some("prep")))
            )
          )
        ),
        arg.map(
          _.renderLaxWithPath(
            Arg(0),
            SwapOutArg {
              case Right(arg: Argument.Complement) => swapOutPred.replace(
                Right(this.copy(arg = Some(arg)))
              )
              case Right(otherArg) => Left(SubstitutionFailure(otherArg, Descent(Arg(0), End(Target))))
              case Left(failure) =>
                val newFailure = failure.ascend(Arg(0))
                proxy.flatMap(_.recover(newFailure))
                  .map(swapOutPred.replace)
                  .getOrElse(Left(newFailure))
            },
            ArgumentPath.descend(path, Arg(0))
          )
        )
      ).flatten.map(_.map(_.map(x => Vector(x))))
        .foldA
        // .nonEmptySequence[ValidatedNec[RenderError, *], WithExtraction[Tree, F[A]]]
        // .sequenceA
    }
  }

  case class Adverbial(
    span: Option[ESpan],
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
      case None => valid(WithExtraction(Vector(leaf(ArgContent.text(form)))))
    }
  }

  sealed trait VerbLike extends Predication {
    def index: Option[Int]
    type Self <: VerbLike
    type GramFeats = (VPForm, Aspect)
    def predPOS: String

    def renderVerbChain(
      form: VPForm,
      aspect: Aspect,
      needsFlippable: Boolean
    ): Either[NonEmptyChain[String], NonEmptyList[String]]

    def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A]

    import Validated.valid
    // import LabeledTree.{leaf, leaves, node}
    import SyntaxTree.{node, leaf}
    import Component.WithExtraction

    override def renderLax[F[_], A](
      feats: (VPForm, Aspect),
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): Component.RenderBranches[F[A]] = {
      validatePath(path) *> {
        val args: RenderBranches[F[A]] = renderArguments(path, swapOutPred)
        def makeVerbTree(chain: NonEmptyList[String]) = {
          if(chain.size == 1) Vector(
            leaf(ArgContent.text(chain.head, pos = Some(predPOS)))
          ) else Vector(
            leaf(ArgContent.text(chain.init.mkString(" "), pos = Some("aux"))),
            leaf(ArgContent.text(chain.last, pos = Some(predPOS)))
          )
        }

        Validated.fromEither(
          renderVerbChain(feats._1, feats._2, false).left.map(_.map(error(_)))
        ).andThen { verbChain =>
          val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
          Vector(
            verb, args
          ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
        }
        // [ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
        // .foldA
      }
    }

    def validatePath[F[_]](
      path: Option[ArgumentPath.Descent[F]]
    ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
      case None => valid(())
      case Some(ArgPosition.Subj) => invalid(s"Cannot descend into subject position for VP.")
      case Some(ArgPosition.Arg(i)) if arguments.size > i => valid(())
      case Some(pos) => invalid(s"Cannot descend into argument position for extraction: $pos")
    }

    def renderArguments[F[_], A](
      path: Option[ArgumentPath.Descent[F]],
      swapOutPred: SwapOutPred[Self, A]
    ): Component.RenderBranches[F[A]] = {
      arguments.zipWithIndex.foldMapA[
        ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]
      ] { case (arg, index) =>
        arg.renderLaxWithPath(
          ArgPosition.Arg(index),
          swapOutArg(swapOutPred, index),
          ArgumentPath.descend(path, ArgPosition.Arg(index))
        ).map(_.map(Vector(_)))
      }
    }
  }

  sealed trait NonCopularVerbLike extends VerbLike {
    type Self <: NonCopularVerbLike
  }

  case class Verbal(
    index: Option[Int],
    verb: Verb,
    isPassive: Boolean,
    arguments: Vector[NonSubject],
    proxy: Option[ProxyArgument[Verbal]] = None
  ) extends NonCopularVerbLike {
    type Self = Verbal
    override def predPOS = "verb"
    override def renderVerbChain(vpForm: VPForm, aspect: Aspect, needsFlippable: Boolean) = {
      def pastParticiple = verb.forms.pastParticiple.toString
      vpForm match {
        case VPForm.Predicative =>
          if(!isPassive) {
            Left(NonEmptyChain.one("Cannot construct predicative verb from active form."))
          } else Right(NonEmptyList.of(pastParticiple))
        case form: VPForm.NotPredicative =>
          if(isPassive) {
            Right(form.getCopulaAuxChain(aspect).append(pastParticiple))
              // tan.getCopulaAuxChain(otherType, subject)
          } else {
            Right(
              form.getAuxChain(
                verb.forms, aspect, ensureDoSupport = needsFlippable
              )
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
    // import ArgumentPath._
    // import ArgPosition._
    // def doSomething(
    //   subject: Argument.Subject,
    //   // modifiers: Vector[NonNominal],
    //   tan: TAN
    // ) = Verbal(
    //   None,
    //   subject, Verb(InflectedForms.doForms), false,
    //   Vector(Argument.ProForm.what),// +: modifiers,
    //   tan,
    //   Some(
    //     ProxyArgument(
    //       Descent(Arg(0), End(Target)),
    //       arg => arg match {
    //         case bare: Argument.BareInfinitive =>
    //           bare.pred.map(Right(_))
    //         case _ => None
    //       }
    //     )
    //   )
    // )
  }

  case class Adjectival(
    index: Option[Int],
    adjective: Adjective,
    arguments: Vector[NonNominal],
    proxy: Option[ProxyArgument[Adjectival]] = None
  ) extends NonCopularVerbLike {
    type Self = Adjectival
    override def predPOS = "adj"
    override def renderVerbChain(vpForm: VPForm, aspect: Aspect, needsFlippable: Boolean) = {
      val adj = adjective.form.toString
      vpForm match {
        case VPForm.Predicative => Right(NonEmptyList.of(adj))
        case form: VPForm.NotPredicative =>
          val aux = form.getCopulaAuxChain(aspect).append(adj)
          Right(aux)
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

  case class Copular(
    index: Option[Int],
    argument: NounOrOblique,
    modifiers: Vector[NonNominal],
    proxy: Option[ProxyArgument[Copular]] = None
  ) extends VerbLike {
    type Self = Copular
    def arguments = argument +: modifiers
    def predPOS = "copula"

    override def renderVerbChain(
      vpForm: VPForm,
      aspect: Aspect,
      needsFlippable: Boolean
    ): Either[NonEmptyChain[String], NonEmptyList[String]] = vpForm match {
      case VPForm.Predicative =>
        Left(NonEmptyChain.one("Cannot construct predicative clause from a copula."))
      case form: VPForm.NotPredicative =>
        Right(form.getCopulaAuxChain(aspect))
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

  case class Clausal(
    subject: Argument.Subject,
    predicate: VerbLike,
    proxy: Option[ProxyArgument[Clausal]] = None
  ) extends Predication {
    type Self = Clausal
    type GramFeats = (SForm, Aspect)
    def arguments: Vector[Argument] = predicate.arguments

    import Validated.valid
    // import LabeledTree.{leaf, leaves, node}
    import SyntaxTree.{node, leaf}
    import Component.WithExtraction

    override def renderLax[F[_], A](
      feats: (SForm, Aspect),
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): Component.RenderBranches[F[A]] = {
      val (sForm, aspect) = feats
      // import feats.{clauseType, includeSubject}
      validatePath(path) *> {
        val args: RenderBranches[F[A]] = renderArguments(path, swapOutPred)
        def makeVerbTree(chain: NonEmptyList[String]) = {
          if(chain.size == 1) Vector(
            leaf(ArgContent.text(chain.head, pos = Some(predicate.predPOS)))
          ) else Vector(
            leaf(ArgContent.text(chain.init.mkString(" "), pos = Some("aux"))),
            leaf(ArgContent.text(chain.last, pos = Some(predicate.predPOS)))
          )
        }
        val comp: Component.RenderBranches[F[A]] = valid(
          WithExtraction(
            sForm.getComplementizer.toVector.map(comp =>
              leaf(ArgContent.text(comp, pos = Some("comp")))
            )
          )
        )
        renderSubject(swapOutPred, path).andThen { subjValue =>
          val flipAuxiliary = subjValue.value.exists(_.nonEmpty) && (
            sForm match {
              case SForm.Inverted(_) => true
              // case SForm.FiniteQuestion(_, _) => true
              case _ => false
            }
          )
          Validated.fromEither(
            predicate.renderVerbChain(sForm.getVPForm(subject), aspect, flipAuxiliary)
              .left.map(_.map(error(_)))
          ).andThen { verbChain =>
            val subj: RenderBranches[F[A]] = valid(subjValue.map(x => Vector(x)))

            if(flipAuxiliary) { // aux flip
              val aux: RenderBranches[F[A]] = valid(
                WithExtraction(
                  Vector(leaf(ArgContent.text(verbChain.head, pos = Some("aux"))))))
              NonEmptyList.fromList(verbChain.tail) match {
                case None =>
                  Vector(
                    comp, aux, subj, args
                  ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
                case Some(verbTail) =>
                  val verb = valid(WithExtraction(makeVerbTree(verbTail)))
                  Vector(
                    comp, aux, subj, verb, args
                  ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
                    // .foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
              }
            } else {
              val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
              Vector(
                comp, subj, verb, args
              ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
            }
          }
        }
      }
    }

    def validatePath[F[_]](
      path: Option[ArgumentPath.Descent[F]]
    ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
      case None => valid(())
      case Some(ArgPosition.Subj) => valid(())
      case _ => predicate.validatePath(path)
    }

    def renderSubject[F[_], A](
      swapOutPred: SwapOutPred[Self, A],
      path: Option[ArgumentPath.Descent[F]]
    ): Component.RenderTree[F[A]] = {
      import ArgumentPath._, ArgPosition._
      subject.renderLaxWithPath(
        Subj,
        SwapOutArg {
          case Right(arg: Argument.Subject) =>
            swapOutPred.replace(Right(this.copy(subject = arg)))
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

    def swapOutInnerPred[A](swapOutPred: SwapOutPred[Self, A]): SwapOutPred[VerbLike, A] = SwapOutPred {
      case Left(arg) => swapOutPred.replace(Left(arg))
      case Right(predicate) => swapOutPred.replace(Right(this.copy(predicate = predicate)))
    }

    import ArgumentPath._, ArgPosition.Arg
    private[this] def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
      SwapOutArg {
        // case Right(arg: Subject) if index == 0 =>
        //   swapOutPred.replace(Right(this.copy(subject = arg)))
        // case Right(arg: NonSubject) if index > 0 =>
        case Right(arg: NonSubject) =>
          predicate.swapOutArg(swapOutInnerPred(swapOutPred), index).replace(Right(arg))
        case Right(otherArg) =>
          Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
        case Left(failure) =>
          val newFailure = failure.ascend(Arg(index))
          proxy.flatMap(_.recover(newFailure))
            .map(swapOutPred.replace)
            .getOrElse(Left(newFailure))
      }

    def renderArguments[F[_], A](
      path: Option[ArgumentPath.Descent[F]],
      swapOutPred: SwapOutPred[Self, A]
    ): Component.RenderBranches[F[A]] = {
      arguments.zipWithIndex.foldMapA[
        ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]
      ] { case (arg, index) =>
        arg.renderLaxWithPath(
          ArgPosition.Arg(index),
          swapOutArg(swapOutPred, index),
          ArgumentPath.descend(path, ArgPosition.Arg(index))
        ).map(_.map(Vector(_)))
      }
    }

    override def argumentPaths: Set[ArgumentPath.Descent[ProFormExtraction]] = {
      import ArgPosition._
      subject.argumentPaths.map(_.ascend(Subj)) ++ predicate.argumentPaths
      // arguments.zipWithIndex.flatMap { case (arg, index) =>
      //   arg.argumentPaths.map(_.ascend(Arg(index)))
      // }
    }
    // TODO island constraints?
    override def extractionPaths: Set[ArgumentPath.Descent[Extraction]] =
      subject.extractionPaths.map(_.ascend(ArgPosition.Subj)) ++ predicate.extractionPaths
  }

  // case class ClauseFeats(
  //   clauseType: ClauseType,
  //   includeSubject: Boolean
  // )

  // sealed trait Clausal extends Predication {
  //   def index: Option[Int]
  //   type Self <: Clausal
  //   // type Self = Clausal
  //   type GramFeats = ClauseFeats
  //   def subject: Argument.Subject
  //   def withSubject(subject: Argument.Subject): Self
  //   def tan: TAN
  //   def arguments: Vector[Argument]

  //   def predPOS: String

  //   def renderVerbChain(
  //     clauseType: ClauseType,
  //     needsFlippable: Boolean
  //   ): Either[NonEmptyChain[String], NonEmptyList[String]]

  //   import Validated.valid
  //   // import LabeledTree.{leaf, leaves, node}
  //   import SyntaxTree.{node, leaf}
  //   import Component.WithExtraction

  //   override def renderLax[F[_], A](
  //     feats: ClauseFeats,
  //     swapOutPred: SwapOutPred[Self, A],
  //     path: Option[ArgumentPath.Descent[F]]
  //   ): Component.RenderBranches[F[A]] = {
  //     import feats.{clauseType, includeSubject}
  //     val frontAuxiliary = clauseType == ClauseType.Inverted
  //     validatePath(includeSubject, path) *> {
  //       val args: RenderBranches[F[A]] = renderArguments(path, swapOutPred)
  //       def makeVerbTree(chain: NonEmptyList[String]) = {
  //         if(chain.size == 1) Vector(
  //           leaf(ArgContent.text(chain.head, pos = Some(predPOS)))
  //         ) else Vector(
  //           leaf(ArgContent.text(chain.init.mkString(" "), pos = Some("aux"))),
  //           leaf(ArgContent.text(chain.last, pos = Some(predPOS)))
  //         )
  //       }
  //       if(includeSubject) {
  //         renderSubject(includeSubject, swapOutPred, path).andThen { subjValue =>
  //           val flipAuxiliary = frontAuxiliary && subjValue.value.exists(_.nonEmpty)
  //             // .foldMap(_.text.combineAll)
  //           Validated.fromEither(
  //             renderVerbChain(clauseType, flipAuxiliary).left.map(_.map(error(_)))
  //           ).andThen { verbChain =>
  //             val subj: RenderBranches[F[A]] = valid(subjValue.map(x => Vector(x)))
  //             if(flipAuxiliary) { // aux flip
  //               val aux: RenderBranches[F[A]] = valid(
  //                 WithExtraction(
  //                   Vector(leaf(ArgContent.text(verbChain.head, pos = Some("aux"))))))
  //               NonEmptyList.fromList(verbChain.tail) match {
  //                 case None =>
  //                   Vector(
  //                     aux, subj, args
  //                   ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //                 case Some(verbTail) =>
  //                   val verb = valid(WithExtraction(makeVerbTree(verbTail)))
  //                   Vector(
  //                     aux, subj, verb, args
  //                   ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //                     // .foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //               }
  //             } else {
  //               val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
  //               Vector(
  //                 subj, verb, args
  //               ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //             }
  //           }
  //         }
  //       } else {
  //         Validated.fromEither(
  //           renderVerbChain(clauseType, false).left.map(_.map(error(_)))
  //         ).andThen { verbChain =>
  //           val verb: RenderBranches[F[A]] = valid(WithExtraction(makeVerbTree(verbChain)))
  //           Vector(
  //             verb, args
  //           ).foldA[ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //         }
  //         // [ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]]
  //         // .foldA
  //       }
  //     }.andThen(res =>
  //         if(res.extractions.size > 1) {
  //           invalid("Somehow produced multiple extractions.")
  //         } else if(res.extractions.nonEmpty && path.isEmpty) {
  //           invalid("Produced a spurious extraction.")
  //         } else valid(res)
  //       )
  //   }

  //   def validatePath[F[_]](
  //     includeSubject: Boolean,
  //     path: Option[ArgumentPath.Descent[F]]
  //   ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
  //     case None => valid(())
  //     case Some(ArgPosition.Subj) if includeSubject => valid(())
  //     case Some(ArgPosition.Arg(i)) if arguments.size > i => valid(())
  //     case Some(pos) => invalid(s"Cannot descend into argument position for extraction: $pos")
  //   }

  //   def renderSubject[F[_], A](
  //     includeSubject: Boolean,
  //     swapOutPred: SwapOutPred[Self, A],
  //     path: Option[ArgumentPath.Descent[F]]
  //   ): Component.RenderTree[F[A]] = {
  //     import ArgumentPath._, ArgPosition._
  //     subject.renderLaxWithPath(
  //       Subj,
  //       SwapOutArg {
  //         case Right(arg: Argument.Subject) =>
  //           swapOutPred.replace(Right(this.withSubject(arg)))
  //         case Right(otherArg) =>
  //           Left(SubstitutionFailure(otherArg, Descent(Subj, End(Target))))
  //         case Left(failure) =>
  //           val newFailure = failure.ascend(Subj)
  //           proxy.flatMap(_.recover(newFailure))
  //             .map(swapOutPred.replace)
  //             .getOrElse(Left(newFailure))
  //       },
  //       ArgumentPath.descend(path, Subj)
  //     )
  //   }

  //   def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A]

  //   def renderArguments[F[_], A](
  //     path: Option[ArgumentPath.Descent[F]],
  //     swapOutPred: SwapOutPred[Self, A]
  //   ): Component.RenderBranches[F[A]] = {
  //     arguments.zipWithIndex.foldMapA[
  //       ValidatedNec[RenderError, *], WithExtraction[Branches, F[A]]
  //     ] { case (arg, index) =>
  //       arg.renderLaxWithPath(
  //         ArgPosition.Arg(index),
  //         swapOutArg(swapOutPred, index),
  //         ArgumentPath.descend(path, ArgPosition.Arg(index))
  //       ).map(_.map(Vector(_)))
  //     }
  //   }

  //   override def argumentPaths: Set[ArgumentPath.Descent[ProFormExtraction]] = {
  //     import ArgPosition._
  //     subject.argumentPaths.map(_.ascend(Subj)) ++ super.argumentPaths
  //     // arguments.zipWithIndex.flatMap { case (arg, index) =>
  //     //   arg.argumentPaths.map(_.ascend(Arg(index)))
  //     // }
  //   }
  //   // TODO island constraints?
  //   override def extractionPaths: Set[ArgumentPath.Descent[Extraction]] =
  //     subject.extractionPaths.map(_.ascend(ArgPosition.Subj)) ++ super.extractionPaths
  // }

  // case class Copular(
  //   index: Option[Int],
  //   subject: Argument.Subject,
  //   argument: NounOrOblique,
  //   modifiers: Vector[NonNominal],
  //   tan: TAN,
  //   proxy: Option[ProxyArgument[Copular]] = None
  // ) extends Clausal {
  //   type Self = Copular
  //   def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
  //   def arguments = argument +: modifiers
  //   def predPOS = "be"

  //   override def renderVerbChain(
  //     clauseType: ClauseType,
  //     needsFlippable: Boolean
  //   ): Either[NonEmptyChain[String], NonEmptyList[String]] = clauseType match {
  //     case ClauseType.Attributive =>
  //       Left(NonEmptyChain.one("Cannot construct attributive clause from a copula."))
  //     case otherType: ClauseType.VerbalClauseType =>
  //       tan.getCopulaAuxChain(otherType, subject)
  //   }
  //   import ArgPosition._, ArgumentPath._
  //   def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
  //     SwapOutArg {
  //       case Right(arg: NounOrOblique) if index == 0 =>
  //         swapOutPred.replace(Right(this.copy(argument = arg)))
  //       case Right(arg: NonNominal) if index > 0 =>
  //         swapOutPred.replace(Right(this.copy(modifiers = modifiers.updated(index, arg))))
  //       case Right(otherArg) =>
  //         Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
  //       case Left(failure) =>
  //         val newFailure = failure.ascend(Arg(index))
  //         proxy.flatMap(_.recover(newFailure))
  //           .map(swapOutPred.replace)
  //           .getOrElse(Left(newFailure))
  //     }
  // }

  // sealed trait NonCopular extends Clausal {
  //   // type Self <: NonCopular
  // }

  // case class Adjectival(
  //   index: Option[Int],
  //   subject: Argument.Subject,
  //   adjective: Adjective,
  //   arguments: Vector[NonNominal],
  //   tan: TAN,
  //   proxy: Option[ProxyArgument[Adjectival]] = None
  // ) extends NonCopular {
  //   type Self = Adjectival
  //   def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
  //   override def predPOS = "adj"
  //   override def renderVerbChain(clauseType: ClauseType, needsFlippable: Boolean) = {
  //     val adj = adjective.form.toString
  //     clauseType match {
  //       case ClauseType.Attributive => Right(NonEmptyList.of(adj))
  //       case otherType: ClauseType.VerbalClauseType =>
  //         val aux = tan.getCopulaAuxChain(otherType, subject)
  //         aux.map(_.append(adj))
  //     }
  //   }
  //   import ArgPosition._, ArgumentPath._
  //   def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
  //     SwapOutArg {
  //       case Right(arg: NonNominal) =>
  //         swapOutPred.replace(Right(this.copy(arguments = arguments.updated(index, arg))))
  //       case Right(otherArg) =>
  //         Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
  //       case Left(failure) =>
  //         val newFailure = failure.ascend(Arg(index))
  //         proxy.flatMap(_.recover(newFailure))
  //           .map(swapOutPred.replace)
  //           .getOrElse(Left(newFailure))
  //     }
  // }

  // case class Verbal(
  //   index: Option[Int],
  //   subject: Argument.Subject,
  //   verb: Verb,
  //   isPassive: Boolean,
  //   arguments: Vector[NonSubject],
  //   tan: TAN,
  //   proxy: Option[ProxyArgument[Clausal]] = None
  // ) extends NonCopular {
  //   type Self = Clausal
  //   def withSubject(subject: Argument.Subject): Self = this.copy(subject = subject)
  //   override def predPOS = "verb"
  //   override def renderVerbChain(clauseType: ClauseType, needsFlippable: Boolean) = {
  //     def pastParticiple = verb.forms.pastParticiple.toString
  //     clauseType match {
  //       case ClauseType.Attributive =>
  //         if(!isPassive) {
  //           Left(NonEmptyChain.one("Cannot construct attributive clause from active form."))
  //         } else Right(NonEmptyList.of(pastParticiple))
  //       case otherType: ClauseType.VerbalClauseType =>
  //         if(isPassive) {
  //           val aux = tan.getCopulaAuxChain(otherType, subject)
  //           aux.map(_ append pastParticiple)
  //         } else {
  //           tan.getAuxChain(
  //             verb.forms, otherType, subject,
  //             ensureDoSupport = needsFlippable
  //           )
  //         }
  //     }
  //   }
  //   import ArgPosition._, ArgumentPath._
  //   def swapOutArg[A](swapOutPred: SwapOutPred[Self, A], index: Int): SwapOutArg[A] =
  //     SwapOutArg {
  //       case Right(arg: NonSubject) =>
  //         swapOutPred.replace(Right(this.copy(arguments = arguments.updated(index, arg))))
  //       case Right(otherArg) =>
  //         Left(SubstitutionFailure(otherArg, Descent(Arg(index), End(Target))))
  //       case Left(failure) =>
  //         val newFailure = failure.ascend(Arg(index))
  //         proxy.flatMap(_.recover(newFailure))
  //           .map(swapOutPred.replace)
  //           .getOrElse(Left(newFailure))
  //     }
  // }
  // object Verbal {
  //   // maybe also 'what happened _?' type of pro-form?
  //   // XXX change to a pro-form
  //   import ArgumentPath._
  //   import ArgPosition._
  //   def doSomething(
  //     subject: Argument.Subject,
  //     // modifiers: Vector[NonNominal],
  //     tan: TAN
  //   ) = Verbal(
  //     None,
  //     subject, Verb(InflectedForms.doForms), false,
  //     Vector(Argument.ProForm.what),// +: modifiers,
  //     tan,
  //     Some(
  //       ProxyArgument(
  //         Descent(Arg(0), End(Target)),
  //         arg => arg match {
  //           case bare: Argument.BareInfinitive =>
  //             bare.pred.map(Right(_))
  //           case _ => None
  //         }
  //       )
  //     )
  //   )
  // }

}
