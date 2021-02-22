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

sealed trait ArgumentPath[A] extends Product with Serializable {
  import ArgumentPath._
  // def isExtraction: Boolean = this match {
  //   case ExtractionDescent(_, _) => true
  //   case Extraction(_) => true
  //   case FocalDescent(_, _) => false
  //   case Focus => false
  // }
  def ascend(position: ArgPosition): Descent[A] = Descent(position, this)
  def descent: Option[Descent[A]] = this match {
    case d: Descent[A] => Some(d)
    case _ => None
  }
}


object ArgumentPath {

  case class Descent[A](
    position: ArgPosition,
    next: ArgumentPath[A]
  ) extends ArgumentPath[A]
  case class End[A](processor: Processor[A]) extends ArgumentPath[A]

  sealed trait Processor[A] {
    def process(arg: Argument, position: ArgPosition): Component.RenderResultOf[
      (LabeledTree.Node[String, String], A)
    ]
  }

  // case class Extraction[A](arg: Argument, innerPath: ArgumentPath[A])
  // extractions pull out arguments and provide a nested path to a focus inside.
  import LabeledTree.{leaves}
  import Validated.valid
  import Component.WithExtraction
  type Extraction = (Argument, ArgumentPath[Argument.ProForm])
  case class Extract(
    innerPath: ArgumentPath[Argument.ProForm]
  ) extends Processor[Extraction] {
    def process(arg: Argument, pos: ArgPosition): Component.RenderResultOf[
      (LabeledTree.Node[String, String], Extraction)
    ] = arg match {
      case pro: Argument.ProForm =>
        valid(leaves() -> (pro -> innerPath))
      case arg: Argument.Expletive =>
        arg.invalid("Cannot extract an expletive.")
      case arg: Argument.Semantic =>
        if(arg.allowPiedPiping(innerPath)) {
          valid(leaves() -> (arg -> innerPath))
        } else arg.invalid("Extraction of concrete argument not allowed.")
    }
  }

  type Focus = Focus.type
  case object Focus extends Processor[(Argument.ProForm)] {
    def process(arg: Argument, pos: ArgPosition): Component.RenderResultOf[
      (LabeledTree.Node[String, String], Argument.ProForm)
    ] = arg match {
      case pro: Argument.ProForm =>
        valid(leaves(pro.symbol -> pro.wh(pos)), pro)
      case arg: Argument.Concrete =>
        arg.invalid("Focal element must be a pro-form.")
    }
  }

  // type Get = Get.type
  // case object Get extends Processor[Argument] {
  //   def process(arg: Argument): Component.RenderResultOf[
  //     (LabeledTree[String, String], Argument)
  //   ] = ???
  // }

  object Processor {
    implicit def processorShow[A]: Show[Processor[A]] = new Show[Processor[A]] {
      def show(p: Processor[A]) = p match {
        case Extract(innerPath) => s"{${argumentPathShow.show(innerPath)}}"
        case Focus => "*"
      }
    }
  }

  // case class ExtractionDescent(
  //   position: ArgPosition, next: ExtractionPath
  // ) extends ExtractionPath with Descent //[ExtractionPath]
  // case class Extraction(next: FocalPath) extends ExtractionPath

  // case class FocalDescent(
  //   position: ArgPosition, next: FocalPath
  // ) extends FocalPath with Descent //[FocalPath]
  // case object Focus extends FocalPath

  def descend[A](path: Option[Descent[A]], pos: ArgPosition): Option[ArgumentPath[A]] =
    path.collect { case Descent(`pos`, next) => next }

  implicit def argumentPathShow[A]: Show[ArgumentPath[A]] = new Show[ArgumentPath[A]] {
    def show(path: ArgumentPath[A]): String = path match {
      case Descent(position, next) => s"${position} -> ${show(next)}"
      case End(a) => Processor.processorShow.show(a)
    }
  }

  // NOTE: shouldn't this be unnecessary bc of contravariance?
  implicit def descentShow[A: Show] = argumentPathShow[A]
    .contramap[Descent[A]](x => x: ArgumentPath[A])
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
  def error(msg: String) = Component.RenderError(this, msg)
  def invalid[A](
    msg: String = "Incomplete component."
  ): ValidatedNec[Component.RenderError, A] =
    Validated.invalid(NonEmptyChain.one(error(msg)))
}
object Component {
  type Branches = LabeledTree.Node[String, String]
  type Tree = LabeledTree[String, String]
  // TODO improve API so an Option is provided
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
  // type RenderResult = ValidatedNec[RenderError, LabeledTree.Node[String, String]]
  type RenderTree[A] = RenderResultOf[WithExtraction[Tree, A]]
  type RenderBranches[A] = RenderResultOf[WithExtraction[Branches, A]]
}

// argument structure (with slots etc.)
sealed trait Argument extends Component {
  import Component._
  import LabeledTree.{leaf,leaves,node}

  def symbol: String

  def renderLax[A](pos: ArgPosition, path: Option[ArgumentPath.Descent[A]]): RenderTree[A]

  import ArgumentPath._
  def renderLaxWithPath[A](pos: ArgPosition, path: Option[ArgumentPath[A]]): RenderBranches[A] = {
    path match {
      case Some(End(pathEnd)) =>
        pathEnd.process(this, pos).andThen { case (tree, res) =>
          Validated.valid(WithExtraction(tree, Vector(res)))
        }
      case Some(d: Descent[A]) =>
        renderLax(pos, Some(d))
          .map(_.map(tree => node(symbol -> tree)))
      case None =>
        renderLax(pos, None: Option[ArgumentPath.Descent[A]])
          .map(_.map(tree => node(symbol -> tree)))
    }
  }

  def render[A](
    pos: ArgPosition,
    path: Option[ArgumentPath[A]]
  ): RenderResultOf[(Branches, Option[A])] =
    renderLaxWithPath(pos, path).map { case WithExtraction(tree, items) =>
      require(
        (path.isEmpty && items.isEmpty) ||
          (path.nonEmpty && items.size == 1)
      ) // require correct number of extractions
      tree -> items.headOption
    }

  def render[A](pos: ArgPosition, path: ArgumentPath[A]): RenderResultOf[(Branches, A)] =
    render(pos, Some(path)).map { case (x, y) => x -> y.get } // require 1 extraction

  def render(pos: ArgPosition): RenderResultOf[Branches] = {
    // ascribe a type to the None to help out type inference
    render(pos, None: Option[ArgumentPath[Argument.ProForm]]).map { case (tree, itemOpt) =>
      require(itemOpt.isEmpty) // require no extractions
      tree
    }
  }

  def argumentPaths: Set[ArgumentPath[Argument.ProForm]]
  def extractionPaths: Set[ArgumentPath[ArgumentPath.Extraction]]
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

    import ArgPosition._, ArgumentPath._
    def renderLax[A](
      pos: ArgPosition,
      path: Option[ArgumentPath.Descent[A]]
    ): RenderTree[A] = path match {
      case Some(_) => invalid("Cannot extract from inside a pro-form.")
      case None => placeholder
          .map(p => valid(WithExtraction(leaf(p))))
          .getOrElse(invalid("No placeholder exists for pro-form."))
    }

    // def argumentPaths: Set[ArgumentPath] = Set(Extraction(Focus), Focus)
    def argumentPaths: Set[ArgumentPath[Argument.ProForm]] = Set(End(Focus))
    def extractionPaths: Set[ArgumentPath[ArgumentPath.Extraction]] = Set(End(Extract(End(Focus))))

    def instances: Set[Semantic]
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
      val instances = Set(NounPhrase(None, Some(true)))
    }
    case object what extends Noun with Complement {
      def wh(pos: ArgPosition) = "what"
      val placeholder = Some("something")
      val number = Some(Number.Singular)
      val person = Some(Person.Third)
      val instances = Set(
        NounPhrase(None, Some(false)),
        Gerund(None, false),
        Gerund(None, true),
        ToInfinitive(None, false, Set(what)),
        ToInfinitive(None, true, Set(what)),
        Attributive(None, Set(what)),
        Finite(None),
        FiniteComplement(None)
      )
    }
    sealed trait Adverb extends ProForm with NonNominal with NounOrOblique {
      def wh(pos: ArgPosition) = whWord.form.toString
      def whWord: Lexicon.Wh.Adverb
      val instances = Set(
        Oblique(None, Set(this)),
        Subordinate(None, Set(this)),
        Adverbial(None, Set(this)),
        ToInfinitive(None, false, Set(this)),
        ToInfinitive(None, true, Set(this)),
        Progressive(None, false, Set(this)),
        Progressive(None, true, Set(this)),
        Attributive(None, Set(this))
      )
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
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) = path match {
      case Some(_) => invalid("Cannot focus or descend into an expletive.")
      case None => pos match {
        case Subj => valid(WithExtraction(leaf(expletive.form.toString)))
        case Arg(_) => invalid("Can only render expletives in subject position.")
      }
    }

    def argumentPaths: Set[ArgumentPath[Argument.ProForm]] = Set()
    def extractionPaths: Set[ArgumentPath[ArgumentPath.Extraction]] = Set()
  }
  object Expletive {
    val it = Expletive(Lexicon.Expletive.it)
    val there = Expletive(Lexicon.Expletive.there)
  }

  sealed trait Semantic extends Concrete {
    def pred: Option[Predication]

    // determine which nested args we pied-pipe with
    def allowPiedPiping(path: ArgumentPath[ProForm]): Boolean = false
    // enforce island constraints
    def blockExtraction(path: ArgumentPath[ArgumentPath.Extraction]): Boolean = false

    import ArgumentPath._
    def argumentPaths: Set[ArgumentPath[Argument.ProForm]] = {
      // widening to deal with invariance of Set
      pred.unorderedFoldMap(_.argumentPaths.map(p => p: ArgumentPath[Argument.ProForm]))
    }
    def extractionPaths: Set[ArgumentPath[ArgumentPath.Extraction]] = {
      argumentPaths.collect {
        case path if allowPiedPiping(path) => End(Extract(path)): ArgumentPath[ArgumentPath.Extraction]
      } ++ pred.unorderedFoldMap(_.extractionPaths.filterNot(blockExtraction))
    }
  }

  // NOTE: predications are optional because we can use arguments
  // to specify general syntactic subcategorization frames.

  case class NounPhrase(
    pred: Option[Predication.Nominal],
    animacyConstraint: Option[Boolean]
  ) extends Semantic with Nominal {

    override def allowPiedPiping(path: ArgumentPath[ProForm]): Boolean = true

    def category = "np"
    // TODO: change to account for multiple possible animacies
    def proForms =
      animate.map(anim => if(anim) ProForm.who else ProForm.what)
        .toSet

    def animate: Option[Boolean] = pred.flatMap(_.animate).orElse(animacyConstraint)
    def person: Option[Person] = pred.flatMap(_.person)
    def number: Option[Number] = pred.flatMap(_.number)

    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) = {
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
  ) extends Semantic with NounOrOblique with NonNominal {

    override def allowPiedPiping(path: ArgumentPath[ProForm]): Boolean = true

    def category = "pp"
    def proForms = adverbials.map(x => x: ProForm)

      // TODO: allowed in Fronted position. should we put this back in? idk
      // case Subj => invalid("Oblique argument cannot appear in subject position.")
      // case Arg(_) =>
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(_.render((), path)).getOrElse(invalid())
  }

  // TODO: island constraints
  case class Subordinate(
    pred: Option[Predication.Subordinate],
    adverbials: Set[ProForm.Adverb]
  ) extends Semantic with NonNominal {
    override def category = "adv[comp]"
    override def proForms = adverbials.map(x => x: ProForm)
    // TODO might depend on the predicate?
    override def allowPiedPiping(path: ArgumentPath[ProForm]) = true
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(_.render((), path)).getOrElse(invalid())
  }

  // for adverbials like 'today', 'every day' etc.
  // as well as adverbs (quickly, eventually)
  case class Adverbial(
    pred: Option[Predication.Adverbial],
    adverbials: Set[ProForm.Adverb]
  ) extends Semantic with NonNominal {
    override def category = "adv"
    override def proForms = adverbials.map(x => x: ProForm)
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(_.render((), path)).getOrElse(invalid())
  }

  sealed trait Clausal extends Semantic {
    def pred: Option[Predication.Clausal]
    def clauseType: ClauseType
    def includeSubject: Boolean
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(
        _.render(Predication.ClauseFeats(clauseType, includeSubject), path)
      ).getOrElse(invalid())

    // filter out subject extractions in case our subject is not included
    override def argumentPaths: Set[ArgumentPath[Argument.ProForm]] =
      super.argumentPaths.filter(x =>
        includeSubject || x.descent.forall(_.position != ArgPosition.Subj)
      )//.map(x => x: ArgumentPath[Argument.ProForm])
    override def extractionPaths: Set[ArgumentPath[ArgumentPath.Extraction]] =
      super.extractionPaths.filter(x =>
        includeSubject || x.descent.forall(_.position != ArgPosition.Subj)
      )//.map(x => x: ArgumentPath[ArgumentPath.Extraction])
  }

  // nominal use of gerund/progressive; can be object of prep or subject
  // TODO possessive subject option
  case class Gerund(
    pred: Option[Predication.Clausal],
    includeSubject: Boolean
  ) extends Clausal with Nominal {
    override def clauseType = ClauseType.Progressive
    override def category = if(includeSubject) "s[g]" else "vp[g]"
    override def proForms = Set(ProForm.what)
    // TODO should allow with 'whose' on subject?
    override def allowPiedPiping(path: ArgumentPath[ProForm]) = false
    override def person = Some(Person.Third)
    override def number = Some(Number.Singular)
  }

  // includeSubject: Boolean,
  case class BareInfinitive(
    pred: Option[Predication.NonCopular]
  ) extends Clausal with Complement {
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
    // TODO: with Subject. or, just add a new argument type for subj inf?
    def complementizer: InfinitiveComplementizer = Lexicon.InfinitiveComplementizer.`for`

    override def clauseType = ClauseType.ToInfinitive
    override def category = if(includeSubject) "s[for-to]" else "vp[to]"

    // override def person = Some(Person.Third)
    // override def number = Some(Number.Singular)

    // TODO clausal override not that useful here. maybe separate out versions
    // with and without subject? idk
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(p =>
        List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
          if(includeSubject) valid(WithExtraction(leaves("comp" -> complementizer.form.toString)))
          else valid(WithExtraction(LabeledTree.Node[String, String](Vector()))),
          p.render(Predication.ClauseFeats(ClauseType.ToInfinitive, includeSubject), path)
        ).foldA
      ).getOrElse(invalid())
  }

  case class Progressive(
    pred: Option[Predication.Clausal],
    includeSubject: Boolean,
    adverbials: Set[ProForm.Adverb]
  ) extends Clausal with Complement {
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
    override def clauseType = ClauseType.Attributive
    override def includeSubject = false
    // def category = if(includeSubject) "s[pt]" else "vp[pt]"
    override def category = "vp[adj/pt]"
  }

  case class Finite(
    pred: Option[Predication.Clausal]
  ) extends Clausal with Complement {
    // with Subject? but cannot appear in subj except as an answer
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
    override def clauseType = ClauseType.Finite
    override def includeSubject = true

    override def category = "s[comp]"
    override def proForms = Set(ProForm.what)

    def person = Some(Person.Third)
    def number = Some(Number.Singular)

    def complementizer: Complementizer = Lexicon.Complementizer.that

    // not sure if Clausal override is useful here
    import ArgPosition._, ArgumentPath.Descent
    override def renderLax[A](pos: ArgPosition, path: Option[Descent[A]]) =
      pred.map(p =>
        List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
          valid(WithExtraction(leaves("comp" -> complementizer.form.toString))),
          p.render(Predication.ClauseFeats(clauseType, includeSubject), path)
        ).foldA
      ).getOrElse(invalid())
  }
}

sealed trait Predication extends Component {
  type GramFeats // grammatical features

  import Component._

  def render[A](
    features: GramFeats,
    path: Option[ArgumentPath.Descent[A]]
  ): RenderTree[A]

  def arguments: Vector[Argument]
  def argumentPaths: Set[ArgumentPath.Descent[Argument.ProForm]] =
    arguments.zipWithIndex.flatMap { case (arg, index) =>
      arg.argumentPaths.map(_.ascend(ArgPosition.Arg(index)))
    }.toSet
  def extractionPaths: Set[ArgumentPath.Descent[ArgumentPath.Extraction]] =
    arguments.zipWithIndex.flatMap { case (arg, index) =>
      arg.extractionPaths.map(_.ascend(ArgPosition.Arg(index)))
    }.toSet
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
  sealed trait Nominal extends Predication {
    type GramFeats = Case

    def animate: Option[Boolean]
    def person: Option[Person]
    def number: Option[Number]
    def arguments = Vector()

    def getForm(c: Case): String

    // override def render(
    //   `case`: Case,
    //   path: Option[ArgumentPath.Descent]
    // ): Component.RenderResultOf[WithExtraction[LabeledTree[String, String]]] = path match {
    //   case Some(_) => invalid("Cannot descend into a nominal predication for extraction (for now).")
    //   case None => valid(WithExtraction(leaf(getForm(`case`))))
    // }

    def render[A](
      features: Case,
      path: Option[ArgumentPath.Descent[A]]
    ): RenderTree[A] = path match {
      case Some(_) => invalid("Cannot descend into a nominal predication for extraction (for now).")
      case None => valid(WithExtraction(leaf(getForm(features))))
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

  sealed trait Oblique extends Predication {
    type GramFeats = Unit
    import ArgPosition._, ArgumentPath.Descent
  }
  // prep should not take an object, i.e., is a particle
  case class Particulate(form: Particle) extends Oblique {
    def arguments = Vector()

    import ArgPosition._, ArgumentPath.Descent

    override def render[A](
      feats: Unit,
      path: Option[Descent[A]]
    ): RenderTree[A] = path match {
      case Some(_) => invalid("Cannot extract from inside a particle.")
      case None => valid(WithExtraction(leaves("prt" -> form.toString)))
    }
  }

  case class Prepositional(
    prep: Preposition,
    obj: Argument.Nominal
  ) extends Oblique {
    def arguments = Vector(obj)
    import ArgPosition._, ArgumentPath.Descent
    override def render[A](
      feats: Unit,
      path: Option[Descent[A]]
    ): RenderTree[A] = {
      if(path.exists(_.position != Arg(0))) {
        invalid(s"Can't descend into argument for extraction from oblique: ${path.get}")
      } else List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
        valid(WithExtraction(leaves("prep" -> prep.toString))),
        obj.renderLaxWithPath(Arg(0), ArgumentPath.descend(path, Arg(0)))
      ).foldA
    }
  }

  case class Adverbial(form: String) extends Predication {
    type GramFeats = Unit
    def arguments = Vector()

    import ArgumentPath.Descent
    override def render[A](
      feats: Unit,
      path: Option[Descent[A]]
    ): RenderTree[A] = path match {
      case Some(_) => invalid("Cannot descend into an adverbial for extraction (for now).")
      case None => valid(WithExtraction(leaf(form)))
    }
  }

  case class Subordinate(
    subordinator: Subordinator,
    clause: Argument.Complement
  ) extends Predication {
    type GramFeats = Unit
    def arguments = Vector(clause)
    import ArgumentPath.Descent, ArgPosition._
    override def render[A](
      feats: Unit,
      path: Option[Descent[A]]
    ): RenderTree[A] = {
      path match {
        case Some(pos) if pos != Arg(0) =>
          invalid(s"Can't descend into argument for extraction from oblique: $pos")
        case otherPath => List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
          valid(WithExtraction(leaves("sub" -> subordinator.form.toString))),
          clause.renderLaxWithPath(Arg(0), ArgumentPath.descend(path, Arg(0)))
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

  case class ClauseFeats(
    clauseType: ClauseType,
    includeSubject: Boolean
  )

  sealed trait Clausal extends Predication {
    type GramFeats = ClauseFeats
    def subject: Argument.Subject
    def tan: TAN
    def arguments: Vector[Argument]

    def predPOS: String

    def renderVerbChain(
      clauseType: ClauseType,
      needsFlippable: Boolean
    ): Either[NonEmptyChain[String], NonEmptyList[String]]

    import Validated.valid
    import LabeledTree.{leaf, leaves, node}
    import Component.WithExtraction

    // def renderQuestion(
    //   path: Option[ArgumentPath.Descent]
    // ): Component.RenderResultOf[LabeledTree[String, String]] = {
    //   val clauseType =
    //     if(path.forall(_.isExtraction)) ClauseType.Inverted
    //     else ClauseType.Finite
    //   render(clauseType, includeSubject = true, path = path).andThen {
    //     case WithExtraction(tree, extractions) =>
    //       extractions.headOption match {
    //         case None => valid(tree)
    //         case Some((arg, path)) =>
    //           arg.render(ArgPosition.Subj, Some(path)).andThen {
    //             case WithExtraction(argTree, argExtractions) =>
    //               if(argExtractions.nonEmpty) invalid("Should not have a nested extraction.")
    //               else valid(argTree |+| tree)
    //           }
    //       }
    //   }
    // }

    // TODO move out to supertype
    def render(
      clauseType: ClauseType,
      includeSubject: Boolean,
    ): Component.RenderResultOf[Branches] = {
      render(ClauseFeats(clauseType, includeSubject), None)
        .map((x: WithExtraction[Branches, Nothing]) => x.value)
    }

    override def render[A](
      feats: ClauseFeats,
      path: Option[ArgumentPath.Descent[A]]
    ): Component.RenderBranches[A] = {
      import feats.{clauseType, includeSubject}
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
          Validated.fromEither(
            renderVerbChain(clauseType, flipAuxiliary).left.map(_.map(error(_)))
          ).andThen { verbChain =>
            if(flipAuxiliary) { // aux flip
              val aux = valid(WithExtraction(leaves("aux" -> verbChain.head)))
              NonEmptyList.fromList(verbChain.tail) match {
                case None =>
                  List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
                    aux, subj, args
                  ).foldA
                case Some(verbTail) =>
                  val verb = valid(WithExtraction(makeVerbTree(verbTail)))
                  List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
                    aux, subj, verb, args
                  ).foldA
              }
            } else {
              val verb = valid(WithExtraction(makeVerbTree(verbChain)))
              if(includeSubject) List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](
                subj, verb, args
              ).foldA
              else List[ValidatedNec[RenderError, WithExtraction[Branches, A]]](verb, args).foldA
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

    def validatePath[A](
      includeSubject: Boolean,
      path: Option[ArgumentPath.Descent[A]]
    ): ValidatedNec[Component.RenderError, Unit] = path.map(_.position) match {
      case None => valid(())
      case Some(ArgPosition.Subj) if includeSubject => valid(())
      case Some(ArgPosition.Arg(i)) if arguments.size > i => valid(())
      case Some(pos) => invalid(s"Cannot descent into argument position for extraction: $pos")
    }

    def renderSubject[A](
      includeSubject: Boolean, path: Option[ArgumentPath.Descent[A]]
    ): Component.RenderBranches[A] = {
      if(!includeSubject) {
        Validated.valid(Monoid[Component.WithExtraction[Branches, A]].empty)
      } else subject.renderLaxWithPath(ArgPosition.Subj, ArgumentPath.descend(path, ArgPosition.Subj))
    }

    def renderArguments[A](path: Option[ArgumentPath.Descent[A]]): Component.RenderBranches[A] = {
      arguments.zipWithIndex.foldMapA[
        ValidatedNec[RenderError, *], WithExtraction[Branches, A]
      ] { case (arg, index) =>
        arg.renderLaxWithPath(
          ArgPosition.Arg(index), ArgumentPath.descend(path, ArgPosition.Arg(index))
        )
      }
    }

    override def argumentPaths: Set[ArgumentPath.Descent[Argument.ProForm]] = {
      import ArgPosition._
      subject.argumentPaths.map(_.ascend(Subj)) ++ super.argumentPaths
      // arguments.zipWithIndex.flatMap { case (arg, index) =>
      //   arg.argumentPaths.map(_.ascend(Arg(index)))
      // }
    }
  }

  case class Copular(
    subject: Argument.Subject,
    argument: NounOrOblique,
    modifiers: Vector[NonNominal],
    tan: TAN
  ) extends Clausal {
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
  }

  sealed trait NonCopular extends Clausal

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

  // ---------------------------------------------------------------

  // argument: 'he helped (me/_) do the laundry / finish the homework.'
  // argument: 'he helped (me/*_) be designated as leader.'
  // argument: 'he helped (me/*_) be the man i wanted to be.'
  // verbs: help, make (make him do his hw), go (go swim).
  // not included in NonNominalPredication bc can't appear as an adjective argument (?)

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
  //   verb: VoicedVerbPhrase
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
  //   -- aspectual possibilities
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
