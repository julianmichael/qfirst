package qfirst.cafe

import qfirst.parsing._

import qasrl.bank.ConsolidatedSentence

import shapeless._

import jjm.ling.Text
import jjm.ling.ESpan
import jjm.implicits._

import cats.data.NonEmptyList
import cats.implicits._
import jjm.ling.en.InflectedForms
import qasrl.Tense
import jjm.ling.en.VerbForm

class Parsing(sentence: ConsolidatedSentence) {

  case class Aspect(
    isPerfect: Boolean,
    isProgressive: Boolean,
    isNegated: Boolean
  )
  object Aspect {
    def default = Aspect(false, false, false)
  }

  sealed trait VPForm {
    def resolveAspect(aspect: Aspect): Option[Aspect] = Some(aspect)
  }
  object VPForm {

    case object Predicative extends VPForm
    case object BareInfinitive extends VPForm
    case object ToInfinitive extends VPForm
    case object Perfect extends VPForm {
      override def resolveAspect(aspect: Aspect): Option[Aspect] =
        if(aspect.isPerfect) None else Some(aspect.copy(isPerfect = true))
    }
    case object Progressive extends VPForm {
      override def resolveAspect(aspect: Aspect): Option[Aspect] =
        if(aspect.isProgressive) None else Some(aspect.copy(isProgressive = true))
    }
    case class Finite(
      tense: Tense.Finite,
      subjectNumber: Option[Number],
      subjectPerson: Option[Person]
    ) extends VPForm
  }

  def vpFormMapping(forms: InflectedForms): Map[String, Vector[VPForm]] = {
    import VPForm._
    List[(String, Vector[VPForm])](
      forms.stem.toString -> Vector(
        BareInfinitive,
        Finite(Tense.Finite.Present, Some(Number.Singular), Some(Person.First)),
        Finite(Tense.Finite.Present, Some(Number.Plural), None)
      ),
      forms.presentSingular3rd.toString -> Vector(
        Finite(Tense.Finite.Present, Some(Number.Singular), Some(Person.Third))
      ),
      forms.past.toString -> Vector(
        Finite(Tense.Finite.Past, None, None)
      ),
      forms.presentParticiple.toString -> Vector(Progressive),
      forms.pastParticiple.toString -> Vector(Perfect, Predicative)
    ).foldMap { case (k, v) => Map(k -> v) }
  }

  case class Copula(source: Option[Int], form: VPForm)
  object Copula {
    // TODO negations and adverbs of certainty/probability?
    import VPForm._
    // Note: 'you' is treated as grammatically plural
    // these can be used both as auxiliaries and copulas
    val forms = Map[String, VPForm](
      "be" -> BareInfinitive,
      "been" -> Perfect,
      "being" -> Progressive,
      "is" -> Finite(Tense.Finite.Present, Some(Number.Singular), Some(Person.Third)),
      "am" -> Finite(Tense.Finite.Present, Some(Number.Singular), Some(Person.First)),
      "are" -> Finite(Tense.Finite.Present, Some(Number.Plural), None),
      "was" -> Finite(Tense.Finite.Past, Some(Number.Singular), None),
      "were" -> Finite(Tense.Finite.Past, Some(Number.Plural), None)
    )
  }

  sealed trait Auxiliary {
    def modify(vp: VPForm): Option[VPForm]
    def modifyVP(vp: VerbPhrase): Option[VerbPhrase] = vp match {
      case VerbPhrase(source, form, aspect, pred) =>
        (modify(form), form.resolveAspect(aspect))
          .mapN { (newForm, newAspect) =>
            VerbPhrase(source, newForm, newAspect, pred)
          }
    }
  }
  object Auxiliary {
    def apply(
      f: PartialFunction[VPForm, VPForm]
    ): Auxiliary = new Auxiliary {
      override def modify(form: VPForm): Option[VPForm] = f.lift(form)
    }

    // TODO negations and adverbs of certainty/probability?

    import VPForm._
    val beAux = Copula.forms
      .mapVals(form => Vector(Auxiliary { case Predicative | Progressive => form }))

    val haveAux = vpFormMapping(InflectedForms.haveForms)
      .mapVals(_.filter(_ != Predicative))
      .mapVals(_.map(form => Auxiliary { case Perfect => form }))

    val doAux = vpFormMapping(InflectedForms.doForms)
      .mapVals(_.filter(_ != Predicative))
      .mapVals(_.map(form => Auxiliary { case BareInfinitive => form }))

    val all: Map[String, Vector[Auxiliary]] =
      List(beAux, haveAux, doAux).reduce(_ ++ _)
  }

  case class Preposition(source: Option[Int], form: String)
  object Preposition {
    val preps = Lexicon.Preposition.preps.map(_.toString)
  }

  // TODO
  // then conversion from VP to clause by taking subject
  // and don't forget conversion from VP to Arg
  // negation and adverbs of certainty
  // and eventually adjectives

  case class VerbPhrase(
    source: Option[Int],
    form: VPForm,
    aspect: Aspect,
    verbalPred: VerbalPred
  ) {
    // def toArgument: Argument.Clausal
  }

  sealed trait VerbalPred

  case class VerbPred(
    verb: InflectedForms,
    isPassive: Boolean,
    arguments: Vector[Argument.NonSubject],
  ) extends VerbalPred

  case class AdjPred(
    adjective: String,
    arguments: Vector[Argument.NonNominal]
  ) extends VerbalPred

  case class CopulaPred(
    argument: Argument.NounOrOblique,
    modifiers: Vector[Argument.NonNominal]
  ) extends VerbalPred

  // sealed trait SForm
  // object SForm {
  //   // NOTE: could include small clauses (bare inf) as well as inverted ones
  //   // e.g., "be he a rascal, ..."
  //   // case class BareInfinitive(aspect: Aspect) extends VPForm
  //   case class ForToInfinitive(aspect: Aspect) extends SForm
  //   case class Progressive(genitiveSubject: Boolean, aspect: Aspect) extends SForm
  //   case class Finite(
  //     tense: qasrl.Tense.Finite,
  //     aspect: Aspect,
  //   ) extends SForm
  //   case class FiniteComplement(
  //     complementizer: Lexicon.Complementizer,
  //     tense: Tense.Finite,
  //     aspect: Aspect,
  //   ) extends SForm
  //   case class Inverted(
  //     tense: Tense.Finite,
  //     aspect: Aspect
  //   )
  //   case class FiniteQuestion(
  //     tense: Tense.Finite,
  //     aspect: Aspect,
  //     path: Option[ArgumentPath[Extraction]]
  //   )
  // }



  import SyncCFGProductionSyntax._

  case class Node[+A](
    value: A,
    path: Option[Either[ArgumentPath[ArgSnapshot], ArgumentPath[ProFormExtraction]]]
  ) {
    def gapPath = path.flatMap(_.left.toOption)
    def focusPath = path.flatMap(_.toOption)
    def map[B](f: A => B): Node[B] = Node(f(value), path)
    def flatMapOpt[B](f: A => Node[B]): Option[Node[B]] = {
      (f(value), path) match {
        case (Node(b, Some(_)), Some(_)) => None
        case (Node(b, p2), p1) => Some(Node(b, p1.orElse(p2)))
      }
    }
    def zipOpt[B](that: Node[B]): Option[Node[(A, B)]] =
      flatMapOpt(a => that.map(a -> _))
  }

  val Aux = new ParseSymbol[Auxiliary]("Aux")
  val Cop = new ParseSymbol[Copula]("Cop")
  val Prep = new ParseSymbol[Preposition]("Prep")

  val VBar = new ParseSymbol[Node[VerbPhrase]]("V'")
  val VP = new ParseSymbol[Node[VerbPhrase]]("VP")

  val AdvP = new ParseSymbol[Argument.Adverbial]("AdvP")
  val NP = new ParseSymbol[Node[Argument.Nominal]]("NP")
  val PP = new ParseSymbol[Node[Argument.Prepositional]]("PP")

  // val VP = new ParseSymbol[Node[Predication.Verbal]]("VP")
  // val Complement = new ParseSymbol[Node[Argument.Complement]]("Complement")

  val Arg = new ParseSymbol[Node[Argument]]("Arg")

  def inferNPFromSpan(span: ESpan) = Argument.NounPhrase(
    Some(
      Predication.NounPhrase(
        Some(span), Text.renderSpan(sentence.sentenceTokens, span),
        None, None, None
      )
    )
  )
  def inferAdvPFromSpan(span: ESpan) = Argument.Adverbial(
    Some(
      Predication.Adverbial(
        Some(span), Text.renderSpan(sentence.sentenceTokens, span)
      )
    ),
    Set()
  )
  def makePrtP(prep: Preposition) = {
    Node(
      Argument.Prepositional(
        Some(
          Predication.Prepositional(
            prep.source, Lexicon.Preposition(prep.form.lowerCase), None // TODO prep form
          )
        )
      ),
      None
    )
  }
  def makePP(prep: Preposition, arg: Node[Argument.Complement]) = {
    arg.map { compl =>
      Argument.Prepositional(
        Some(
          Predication.Prepositional(
            prep.source, Lexicon.Preposition(prep.form.lowerCase), Some(compl) // TODO prep form
          )
        )
      )
    }
  }

  val whForms = {
    import Argument.ProForm._
    Map(
      "what"  -> what,
      "who"   -> who,
      "where" -> where,
      "when"  -> when,
      "how"   -> how,
      "why"   -> why
    )
  }

  val indefinitePronouns = {
    import Argument.ProForm._
    Map(
      "something" -> what,
      "someone"   -> who,
      "somewhere" -> where
    )
  }

  val genlex = (s: SurfaceForm) => s match {
    case SurfaceForm.Excerpt(span, text) =>
      ScoredStream.unit(Derivation(NP, Node(inferNPFromSpan(span), None))).merge(
        ScoredStream.unit(Derivation(AdvP, inferAdvPFromSpan(span)))
      )
    case SurfaceForm.Token(sourceOpt, text) =>
      import ArgumentPath.{End, Focus}
      val txt = text.toString
      List(
        whForms.get(txt).map(pro =>
          Derivation(Arg, Node(pro, Some(Right(End(Focus)))))
        ).foldMap(ScoredStream.unit(_)),
        indefinitePronouns.get(txt).map(pro =>
          Derivation(Arg, Node(pro, None))
        ).foldMap(ScoredStream.unit(_)),
        Auxiliary.all.get(txt).foldMap(auxes =>
          auxes.foldMap(aux =>
            ScoredStream.unit(Derivation(Aux, aux))
          )
        ),
        Copula.forms.get(txt).map(form =>
          Derivation(Cop, Copula(sourceOpt, form))
        ).foldMap(ScoredStream.unit(_)),
        sourceOpt.foldMap(index =>
          sentence.verbEntries.get(index).foldMap(verb =>
            vpFormMapping(verb.verbInflectedForms).get(txt).combineAll.foldMap(form =>
              ScoredStream.unit(
                Derivation(
                  VBar,
                  Node(
                    VerbPhrase(
                      Some(index), form, Aspect.default,
                      VerbPred(
                        verb.verbInflectedForms,
                        isPassive = form == VPForm.Predicative,
                        Vector()
                      )
                    ),
                    None
                  )
                )
              )
            )
          )
        ),
        Option(text).filter(Preposition.preps.contains).foldMap(prep =>
          ScoredStream.unit(Derivation(Prep, Preposition(sourceOpt, text)))
        )
          // TODO: handle adjectives somehow. later.
      ).combineAll
        // case x if InflectedForms.auxi => Derivation(Arg, Node(Argument.ProForm.where, None))
    // case SurfaceForm.Participial(sourceOpt, form) =>
    //   ScoredStream.empty[Derivation]
    // case prep @ SurfaceForm.Preposition(sourceOpt, form) =>
    //   ScoredStream.unit(Derivation(preposition, prep))
    // case cop @ SurfaceForm.Copula(_) =>
    //   ScoredStream.unit(Derivation(copula, cop))
    // case SurfaceForm.Verb(source, forms, feats) =>
    //   ScoredStream.empty[Derivation]
  }

  val productions = {
    val prtp = Prep to PP usingSingleZ {
      case prep => makePrtP(prep)
    }
    val pp = (Prep, Arg) to PP usingSingleZ {
      case (prep, Node(arg: Argument.Complement, path)) =>
        makePP(prep, Node(arg, path))
    }
    val copulaVBar = (Cop, Arg) to VBar usingSingleZ {
      case (Copula(source, form), Node(arg: Argument.NounOrOblique, path)) =>
        Node(
          VerbPhrase(source, form, Aspect.default, CopulaPred(arg, Vector())),
          path
        )
    }
    val vBarArg = (VBar, Arg) to VBar using {
      case (vBarNode, argNode) => vBarNode.zipOpt(argNode).flatMap {
        case Node((vp @ VerbPhrase(_, _, _, CopulaPred(arg, mods)), newMod: Argument.NonNominal), path) =>
          Some(Node(vp.copy(verbalPred = CopulaPred(arg, mods :+ newMod)), path))
        case Node((vp @ VerbPhrase(_, _, _, AdjPred(adj, args)), newArg: Argument.NonNominal), path) =>
          Some(Node(vp.copy(verbalPred = AdjPred(adj, args :+ newArg)), path))
        case Node((vp @ VerbPhrase(_, _, _, VerbPred(verb, isPassive, args)), newArg: Argument.NonSubject), path) =>
          Some(Node(vp.copy(verbalPred = VerbPred(verb, isPassive, args :+ newArg)), path))
        case _ => None
      }.foldMap(ScoredStream.unit(_))
    }
    val vp = VBar to VP usingSingleZ { case vbar => vbar }
    val auxVP = (Aux, VP) to VP using {
      case (aux, node) => aux.modifyVP(node.value)
          .foldMap(vp => ScoredStream.unit(node.copy(value = vp)))
    }

    val ppArg = PP to Arg usingSingleZ { case pp => pp }
    val npArg = NP to Arg usingSingleZ { case np => np }
    val advpArg = AdvP to Arg usingSingleZ { case advp => Node(advp, None) }
    // pp :: prtp :: HNil
    auxVP :: HNil
  }
  val grammar = SyncCFG(productions)
  val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genlex, grammar)
  def parseTest(forms: Vector[SurfaceForm]) = parser.parse(forms, Arg)
}
