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
      case VerbPhrase(pred, form, aspect) =>
        (modify(form), form.resolveAspect(aspect))
          .mapN { (newForm, newAspect) =>
            VerbPhrase(pred, newForm, newAspect)
          }
    }
  }
  object Auxiliary {
    def apply(
      f: PartialFunction[VPForm, VPForm]
    ): Auxiliary = new Auxiliary {
      override def modify(form: VPForm): Option[VPForm] = f.lift(form)
    }

    import VPForm._
    val beAux = Copula.forms
      .mapVals(form => Vector(apply { case Predicative | Progressive => form }))

    val haveAux = vpFormMapping(InflectedForms.haveForms)
      .mapVals(_.filter(_ != Predicative))
      .mapVals(_.map(form => apply { case Perfect => form }))

    val doAux = vpFormMapping(InflectedForms.doForms)
      .mapVals(_.filter(_ != Predicative))
      .mapVals(_.map(form => apply { case BareInfinitive => form }))

    val toAux = Map(
      "to" -> Vector(apply { case BareInfinitive => ToInfinitive })
    )

    val modals = Set(
      "can", // can't
      "will", // won't
      "might", // mightn't...?
      "would", // wouldn't
      "should", // shouldn't
    )
    val modalAux = modals.toList.map(mod =>
      mod -> Vector(
        apply {
          case BareInfinitive => Finite(
            qasrl.Tense.Finite.Modal(mod.lowerCase), None, None
          )
        }
      )
    ).toMap

    val all: Map[String, Vector[Auxiliary]] =
      List(beAux, haveAux, doAux, toAux, modalAux).reduce(_ ++ _)
  }

  case class Preposition(source: Option[Int], form: String)
  object Preposition {
    val preps = Lexicon.Preposition.preps.map(_.toString)
  }

  // TODO
  // negation and adverbs of certainty
  // and eventually adjectives --- maybe represent with spans too (for now)
  // TODO negations and adverbs of certainty/probability?
  // and figure out situation with pronouns

  case class VerbPhrase(
    pred: Predication.VerbLike,
    form: VPForm,
    aspect: Aspect
  ) {
    import VPForm._
    def toArgument: Vector[Argument.Semantic] = form match {
      case Predicative => pred match {
        case pred: Predication.NonCopularVerbLike =>
          Vector(Argument.Predicative(Some(pred)))
        case _ => Vector()
      }
      case Progressive => Vector(
        Argument.Verbal.Gerund(Some(pred), aspect),
        Argument.Verbal.Progressive(Some(pred), aspect)
      )
      case BareInfinitive => Vector(
        Argument.Verbal.BareInfinitive(Some(pred), aspect)
      )
      case ToInfinitive => Vector(
        Argument.Verbal.ToInfinitive(Some(pred), aspect)
      )
      case _ => Vector()
    }
  }

  case class Clause(
    pred: Predication.Clausal,
    form: SForm,
    aspect: Aspect
  ) {
    import SForm._
    def toArgument: Vector[Argument.Clausal] = form match {
      case Progressive => Vector(
        Argument.Clausal.Gerund(Some(pred), aspect),
        Argument.Clausal.Progressive(Some(pred), aspect)
      )
      // case BareInfinitive => Vector(
      //   Argument.Clausal.BareInfinitive(Some(pred), aspect)
      // )
      case ForToInfinitive => Vector(
        Argument.Clausal.ForToInfinitive(Some(pred), aspect)
      )
      case f @ Finite(_) => Vector(
        Argument.Clausal.Finite(Some(pred), f, aspect)
      )
      case f @ FiniteComplement(_, _) => Vector(
        Argument.Clausal.FiniteComplement(Some(pred), f, aspect)
      )
      case f @ Inverted(_) => Vector(
        Argument.Clausal.Inverted(Some(pred), f, aspect)
      )
      // case _ => Vector()
    }
  }

  object ForComplementizer

  case class Complementizer(form: String)
  object Complementizer {
    def all = Set("that")
  }

  def onlyOneOf[A](x: Option[A], y: Option[A]): Option[Option[A]] = (x, y) match {
    case (Some(z), None) => Some(Some(z))
    case (None, Some(z)) => Some(Some(z))
    case (None, None) => Some(None)
    case (Some(_), Some(_)) => None
  }

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
    def mapToStream[B](f: A => ScoredStream[B]): ScoredStream[Node[B]] = {
      f(value).map(v => Node(v, path))
    }
  }

  val Aux = new ParseSymbol[Auxiliary]("Aux")
  val Cop = new ParseSymbol[Copula]("Cop")
  val Prep = new ParseSymbol[Preposition]("Prep")
  val For = new ParseSymbol[ForComplementizer.type]("For")
  val Comp = new ParseSymbol[Complementizer]("Comp")

  val VBar = new ParseSymbol[Node[VerbPhrase]]("V'")
  val VP = new ParseSymbol[Node[VerbPhrase]]("VP")
  val S = new ParseSymbol[Node[Clause]]("S")

  val AdvP = new ParseSymbol[Argument.Adverbial]("AdvP")
  val NP = new ParseSymbol[Node[Argument.Nominal]]("NP")
  val PP = new ParseSymbol[Node[Argument.Prepositional]]("PP")

  // val VP = new ParseSymbol[Node[Predication.Verbal]]("VP")
  // val Complement = new ParseSymbol[Node[Argument.Complement]]("Complement")

  val Arg = new ParseSymbol[Node[Argument]]("Arg")
  val Q = new ParseSymbol[Argument.Clausal.FiniteQuestion]("Question")

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

  def makeVBars(forms: InflectedForms, text: String, source: Option[Int]) = {
    vpFormMapping(forms).get(text).combineAll.foldMap(form =>
      ScoredStream.unit(
        Derivation(
          VBar,
          Node(
            VerbPhrase(
              Predication.Verbal(
                source,
                Lexicon.Verb(forms),
                isPassive = form == VPForm.Predicative,
                Vector()
              ),
              form, Aspect.default
            ),
            None
          ),
          text
        )
      )
    )
  }

  val extraVerbs = Vector(
    InflectedForms.doForms
  )

  val genlex = (s: SurfaceForm) => s match {
    case SurfaceForm.Excerpt(span, text) =>
      ScoredStream.unit(Derivation(NP, Node(inferNPFromSpan(span), None), text)).merge(
        ScoredStream.unit(Derivation(AdvP, inferAdvPFromSpan(span), text))
      )
    case SurfaceForm.Token(text, sourceOpt) =>
      import ArgumentPath.{End, Focus}
      List(
        whForms.get(text).map(pro =>
          Derivation(Arg, Node(pro, Some(Right(End(Focus)))), text)
        ).foldMap(ScoredStream.unit(_)),
        indefinitePronouns.get(text).map(pro =>
          Derivation(Arg, Node(pro, None), text)
        ).foldMap(ScoredStream.unit(_)),
        Auxiliary.all.get(text).foldMap(auxes =>
          auxes.foldMap(aux =>
            ScoredStream.unit(Derivation(Aux, aux, text))
          )
        ),
        Copula.forms.get(text).map(form =>
          Derivation(Cop, Copula(sourceOpt, form), text)
        ).foldMap(ScoredStream.unit(_)),
        sourceOpt.map(index =>
          sentence.verbEntries.get(index).foldMap(verb =>
            makeVBars(verb.verbInflectedForms, text, sourceOpt)
          )
        ).getOrElse(
          extraVerbs.foldMap(forms =>
            makeVBars(forms, text, None)
          )
        ),
        Option(text).filter(Preposition.preps.contains).foldMap(prep =>
          ScoredStream.unit(Derivation(Prep, Preposition(sourceOpt, text), text))
        ),
        Option(text).filter(_ == "for").foldMap(_ =>
          ScoredStream.unit(Derivation(For, ForComplementizer, text))
        ),
        Option(text).filter(Complementizer.all.contains).foldMap(comp =>
          ScoredStream.unit(Derivation(Comp, Complementizer(comp), text))
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
        makePP(prep, Node(arg, ArgumentPath.ascend(path, ArgPosition.Arg(0))))
    }
    val copulaVBar = (Cop, Arg) to VBar usingSingleZ {
      case (Copula(source, form), Node(arg: Argument.NounOrOblique, path)) =>
        Node(
          VerbPhrase(Predication.Copular(source, arg, Vector()), form, Aspect.default),
          ArgumentPath.ascend(path, ArgPosition.Arg(0))
        )
    }
    // TODO reevaluate this rule
    val preInvertedCopulaVBar = Arg to VBar usingSingle {
      case Node(arg: Argument.NounOrOblique, path) =>
        Scored(
          Node(
            VerbPhrase(Predication.Copular(None, arg, Vector()), VPForm.Predicative, Aspect.default),
            ArgumentPath.ascend(path, ArgPosition.Arg(0))
          ),
          0.0
        )
    }
    val vBarArg = (VBar, Arg) to VBar using {
      case (vBarNode, argNode) =>
        onlyOneOf(
          vBarNode.path,
          ArgumentPath.ascend(argNode.path, ArgPosition.Arg(vBarNode.value.pred.arguments.size))
        ).foldMap { path =>
          (vBarNode.value, argNode.value) match {
            case (
              vp @ VerbPhrase(cop: Predication.Copular, _, _),
              newMod: Argument.NonNominal
            ) =>
              ScoredStream.unit(
                Node(
                  vp.copy(pred = cop.copy(modifiers = cop.modifiers :+ newMod)),
                  path
                )
              )
            case (
              vp @ VerbPhrase(adj: Predication.Adjectival, _, _),
              newArg: Argument.NonNominal
            ) =>
              ScoredStream.unit(
                Node(
                  vp.copy(pred = adj.copy(arguments = adj.arguments :+ newArg)),
                  path
                )
              )
            case (
              vp @ VerbPhrase(verb: Predication.Verbal, _, _),
              newArg: Argument.NonSubject
            ) =>
              ScoredStream.unit(
                Node(
                  vp.copy(pred = verb.copy(arguments = verb.arguments :+ newArg)),
                  path
                )
              )
            case _ => ScoredStream.empty
          }
      }
    }
    val vp = VBar to VP usingSingleZ { case vbar => vbar }
    val auxVP = (Aux, VP) to VP using {
      case (aux, node) => aux.modifyVP(node.value)
          .foldMap(vp => ScoredStream.unit(node.copy(value = vp)))
    }
    val vpToS = (Arg, VP) to S using {
      case (subj, vp) =>
        onlyOneOf(
          ArgumentPath.ascend(subj.path, ArgPosition.Subj),
          vp.path
        ).foldMap { path =>
          val vals = (subj.value, vp.value) match {
            case (subj: Argument.Subject, VerbPhrase(vPred, vForm, aspect)) =>
              import VPForm._
              vForm match {
                case Progressive => ScoredStream.unit(
                  Clause(Predication.Clausal(subj, vPred), SForm.Progressive, aspect)
                )
                case Finite(tense, numAgr, persAgr) =>
                  val numGood = cats.data.Ior
                    .fromOptions(numAgr, subj.number)
                    .forall(_.onlyBoth.forall(Function.tupled(_ == _)))
                  val persGood = cats.data.Ior
                    .fromOptions(persAgr, subj.person)
                    .forall(_.onlyBoth.forall(Function.tupled(_ == _)))
                  if(numGood && persGood) {
                    ScoredStream.unit(
                      Clause(Predication.Clausal(subj, vPred), SForm.Finite(tense), aspect)
                    )
                  } else ScoredStream.empty

                case Predicative | BareInfinitive | ToInfinitive | Perfect =>
                  ScoredStream.empty
              }
            case _ => ScoredStream.empty
          }
          vals.map(value => Node(value, path))
        }
    }
    val vpWithExtractedSubjToSInv = (Arg, VP) to S using {
      case (
        Node(subj: Argument.Subject, Some(Left(path @ ArgumentPath.End(ArgumentPath.Target)))),
        Node(VerbPhrase(pred, VPForm.Finite(tense, _, _), aspect), None)
      ) =>
        ScoredStream.unit(
          Node(
            Clause(Predication.Clausal(subj, pred), SForm.Inverted(tense), aspect),
            Some(Left(ArgumentPath.Descent(ArgPosition.Subj, path)))
          )
        )
      case _ => ScoredStream.empty
    }
    // NOTE: this prohibits any extraction from inside the subject,
    // which I think is correct in English anyway.
    val vpToSInv = (Aux, Arg, VP) to S using {
      case (aux,
            Node(subj: Argument.Subject, None),
            Node(vp, vpPath)
      ) =>
        aux.modifyVP(vp).collect {
          case VerbPhrase(pred, VPForm.Finite(tense, numAgr, persAgr), aspect) =>
            val numGood = cats.data.Ior
              .fromOptions(numAgr, subj.number)
              .forall(_.onlyBoth.forall(Function.tupled(_ == _)))
            val persGood = cats.data.Ior
              .fromOptions(persAgr, subj.person)
              .forall(_.onlyBoth.forall(Function.tupled(_ == _)))
            if(numGood && persGood) {
              ScoredStream.unit(
                Node(
                  Clause(Predication.Clausal(subj, pred), SForm.Inverted(tense), aspect),
                  vpPath
                )
              )
            } else ScoredStream.empty[Node[Clause]]
        }.combineAll
    }
    val vpToForToS = (For, Arg, VP) to S using {
      case (_, subj, vp) =>
        onlyOneOf(
          ArgumentPath.ascend(subj.path, ArgPosition.Subj),
          vp.path
        ).foldMap { path =>
          (subj.value, vp.value) match {
            case (subj: Argument.Subject, VerbPhrase(vPred, VPForm.ToInfinitive, aspect)) =>
              ScoredStream.unit(
                Node(
                  Clause(Predication.Clausal(subj, vPred), SForm.ForToInfinitive, aspect),
                  path
                )
              )
            case _ => ScoredStream.empty
          }
        }
    }
    val sToComplement = (Comp, S) to S using {
      case (comp, sNode) =>
        sNode.mapToStream {
          case Clause(pred, SForm.Finite(tense), aspect) =>
            ScoredStream.unit(
              Clause(
                pred,
                SForm.FiniteComplement(
                  Lexicon.Complementizer(comp.form.lowerCase),
                  tense
                ),
                aspect
              )
            )
          case _ => ScoredStream.empty
        }
    }
    val sInvToQuestion = (Arg, S) to Q using {
      case (
        Node(extractee: Argument, Some(Right(focus))),
        Node(
          Clause(pred, form: SForm.Inverted, aspect),
          Some(Left(gap: ArgumentPath.Descent[ArgSnapshot]))
        )
      ) => {
        // println("########")
        // println(extractee)
        // println(pred)
        // println(gap)
        // println("-- -- --")
        // println(pred.render(form -> aspect, gap))
        // println(
        //   pred.render(form -> aspect, gap).toOption.map(_._2).map { snap =>
        //     snap.swapOut.replace(Right(extractee))
        //   }
        // )
        // println("^^^^^^^^")
        pred.render(form -> aspect, gap).toOption.map(_._2).foldMap { snap =>
          snap.swapOut.replace(Right(extractee)).toOption.foldMap { clause =>
            ScoredStream.unit(
              Argument.Clausal.FiniteQuestion(
                Some(clause), form, aspect, Some(gap.map(_ => ArgumentPath.Extract(focus)))
              )
            )
          }
        }
      }
    }

    val gap = () to Arg using ScoredStream.fromIndexedSeq(
      Vector(
        Argument.NounPhrase(None),
        Argument.Adverbial(None, Set()),
        // Argument.Prepositional(None, Set()),
        // Argument.Predicative(None),
        // Argument.Verbal.Gerund(None, Aspect.default),
        // Argument.Verbal.ToInfinitive(None, Aspect.default),
        // Argument.Verbal.Progressive(None, Aspect.default),
        // Argument.Clausal.Gerund(None, Aspect.default),
        // Argument.Clausal.ForToInfinitive(None, Aspect.default),
        // Argument.Clausal.Progressive(None, Aspect.default),
        // Argument.Clausal.Finite(None, SForm.Finite(qasrl.Tense.Finite.Past), Aspect.default),
        // Argument.Clausal.Inverted(None, SForm.Inverted(qasrl.Tense.Finite.Past), Aspect.default),
        // Argument.Clausal.FiniteComplement(
        //   // _could_ add other complementizers later, but might as well wait until
        //   // full lexicalization.
        //   None, SForm.FiniteComplement(
        //     Lexicon.Complementizer.that,
        //     qasrl.Tense.Finite.Past
        //   ), Aspect.default
        // )
      ).map(arg =>
        Scored[Node[Argument]](
          Node(
            arg,
            Some(Left(ArgumentPath.End(ArgumentPath.Target)))
          ),
          7.0 // penalty for gaps to aid parsing
        )
      )
    )


    val vpToArg = VP to Arg using {
      case vp => ScoredStream.fromIndexedSeq(
        vp.value.toArgument.map(arg => vp.map(_ => arg: Argument)).map(Scored.unit)
      )
    }
    val sToArg = S to Arg using {
      case s => ScoredStream.fromIndexedSeq(
        s.value.toArgument.map(arg => s.map(_ => arg: Argument)).map(Scored.unit)
      )
    }
    val ppToArg = PP to Arg usingSingleZ { case pp => pp }
    val npToArg = NP to Arg usingSingleZ { case np => np }
    val advpToArg = AdvP to Arg usingSingleZ { case advp => Node(advp, None) }
    prtp :: pp ::
      copulaVBar ::
      preInvertedCopulaVBar ::
      vBarArg :: vp :: auxVP ::
      vpToS :: vpWithExtractedSubjToSInv :: vpToSInv ::
      vpToForToS :: sToComplement ::
      sInvToQuestion ::
      gap ::
      vpToArg :: sToArg :: ppToArg :: npToArg :: advpToArg :: HNil
  }
  val grammar = SyncCFG(productions)
  val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genlex, grammar)
  def parse(forms: Vector[SurfaceForm]) =
    parser.parse(
      forms,
      Q,
      ScoredStream.recurrence(
        Scored(EvaluationBlock, 0.0), (x: EvaluationBlock.type) => Scored(x, 10.0)
      )
    )
}
