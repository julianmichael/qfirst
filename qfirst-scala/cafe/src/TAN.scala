package qfirst.cafe

import jjm.DependentMap
import jjm.LowerCaseString
import jjm.ling.en._
import jjm.ling.en.VerbForm._
import jjm.implicits._

import cats.Id
import cats.Foldable
import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import cats.data.StateT
import cats.data.State
import cats.implicits._

import monocle.macros._

import io.circe.generic.JsonCodec

import qasrl.Tense
import qfirst.cafe.ClauseType.Infinitive
import cats.Parallel

@JsonCodec @Lenses case class TAN(
  tense: Option[Tense.Finite],
  isPerfect: Boolean,
  isProgressive: Boolean,
  isNegated: Boolean
) {

  type AuxChainResult[A] = Either[NonEmptyChain[String], A]

  def getCopulaAuxChain(
    clauseType: ClauseType.VerbalClauseType,
    subject: ArgumentPosition[Argument.Subject]
  ): AuxChainResult[NonEmptyList[String]] = {
    getAuxChain(InflectedForms.beSingularForms, clauseType, subject)
  }

  def getAuxChain(
    forms: InflectedForms,
    clauseType: ClauseType.VerbalClauseType,
    subject: ArgumentPosition[Argument.Subject]
  ): AuxChainResult[NonEmptyList[String]] = clauseType match {
    case ClauseType.Bare =>
      Tense.NonFinite.Bare.asRight[NonEmptyChain[String]].map(getVerbStack(forms, _))
    case ClauseType.Infinitive =>
      Tense.NonFinite.To.asRight[NonEmptyChain[String]].map(getVerbStack(forms, _))
    case ClauseType.Progressive =>
      Tense.NonFinite.Gerund.asRight[NonEmptyChain[String]].map(getVerbStack(forms, _))
    case ClauseType.Finite => for {
      tense <- tense.toRight(NonEmptyChain.one("Finite clause needs a determined tense"))
      (person, number) <- Parallel.parProduct(
        subject.person.toRight(
          NonEmptyChain.one("Subject of a finite clause needs person feature for agreement")),
        subject.number.toRight(
          NonEmptyChain.one("Subject of a finite clause needs number feature for agreement"))
      )
    } yield fixAgreement(getVerbStack(forms, tense), forms, person, number)
  }

  def fixAgreement(
    verbStack: NonEmptyList[String],
    forms: InflectedForms,
    person: Person,
    number: Number
  ): NonEmptyList[String] = {
    modTop { top =>
      val theseForms = getForms(top.lowerCase, forms).get
      val form = theseForms.getForm(top.lowerCase)
      if(theseForms == InflectedForms.beSingularForms) {
        import Person._, Number._
        ((person, number), top) match {
          case ((First, Singular), "is") => "am"
          case ((Second, Singular) | (_, Plural), "is") => "are"
          case ((Second, Singular) | (_, Plural), "was") => "were"
          case _ => top // already third person singular, or 1st person singular past
        }
      } else if(top == forms.presentSingular3rd.toString && number == Number.Plural) {
        forms.stem.toString
      } else top
    }.runS(verbStack).value
  }

  private[this] def modalTokens(modal: LowerCaseString) =
    if (isNegated) {
      if (modal.toString == "will") NonEmptyList.of("won't")
      else if (modal.toString == "can") NonEmptyList.of("can't")
      else if (modal.toString == "might") NonEmptyList.of("might", "not")
      else NonEmptyList.of(s"${modal}n't")
    } else {
      NonEmptyList.of(modal.toString)
    }

  private[this] def getForms(s: LowerCaseString, verbForms: InflectedForms) = {
    if(verbForms.allForms.contains(s)) Some(verbForms)
    else if (InflectedForms.beSingularForms.allForms.contains(s)) Some(InflectedForms.beSingularForms)
    else if (InflectedForms.doForms.allForms.contains(s)) Some(InflectedForms.doForms)
    else if (InflectedForms.haveForms.allForms.contains(s)) Some(InflectedForms.haveForms)
    else None
  }

  private[this] def push(s: String) =
    State.modify[NonEmptyList[String]](s :: _)
  private[this] def pushAll(ss: NonEmptyList[String]) =
    State.modify[NonEmptyList[String]](x => ss ++ x.toList)
  private[this] def modTop(f: String => String) =
    State.modify[NonEmptyList[String]](l => NonEmptyList(f(l.head), l.tail))
  private[this] def modForm(form: VerbForm, forms: InflectedForms) =
    modTop(w => getForms(w.lowerCase, forms).fold(w)(_(form)))
  private[this] def pass = State.pure[NonEmptyList[String], Unit](())

  def getVerbStack(forms: InflectedForms, tense: Tense) = {
    val stackState = if(tense == Tense.NonFinite.Gerund && isProgressive && !isPerfect) {
      // avoid e.g. "being swimming"
      modForm(PresentParticiple, forms) >> (if (isNegated) push("not") else pass)
    } else {
      for {
        // start with verb stem
        _ <- (if (isProgressive) modForm(PresentParticiple, forms) >> push("be") else pass)
        _ <- (if (isPerfect) modForm(PastParticiple, forms) >> push("have") else pass)
        postAspectStack <- State.get[NonEmptyList[String]]
        _ <- tense match {
          case Tense.Finite.Modal(m) => pushAll(modalTokens(m))
          case Tense.Finite.Past =>
            if (isNegated) {
              if (postAspectStack.size == 1) push("didn't")
              else (modForm(Past, forms) >> modTop(_ + "n't"))
            } else modForm(Past, forms)
          case Tense.Finite.Present =>
            if (isNegated) {
              if (postAspectStack.size == 1) push("doesn't")
              else (modForm(PresentSingular3rd, forms) >> modTop(_ + "n't"))
            } else modForm(PresentSingular3rd, forms)
          case nf: Tense.NonFinite =>
            val verbMod = nf match {
              case Tense.NonFinite.Bare => pass
              case Tense.NonFinite.To => push("to")
              case Tense.NonFinite.Gerund => modForm(PresentParticiple, forms)
            }
            verbMod >> (if (isNegated) push("not") else pass)
        }
      } yield ()
    }

    stackState.runS(NonEmptyList.of(forms.stem.toString)).value
  }

  // def splitVerbStackIfNecessary(verbStack: NonEmptyList[String], forms: InflectedForms) = {
  //   if (verbStack.size > 1) {
  //     verbStack
  //   } else
  //     tense match {
  //       case Tense.Finite.Past     => (modForm(Stem, forms) >> push("did")).runS(verbStack).value
  //       case Tense.Finite.Present  => (modForm(Stem, forms) >> push("does")).runS(verbStack).value
  //       case Tense.Finite.Modal(_) => verbStack // should never happen, since a modal adds another token
  //       case _ => verbStack // Non-finite case, where splitting cannot occur
  //     }
  // }

  // should always agree with what's produced on the verb stack.
  // ideally they would share code somehow but this is easiest for now and probably works.
  // def getVerbConjugation(subjectPresent: Boolean): VerbForm = {
  //   if (isPassive) PastParticiple
  //   else if (isProgressive) PresentParticiple
  //   else if (isPerfect) PastParticiple
  //   else
  //     tense match {
  //       case Tense.Finite.Modal(_)              => Stem
  //       case _ if (isNegated || subjectPresent) => Stem
  //       case Tense.Finite.Past                  => Past
  //       case Tense.Finite.Present               => PresentSingular3rd
  //       case Tense.NonFinite.Bare               => Stem
  //       case Tense.NonFinite.To                 => Stem
  //       case Tense.NonFinite.Gerund             => PresentParticiple
  //     }
  // }

  // private[this] def renderAuxThroughVerb[A](includeSubject: Boolean, argValues: ArgMap[A]) = {
  //   val verbStack = getVerbStack
  //   if (includeSubject) {
  //     val splitVerbStack = splitVerbStackIfNecessary(verbStack)
  //     val (aux, verb) = (splitVerbStack.head, splitVerbStack.tail)
  //     appendString[A](aux) >> renderNecessaryNoun(Subj, argValues) >> appendAllStrings[List, A](verb)
  //   } else appendAllStrings[NonEmptyList, A](verbStack)
  // }
}
object TAN

