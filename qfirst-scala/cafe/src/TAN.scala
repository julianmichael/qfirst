package qfirst.cafe

import jjm.DependentMap
import jjm.LowerCaseString
import jjm.ling.en._
import jjm.ling.en.VerbForm._
import jjm.implicits._

import cats.Id
import cats.Foldable
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.State
import cats.implicits._

import monocle.macros._

import io.circe.generic.JsonCodec

@JsonCodec @Lenses case class TAN(
  tense: Tense,
  isPerfect: Boolean,
  isProgressive: Boolean,
  isNegated: Boolean
) {
  def getVerbPrefixAndForm(
    isPassive: Boolean,
    subjectPresent: Boolean
  ): (List[LowerCaseString], VerbForm) = {
    val dummyFrame = Frame(
      ArgStructure(DependentMap.empty[ArgumentSlot.Aux, Id], isPassive),
      InflectedForms.generic, this)
    val initVerbStack = dummyFrame.getVerbStack
    val verbStack = if(subjectPresent) {
      dummyFrame.splitVerbStackIfNecessary(initVerbStack)
    } else initVerbStack
    val verbPrefix = verbStack.init.map(_.lowerCase)
    val verbForm = dummyFrame.getVerbConjugation(subjectPresent)
    verbPrefix -> verbForm
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

  private[this] def getForms(s: LowerCaseString) = {
    if (verbInflectedForms.allForms.contains(s)) Some(verbInflectedForms)
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
  private[this] def modForm(form: VerbForm) =
    modTop(w => getForms(w.lowerCase).fold(w)(_(form)))


  def getVerbStack = {
    def pass = State.pure[NonEmptyList[String], Unit](())

    val stackState = for {
      // start with verb stem
      _               <- (if (isPassive) modForm(PastParticiple) >> push("be") else pass)
      _               <- (if (isProgressive) modForm(PresentParticiple) >> push("be") else pass)
      _               <- (if (isPerfect) modForm(PastParticiple) >> push("have") else pass)
      postAspectStack <- State.get[NonEmptyList[String]]
      _ <- tense match {
        case Tense.Finite.Modal(m) => pushAll(modalTokens(m))
        case Tense.Finite.Past =>
          if (isNegated) {
            if (postAspectStack.size == 1) push("didn't")
            else (modForm(Past) >> modTop(_ + "n't"))
          } else modForm(Past)
        case Tense.Finite.Present =>
          if (isNegated) {
            if (postAspectStack.size == 1) push("doesn't")
            else (modForm(PresentSingular3rd) >> modTop(_ + "n't"))
          } else modForm(PresentSingular3rd)
        case nf: Tense.NonFinite =>
          val verbMod = nf match {
            case Tense.NonFinite.Bare => pass
            case Tense.NonFinite.To => push("to")
            case Tense.NonFinite.Gerund => modForm(PresentParticiple)
          }
          verbMod >> (if (isNegated) push("not") else pass)
      }
    } yield ()

    stackState.runS(NonEmptyList.of(verbInflectedForms.stem)).value
  }

  def splitVerbStackIfNecessary(verbStack: NonEmptyList[String]) = {
    if (verbStack.size > 1) {
      verbStack
    } else
      tense match {
        case Tense.Finite.Past     => (modForm(Stem) >> push("did")).runS(verbStack).value
        case Tense.Finite.Present  => (modForm(Stem) >> push("does")).runS(verbStack).value
        case Tense.Finite.Modal(_) => verbStack // should never happen, since a modal adds another token
        case _ => verbStack // Non-finite case, where splitting cannot occur
      }
  }

  // should always agree with what's produced on the verb stack.
  // ideally they would share code somehow but this is easiest for now and probably works.
  def getVerbConjugation(subjectPresent: Boolean): VerbForm = {
    if (isPassive) PastParticiple
    else if (isProgressive) PresentParticiple
    else if (isPerfect) PastParticiple
    else
      tense match {
        case Tense.Finite.Modal(_)              => Stem
        case _ if (isNegated || subjectPresent) => Stem
        case Tense.Finite.Past                  => Past
        case Tense.Finite.Present               => PresentSingular3rd
        case Tense.NonFinite.Bare               => Stem
        case Tense.NonFinite.To                 => Stem
        case Tense.NonFinite.Gerund             => PresentParticiple
      }
  }

  private[this] def renderAuxThroughVerb[A](includeSubject: Boolean, argValues: ArgMap[A]) = {
    val verbStack = getVerbStack
    if (includeSubject) {
      val splitVerbStack = splitVerbStackIfNecessary(verbStack)
      val (aux, verb) = (splitVerbStack.head, splitVerbStack.tail)
      appendString[A](aux) >> renderNecessaryNoun(Subj, argValues) >> appendAllStrings[List, A](verb)
    } else appendAllStrings[NonEmptyList, A](verbStack)
  }
}
object TAN

