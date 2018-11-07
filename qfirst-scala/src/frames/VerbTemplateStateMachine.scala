package qfirst.frames

import qasrl.util.implicits._
import qasrl.{Tense, Modal, PresentTense, PastTense}

import cats.data.NonEmptyList
import cats.data.StateT
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

import monocle.macros._

object VerbTemplateStateMachine {
  @Lenses case class VerbState(
    subj: Option[Noun],
    isPassive: Boolean,
    tan: TAN
  )
  object VerbState {
    def initial = VerbState(None, false, TAN(PastTense, false, false, false))
  }

  sealed trait TemplateState
  case object TemplateComplete extends TemplateState
  case class TemplateProgress(
    transitions: NonEmptyList[TemplateTransition]
  ) extends TemplateState {
    def +(transition: TemplateTransition) = TemplateProgress(
      transition :: transitions
    )
  }

  type TemplateTransition = (String, StateT[Option, VerbState, TemplateState])

  def pure[A](a: A) = StateT.pure[Option, VerbState, A](a)
  def abort[A] = StateT.liftF[Option, VerbState, A](None)
  def guard(cond: Boolean) = if(cond) pure(()) else abort[Unit]
  def get = StateT.get[Option, VerbState]
  def set(fs: VerbState) = StateT.set[Option, VerbState](fs)
  def modify(f: VerbState => VerbState) = StateT.modify[Option, VerbState](f)
  def modTAN(f: TAN => TAN) = StateT.modify[Option, VerbState](VerbState.tan.modify(f))
  def lift[A](aOpt: Option[A]) = StateT.liftF[Option, VerbState, A](aOpt)

  def modifyOpt(f: VerbState => Option[VerbState]) = for {
    fs <- get
    newVerbState <- lift(f(fs))
    _ <- set(newVerbState)
  } yield newVerbState

  def modifyTANOpt(f: TAN => Option[TAN]) = for {
    fs <- get
    newTAN <- lift(f(fs.tan))
    _ <- set(fs.copy(tan = newTAN))
  } yield newTAN

  def progress(first: TemplateTransition, rest: TemplateTransition*) = TemplateProgress(
    NonEmptyList.of(first, rest: _*)
  )

  def markSubj(isAnimate: Boolean) = modify(VerbState.subj.set(Some(Noun(isAnimate))))
}
class VerbTemplateStateMachine(
  verbInflectedForms: InflectedForms,
  subjRequired: Boolean,
  next: VerbTemplateStateMachine.TemplateState
) {

  import VerbTemplateStateMachine._
  import verbInflectedForms._

  // follows a have-aux. assume already isPerfect
  val pastParticipleVerb = progress(
    s" been $presentParticiple" -> modify(
      (VerbState.tan composeLens TAN.isProgressive).set(true)
    ).as(next),
    s" been $pastParticiple" -> modify((VerbState.isPassive).set(true))
      .as(next),
    (" " + pastParticiple.toString) -> pure(next)
  )

  // follows a modal
  val infinitiveVerb = progress(
    (" " + stem.toString) -> pure(next),
    s" be $presentParticiple" -> modify(
      (VerbState.tan composeLens TAN.isProgressive).set(true)
    ).as(next),
    s" have been $presentParticiple" -> modify(
      (VerbState.tan composeLens TAN.isPerfect).set(true)
      andThen (VerbState.tan composeLens TAN.isProgressive).set(true)
    ).as(next),
    s" be $pastParticiple" -> modify((VerbState.isPassive).set(true))
      .as(next),
    s" have $pastParticiple" -> modify((VerbState.tan composeLens TAN.isPerfect).set(true))
      .as(next),
    s" have been $pastParticiple" -> modify(
      (VerbState.tan composeLens TAN.isPerfect).set(true)
      andThen (VerbState.isPassive).set(true)
    ).as(next)
  )

  // follows a do-aux
  val stemVerb = progress(
    " " + stem.toString -> pure(next)
  )

  // follows a be-aux
  val presentParticipleOrPassiveVerb = progress(
    (" " + presentParticiple.toString) -> modify(
      (VerbState.tan composeLens TAN.isProgressive).set(true)
    ).as(next),
    (s" being $pastParticiple") -> modify(
      (VerbState.tan composeLens TAN.isProgressive).set(true)
      andThen (VerbState.isPassive).set(true)
    ).as(next),
    (" " + pastParticiple.toString) -> modify(
      (VerbState.isPassive).set(true)
    ).as(next)
  )

  // follows no aux
  val tensedVerb = progress(
    (present.toString) -> modTAN(TAN.tense.set(PresentTense)).as(next),
    (past.toString)    -> modTAN(TAN.tense.set(PastTense)).as(next)
  )

  // neg/subj states carry the verb form through; so, all need to be constructed at construction time

  def postSubjectNegation(targetVerbState: TemplateState) = progress(
    ""     -> pure(targetVerbState),
    " not" -> modify((VerbState.tan composeLens TAN.isNegated).set(true)).as(targetVerbState)
  )

  def subj(targetVerbState: TemplateState, alreadyNegated: Boolean) = {
    val target = if (alreadyNegated) targetVerbState else postSubjectNegation(targetVerbState)
    progress(
      " someone"   -> markSubj(true).as(target),
      " something" -> markSubj(false).as(target),
      " it"        -> markSubj(false).as(target)
    )
  }

  def optionalSubj(targetVerbState: TemplateState, alreadyNegated: Boolean) = {
    val skipSubjTarget =
      if (alreadyNegated) targetVerbState else postSubjectNegation(targetVerbState)
    progress(
      "" -> pure(subj(targetVerbState, alreadyNegated)),
      "" -> pure(skipSubjTarget) // can skip directly to verb if we make subj the answer
    )
  }

  def negContraction(subjRequired: Boolean, targetVerbState: TemplateState) = {
    def target(negate: Boolean) =
      if (subjRequired) subj(targetVerbState, negate) else optionalSubj(targetVerbState, negate)
    progress(
      ""    -> pure(target(false)),
      "n't" -> modify((VerbState.tan composeLens TAN.isNegated).set(true)).as(target(true))
    )
  }

  def haveAux(subjRequired: Boolean) = {
    val target = negContraction(subjRequired, pastParticipleVerb)
    progress(
      "has" -> modTAN(TAN.tense.set(PresentTense) andThen TAN.isPerfect.set(true))
        .as(target),
      "had" -> modTAN(TAN.tense.set(PastTense) andThen TAN.isPerfect.set(true)).as(target)
    )
  }

  def infNegContraction(subjRequired: Boolean) = negContraction(subjRequired, infinitiveVerb)

  def modalAux(subjRequired: Boolean) = {
    def infSubj(negate: Boolean) =
      if (subjRequired) subj(infinitiveVerb, negate) else optionalSubj(infinitiveVerb, negate)
    val infNegContraction = negContraction(subjRequired, infinitiveVerb)
    progress(
      "can't" -> modTAN(
        TAN.tense.set(Modal("can".lowerCase)) andThen TAN.isNegated.set(true)
      ).as(infSubj(true)),
      "can" -> modTAN(TAN.tense.set(Modal("can".lowerCase))).as(infSubj(false)),
      "won't" -> modTAN(
        TAN.tense.set(Modal("will".lowerCase)) andThen TAN.isNegated.set(true)
      ).as(infSubj(true)),
      "will"   -> modTAN(TAN.tense.set(Modal("will".lowerCase))).as(infSubj(false)),
      "might"  -> modTAN(TAN.tense.set(Modal("might".lowerCase))).as(infSubj(false)),
      "would"  -> modTAN(TAN.tense.set(Modal("would".lowerCase))).as(infNegContraction),
      "should" -> modTAN(TAN.tense.set(Modal("should".lowerCase))).as(infNegContraction)
    )
  }

  def doAux(subjRequired: Boolean) = {
    val target = negContraction(subjRequired, stemVerb)
    progress(
      "does" -> modTAN(TAN.tense.set(PresentTense)).as(target),
      "did"  -> modTAN(TAN.tense.set(PastTense)).as(target)
    )
  }

  def beAux(subjRequired: Boolean) = {
    val target = negContraction(subjRequired, presentParticipleOrPassiveVerb)
    progress(
      "is"  -> modTAN(TAN.tense.set(PresentTense)).as(target),
      "was" -> modTAN(TAN.tense.set(PastTense)).as(target)
    )
  }

  def start = {
    val tail = NonEmptyList.of[TemplateTransition](
      "" -> pure(beAux(subjRequired)),
      "" -> pure(modalAux(subjRequired)),
      "" -> pure(doAux(subjRequired)),
      "" -> pure(haveAux(subjRequired))
    )
    val straightToVerb: TemplateTransition = "" -> pure(tensedVerb)
    val transitions = if (subjRequired) tail else straightToVerb :: tail
    TemplateProgress(transitions)
  }
}
