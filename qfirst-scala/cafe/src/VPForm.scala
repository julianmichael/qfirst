package qfirst.cafe

import qasrl.Tense

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.implicits._

import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import cats.data.State
import cats.implicits._
import cats.Parallel

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

  type AuxChainResult[A] = Either[NonEmptyChain[String], A]

  import VPForm._
  def getAuxChain(
    forms: InflectedForms,
    aspect: Aspect,
    ensureDoSupport: Boolean
  ): NonEmptyList[String] = this match {
    case Finite(tense, num, pers)  =>
      fixAgreement(
        getVerbStack(forms, aspect, ensureDoSupport), // && clauseType == Inverted
        forms, pers, num
      )
    case other => getVerbStack(forms, aspect, ensureDoSupport)
  }

  private def fixAgreement(
    verbStack: NonEmptyList[String],
    forms: InflectedForms,
    person: Option[Person],
    number: Option[Number]
  ): NonEmptyList[String] = {
    modTop { top =>
      // not present means we have a modal on top; no need to change
      getForms(top.lowerCase, forms).fold(top) { theseForms =>
        val form = theseForms.getForm(top.lowerCase)
        if(theseForms == InflectedForms.beSingularForms) {
          import Person._, Number._
          ((person, number), top) match {
            case ((Some(First), Some(Singular)), "is") => "am"
            case ((_, Some(Plural)), "is") => "are"
            case ((_, Some(Plural)), "was") => "were"
            case _ => top // already third person singular, or 1st person singular past
          }
        } else if(top == forms.presentSingular3rd.toString && number == Some(Number.Plural)) {
          forms.stem.toString
        } else top
      }
    }.runS(verbStack).value
  }

  private[this] def modalTokens(modal: LowerCaseString, aspect: Aspect) =
    if (aspect.isNegated) {
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

  def getInitVerbStack(forms: InflectedForms, aspect: Aspect) = {
    import VerbForm._
    // start with verb stem
    val stackState = if(this == Progressive && aspect.isProgressive && !aspect.isPerfect) {
      // avoid e.g. "being swimming"
      modForm(PresentParticiple, forms) >> (if (aspect.isNegated) push("not") else pass)
    } else if(this == Predicative) { // ignore aspect in this case
      modForm(PastParticiple, forms) >> (if (aspect.isNegated) push("not") else pass)
    } else {
      for {
        _ <- (if (aspect.isProgressive) modForm(PresentParticiple, forms) >> push("be") else pass)
        _ <- (if (aspect.isPerfect) modForm(PastParticiple, forms) >> push("have") else pass)
        postAspectStack <- State.get[NonEmptyList[String]]
        _ <- this match {
          case BareInfinitive => pass >> (if (aspect.isNegated) push("not") else pass)
          case ToInfinitive => push("to") >> (if (aspect.isNegated) push("not") else pass)
          case Perfect => modForm(PastParticiple, forms) >> // possibly slightly weird
              (if (aspect.isNegated) push("not") else pass)
          case Progressive => modForm(PresentParticiple, forms) >>
              (if (aspect.isNegated) push("not") else pass)
          case Finite(tense, _, _) => tense match {
            case Tense.Finite.Modal(m) => pushAll(modalTokens(m, aspect))
            case Tense.Finite.Past =>
              if (aspect.isNegated) {
                if (postAspectStack.size == 1) push("didn't")
                else (modForm(Past, forms) >> modTop(_ + "n't"))
              } else modForm(Past, forms)
            case Tense.Finite.Present =>
              if (aspect.isNegated) {
                if (postAspectStack.size == 1) push("doesn't")
                else (modForm(PresentSingular3rd, forms) >> modTop(_ + "n't"))
              } else modForm(PresentSingular3rd, forms)
            }
          case Predicative => ??? // should never happen
        }
      } yield ()
    }

    stackState.runS(NonEmptyList.of(forms.stem.toString)).value
  }

  def getVerbStack(forms: InflectedForms, aspect: Aspect, ensureDoSupport: Boolean) = {
    val stack = getInitVerbStack(forms, aspect)
    if(ensureDoSupport) addDoSupportIfNecessary(stack, forms)
    else stack
  }

  def addDoSupportIfNecessary(verbStack: NonEmptyList[String], forms: InflectedForms) = {
    import VerbForm._
    if (verbStack.size > 1) {
      verbStack
    } else this match {
      case Finite(tense, _, _) => tense match {
        case Tense.Finite.Past     => (modForm(Stem, forms) >> push("did")).runS(verbStack).value
        case Tense.Finite.Present  => (modForm(Stem, forms) >> push("does")).runS(verbStack).value
        case Tense.Finite.Modal(_) => verbStack // should never happen, since a modal adds another token
      }
      case _ => verbStack
    }
  }

}
object VPForm {

  case object Predicative extends VPForm
  sealed trait NotPredicative extends VPForm {
    def getCopulaAuxChain(aspect: Aspect): NonEmptyList[String] = {
      getAuxChain(InflectedForms.beSingularForms, aspect, false)
    }
  }
  case object BareInfinitive extends NotPredicative
  case object ToInfinitive extends NotPredicative
  case object Perfect extends NotPredicative {
    override def resolveAspect(aspect: Aspect): Option[Aspect] =
      if(aspect.isPerfect) None else Some(aspect.copy(isPerfect = true))
  }
  case object Progressive extends NotPredicative {
    override def resolveAspect(aspect: Aspect): Option[Aspect] =
      if(aspect.isProgressive) None else Some(aspect.copy(isProgressive = true))
  }
  case class Finite(
    tense: Tense.Finite,
    subjectNumber: Option[Number],
    subjectPerson: Option[Person]
  ) extends NotPredicative
}
