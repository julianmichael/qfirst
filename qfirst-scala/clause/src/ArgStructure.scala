package qfirst.clause

import cats.Id

import io.circe.generic.JsonCodec

import jjm.DependentMap
import jjm.ling.en.InflectedForms

import qasrl._

import monocle.macros._

@JsonCodec @Lenses case class ArgStructure(
  args: DependentMap[ArgumentSlot.Aux, Id],
  isPassive: Boolean
) {
  def forgetAnimacy = {
    val newArgs = args.keys.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id]) {
      (m, k) => k match {
        case Subj   => m.put(Subj, Noun(false))
        case Obj    => m.put(Obj, Noun(false))
        case Obj2  => m.put(
          Obj2, args.get(Obj2).get match {
            case Noun(_) => Noun(false)
            case Prep(p, Some(Noun(_))) => Prep(p, Some(Noun(false)))
            case x => x
          }
        )
        case Adv(wh) => m.put(Adv(wh), args.get(Adv(wh)).get)
      }
    }
    this.copy(args = newArgs)
  }
  override def toString = Frame(
    InflectedForms.generic, args, isPassive = isPassive, tense = qasrl.PresentTense, isPerfect = false, isProgressive = false, isNegated = false
  ).clauses.head
}
object ArgStructure
