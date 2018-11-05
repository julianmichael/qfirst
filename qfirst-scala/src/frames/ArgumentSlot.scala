package qfirst.frames

import nlpdata.util.LowerCaseStrings._

sealed trait ArgumentSlot { type Arg }
case object Subj extends ArgumentSlot { type Arg = Noun }
case object Obj extends ArgumentSlot { type Arg = Noun }
case object Prep1 extends ArgumentSlot { type Arg = Preposition }
case object Prep2 extends ArgumentSlot { type Arg = Preposition }
case object Misc extends ArgumentSlot { type Arg = NonPrepArgument }
case class Adv(wh: LowerCaseString) extends ArgumentSlot { type Arg = Unit }

object ArgumentSlot {
  type Aux[A] = ArgumentSlot { type Arg = A }

  def allAdvSlots =
    List("when", "where", "why", "how", "how long", "how much").map(s => Adv(s.lowerCase))
}
