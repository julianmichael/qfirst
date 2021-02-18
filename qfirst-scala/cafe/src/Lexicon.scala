package qfirst.cafe

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

object Lexicon {
  // case class Wh(form: LowerCaseString)
  // object Wh {
  //   val who = Wh("who".lowerCase)
  //   val what = Wh("what".lowerCase)
  //   val when = Wh("when".lowerCase)
  //   val where = Wh("where".lowerCase)
  //   val why = Wh("why".lowerCase)
  // }
  case class Expletive(form: LowerCaseString) // it, there
  object Expletive {
    val it = Expletive("it".lowerCase)
    val there = Expletive("there".lowerCase)
  }
  case class Particle(form: LowerCaseString)
  case class Preposition(form: LowerCaseString)
  case class Complementizer(form: LowerCaseString)
  case class InfinitiveComplementizer(form: LowerCaseString)
  case class Subordinator(form: LowerCaseString)
  case class Adjective(form: LowerCaseString)
  case class Verb(forms: InflectedForms)
}
