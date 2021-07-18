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
  case class Preposition(form: LowerCaseString)
  object Preposition {
    val mostCommonPrepositions = Set(
      "by", "for", "with",
      // "about", // too many spurious questions from this
      "in", "from", "to", "as" // added my own on this line
    ).map(_.lowerCase)

    // TODO bigger list
    val preps = Set(
      "aboard", "about", "above", "across", "afore",
      "after", "against", "ahead", "along", "alongside",
      "amid", "amidst", "among", "amongst", "around",
      "as", "aside", "astride", "at", "atop",
      "before", "behind", "below", "beneath", "beside",
      "besides", "between", "beyond", "by", "despite",
      "down", "during", "except", "for", "from",
      "given", "in", "inside", "into", "near",
      "next", "of", "off", "on", "onto",
      "opposite", "out", "outside", "over", "pace",
      "per", "round", "since", "than", "through",
      "throughout", "till", "times", "to", "toward",
      "towards", "under", "underneath", "until", "unto",
      "up", "upon", "versus", "via", "with",
      "within", "without"
    ).map(_.lowerCase)

  }
  case class Complementizer(form: LowerCaseString)
  object Complementizer {
    val that = Complementizer("that".lowerCase)
    // TODO maybe put separately for embedded-question complementizers?
    // val whether = Complementizer("whether".lowerCase)
    // val `if` = Complementizer("if".lowerCase)
  }
  case class InfinitiveComplementizer(form: LowerCaseString)
  object InfinitiveComplementizer {
    val `for` = InfinitiveComplementizer("for".lowerCase)
  }
  case class Adjective(form: LowerCaseString)
  case class Verb(forms: InflectedForms)

  sealed trait Wh {
    def form: LowerCaseString
  }
  object Wh {
    case class Noun(form: LowerCaseString) extends Wh
    val who = Noun("who".lowerCase)
    val what = Noun("what".lowerCase)
    case class Adverb(form: LowerCaseString) extends Wh
    val when = Adverb("when".lowerCase)
    val where = Adverb("where".lowerCase)
    val why = Adverb("why".lowerCase)
    val how = Adverb("how".lowerCase)
    // last two are in QA-SRL, but maybe questionable;
    // could perhaps work them in as adjectives or something?
    // need to look at the data.
    val howMuch = Adverb("how much".lowerCase)
    val howLong = Adverb("how long".lowerCase)
  }
}
