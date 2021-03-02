package qfirst.parsing

import shapeless._

class ParsingTestSuite extends munit.CatsEffectSuite {

  // EXAMPLE USAGE

  import SyncCFGProductionSyntax._

  // example of how to make productions
  case class ExpStr(x: String)
  case class ExpInt(i: Int)

  val StrSymb = new ParseSymbol[ExpStr]("Str")
  val StrSymb2 = new ParseSymbol[ExpStr]("Str")
  val IntSymb = new ParseSymbol[ExpInt]("Int")

  val genTerminals = (s: String) => {
    val term = ScoredStream.unit(Derivation(Terminal(s), s))
    val num = scala.util.Try(s.toInt).toOption match {
      case None => ScoredStream.empty[Derivation]
      case Some(i) => ScoredStream.unit(Scored(Derivation(IntSymb, ExpInt(i)), 1.0))
    }
    val arbStr = ScoredStream.unit(Scored(Derivation(StrSymb, ExpStr(s)), 2.0))
    term merge num merge arbStr
  }

  val prod1 = StrSymb to StrSymb usingSingle {
    case ExpStr(str) => Scored(ExpStr(s"($str)"), 1.0)
  }
  val prod2 = (IntSymb, StrSymb) to StrSymb2 using {
    case ExpInt(i) :: ExpStr(str) :: HNil => ScoredStream.unit(Scored(ExpStr(s"${i * i}:$str"), 1.0))
  }
  val prod3 = (IntSymb, t"+", IntSymb) to IntSymb usingSingle {
    case ExpInt(i) :: _ :: ExpInt(i2) :: HNil => Scored(ExpInt(i + i2), 1.0)
  }
  val prod4 = () to IntSymb usingSingle Scored(ExpInt(0), 1.0)

  val grammar1 = SyncCFG(prod1 :: HNil)
  val grammar2 = SyncCFG(prod1 :: prod2 :: HNil)
  val grammar3 = SyncCFG(prod1 :: prod2 :: prod4 :: HNil)

  // more testy grammary
  val cnf1 = SyncCNFGrammar.productionsFromSyncCFG(grammar1)
  val cnf2 = SyncCNFGrammar.productionsFromSyncCFG(grammar2)
  val cnf3 = SyncCNFGrammar.productionsFromSyncCFG(grammar3)

  val parser1 = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar1)
  val parser2 = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar2)
  val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar3)

  // def parseTest(s: String) = parser.parse(s.split(" ").toVector, IntSymb)

  test("sync calculator") {
    assertEquals(Calculator.parseTest("2 + 3").headItemOption.map(_.item), Some(5))
  }
}

object Calculator {

  import SyncCFGProductionSyntax._

  val Num = new ParseSymbol[Int]("Num")
  val genlex = (s: String) => {
    val term = ScoredStream.unit(Derivation(Terminal(s), s))
    val num = scala.util.Try(s.toInt).toOption match {
      case None => ScoredStream.empty[Derivation]
      case Some(i) => ScoredStream.unit(Scored(Derivation(Num, i), 1.0))
    }
    num merge term
  }
  val productions = {
    val plus = (Num, Terminal("+"), Num) to Num using {
      case i :: _ :: j :: HNil => ScoredStream.unit(Scored(i + j, 1.0))
    }
    val minus = (Num, Terminal("-"), Num) to Num using {
      case i :: _ :: j :: HNil => ScoredStream.unit(Scored(i - j, 1.0))
    }
    val times = (Num, Terminal("*"), Num) to Num using {
      case i :: _ :: j :: HNil => ScoredStream.unit(Scored(i * j, 1.0))
    }
    val div = (Num, Terminal("/"), Num) to Num using {
      case i :: _ :: j :: HNil => ScoredStream.unit(Scored(i / j, 1.0))
    }
    val parens = (Terminal("("), Num, Terminal(")")) to Num using {
      case _ :: i :: _ :: HNil => ScoredStream.unit(Scored(i, 1.0))
    }
    plus :: minus :: times :: div :: parens :: HNil
    // ???
  }
  val grammar = SyncCFG(productions)
  val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genlex, grammar)
  def parseTest(s: String) = parser.parse(s.split(" ").toVector, Num)
}
