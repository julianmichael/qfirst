package qfirst.propbank1

import jjm.ling.ESpan

sealed trait PropBankArgument
object PropBankArgument {
  case class Node(branch: SyntaxTreeBranch) extends PropBankArgument
  case class Linked(nodes: List[PropBankArgument]) extends PropBankArgument
  case class ICHConcat(nodes: List[PropBankArgument]) extends PropBankArgument
  case class Concat(nodes: List[PropBankArgument]) extends PropBankArgument
}

case class SyntaxTreeBranch(
  beginIndex: Int,
  constituentHeight: Int)

case class PredicateArgumentStructure(
  predicate: Predicate,
  arguments: List[(String, PropBankArgument)])

case class Predicate(
  index: Int,
  lemma: String,
  sense: String)
