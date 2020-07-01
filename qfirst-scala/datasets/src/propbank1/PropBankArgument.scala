package qfirst.datasets.propbank1

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
