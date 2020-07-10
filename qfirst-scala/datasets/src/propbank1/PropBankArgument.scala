package qfirst.datasets.propbank1

import jjm.ling.ESpan

import qfirst.datasets.SyntaxTree

sealed trait PropBankArgument
object PropBankArgument {
  case class Node(branch: SyntaxTree.Branch) extends PropBankArgument
  case class Linked(nodes: List[PropBankArgument]) extends PropBankArgument
  case class ICHConcat(nodes: List[PropBankArgument]) extends PropBankArgument
  case class Concat(nodes: List[PropBankArgument]) extends PropBankArgument
}
