package qfirst.datasets.propbank1

import jjm.ling.ESpan

import qfirst.datasets.SyntaxTree

sealed trait PropBankArgument {
  def allBranches: List[SyntaxTree.Branch] = this match {
    case PropBankArgument.Node(branch) => List(branch)
    case compound: CompoundPropBankArgument => compound.subArgs.flatMap(_.allBranches)
  }
}
sealed trait CompoundPropBankArgument extends PropBankArgument {
  def subArgs: List[PropBankArgument]
}
object PropBankArgument {
  case class Node(branch: SyntaxTree.Branch) extends PropBankArgument
  case class Linked(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument
  case class ICHConcat(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument
  case class Concat(val subArgs: List[PropBankArgument]) extends CompoundPropBankArgument
}
