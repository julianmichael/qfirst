package qfirst.datasets.propbank1

import qfirst.datasets.SyntaxTree

import cats.implicits._

import jjm.ling.ESpan
import jjm.ling.{HasIndex, HasToken}
import jjm.implicits._

sealed trait PropBankArgument {
  import PropBankArgument._
  def allBranches: List[SyntaxTree.Branch] = this match {
    case Node(branch) => List(branch)
    case compound: CompoundPropBankArgument => compound.subArgs.flatMap(_.allBranches)
  }
  def getTopConstituents[A: HasIndex : HasToken](tree: SyntaxTree[A]): List[SyntaxTree[A]] = {
    val allConstituents = allBranches.map { branch =>
      if(branch.constituentHeight == 0) {
        val parent = tree.getSubtree(branch.copy(constituentHeight = 1))
        if(parent.size == 1) parent
        else tree.getSubtree(branch)
      } else tree.getSubtree(branch)
    }

    allConstituents.filter {
      case SyntaxTree.Node(TracedNonterminal(label, index), _) =>
        val indexMarker = s"*-$index"
        // only keep if no other constituent contains a trace of this one.
        !allConstituents.exists(_.toList.exists(_.token.endsWith(indexMarker)))
      case _ => true
    }
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

  val TracedNonterminal = "(.*)-[0-9]+".r
}
