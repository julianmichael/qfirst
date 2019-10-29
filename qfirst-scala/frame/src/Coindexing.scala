package qfirst.frame

import qfirst.clause.ArgStructure
import qfirst.frame.models.CompleteLinkageClustering

import qasrl.ArgumentSlot

object Coindexing {
  def getCoindexingTree(
    clauseTemplates: Set[ArgStructure],
    coindexingScores: Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double]
  ) = {
    val clausalQVocab = Vocab.make(
      clauseTemplates.flatMap { clauseTemplate =>
        getArgumentSlotsForClauseTemplate(clauseTemplate).toList
          .map(slot => clauseTemplate -> slot)
      }.toSet
    )
    val indices = clausalQVocab.indices
    val fuzzyEquivMatrix = Array.ofDim[Double](clausalQVocab.size, clausalQVocab.size)
    for(i <- indices; j <- indices) {
      val (qi, qj) = clausalQVocab.getItem(i) -> clausalQVocab.getItem(j)
      val score = 1.0 - (
        if(i == j) 1.0 // reflexive
        else if(qi._1 == qj._1) 0.0 // prohibit coindexing within a clause
        else {
          val resOpt = coindexingScores.get(qi -> qj).orElse(coindexingScores.get(qj -> qi))
          resOpt.getOrElse {
            println(s"XXX: $qi \t $qj")
            0.5 // shouldn't happen
          }
        }
      )
      fuzzyEquivMatrix(i)(j) = score
    }
    val (mergeTree, _) = CompleteLinkageClustering.runAgglomerativeClustering(indices, fuzzyEquivMatrix)
    val coindexingTree = mergeTree.map(clausalQVocab.getItem)
    coindexingTree
  }
}
