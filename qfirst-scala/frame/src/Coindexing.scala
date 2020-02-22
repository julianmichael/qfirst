package qfirst.frame

import cats.data.NonEmptyVector

import qfirst.clause.ArgStructure
import qfirst.frame.models.CompleteLinkageClustering

import qasrl.ArgumentSlot

object Coindexing {
  def getCoindexingTree(
    questionTemplates: Set[(ArgStructure, ArgumentSlot)],
    coindexingScores: Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double]
  ) = {
    val clausalQVocab = Vocab.make(questionTemplates)
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
            if(!qi._2.isInstanceOf[qasrl.Adv] && !qj._2.isInstanceOf[qasrl.Adv]) {
              println(s"XXX: $qi \t $qj")
            } // otherwise, shouldn't happen. but of course we need to just get all the coindexing done...
            0.05
          }
        }
      )
      fuzzyEquivMatrix(i)(j) = score
    }
    val (mergeTree, _) = new CompleteLinkageClustering(fuzzyEquivMatrix)
      .runFullAgglomerativeClustering(NonEmptyVector.fromVector(indices).get)
    val coindexingTree = mergeTree.map(clausalQVocab.getItem)
    coindexingTree
  }
}
