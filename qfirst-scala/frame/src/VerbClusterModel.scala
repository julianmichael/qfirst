package qfirst.frame

import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class VerbClusterModel[VerbType](
  verbType: VerbType,
  verbClusterTree: MergeTree[VerbId],
  questionClusterTree: MergeTree[QuestionId]
) {
  val clauseSets = questionClusterTree.unorderedFoldMap(qid =>
    Map(qid.verbId -> Set(qid.question.clauseTemplate))
  )
  val numVerbInstances = verbClusterTree.size
}
object VerbClusterModel
