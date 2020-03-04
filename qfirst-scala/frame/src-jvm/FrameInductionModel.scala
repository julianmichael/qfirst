package qfirst.frame

import jjm.implicits._

import qfirst.frame.models._
import qfirst.clause.ArgStructure

import cats.effect.IO
import cats.data.NonEmptyVector
import cats.implicits._

sealed trait FrameInductionModel[I, P] {
  def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ): IO[AgglomerativeClusteringAlgorithm { type Index = I; type ClusterParam = P }]
}

case class Composite[I, P1, P2](
  spec1: (FrameInductionModel[I, P1], Double),
  spec2: (FrameInductionModel[I, P2], Double)
) extends FrameInductionModel[I, (P1, P2)] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = for {
    alg1 <- spec1._1.create(features, verbType)
    alg2 <- spec2._1.create(features, verbType)
  } yield new CompositeAgglomerativeClusteringAlgorithm {
    val _1 = alg1
    val _1Lambda = spec1._2
    val _2 = alg2
    val _2Lambda = spec2._2
  }
}

case class Joint[P](
  innerModel: FrameInductionModel[QuestionId, P]
) extends FrameInductionModel[VerbId, NonEmptyVector[(MergeTree[QuestionId], P)]] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = {
    // TODO make this a parameter
    val getLossPenalty = (numClusters: Int) => scala.math.pow(numClusters, 2.0) / 2.0

    for {
      questionAlgorithm <- innerModel.create(features, verbType)
      verbIdToQuestionIds <- features.instancesByQuestionId.full.get.map(
        _.apply(verbType).keys.toList
          .groupByNel(_.verbId)
          .map { case (verbId, qids) => verbId -> NonEmptyVector.of(qids.head, qids.tail: _*) }
      )
    } yield new JointAgglomerativeClusteringAlgorithm(
      questionAlgorithm,
      verbIdToQuestionIds,
      getLossPenalty
    )
  }
}

object VerbSqDist extends FrameInductionModel[VerbId, VectorMeanClustering.ClusterMean] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = for {
    vectors <- features.elmoVecs.full.get.map(
      _.apply(verbType).toList.foldMap { case (sid, verbs) =>
        NonMergingMap(
          verbs.value.map { case (verbIndex, vec) =>
            VerbId(sid, verbIndex) -> vec
          }
        )
      }
    )
  } yield new VectorMeanClustering(vectors.value)
}

object QuestionEntropy extends FrameInductionModel[QuestionId, MinEntropyClustering.ClusterMixture] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = for {
    questions <- features.instancesByQuestionId.full.get.map(
      _.apply(verbType).map { case (qid, _) => qid -> qid.question  }
    )
    questionVocab = Vocab.make(questions.values.toSet)
    // TODO add tf-idf transform?
    indexedQuestionFeatures = questions.map { case (qid, question) =>
      qid -> Map(questionVocab.getIndex(question) -> 1.0)
    }
  } yield new MinEntropyClustering(indexedQuestionFeatures, questionVocab.size)
}

object VerbClauseEntropy extends FrameInductionModel[VerbId, MinEntropyClustering.ClusterMixture] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = for {
    clauseCounts <- features.instancesByVerbId.full.get.map(
      _.apply(verbType).map {
        case (verbId, qaPairs) =>
          verbId -> qaPairs.keys.toList.foldMap { question =>
            Map(question.clauseTemplate -> 1.0)
          }
      }
    )
    clauseVocab = Vocab.make(clauseCounts.values.toList.foldMap(_.keySet))
    indexedClauseCounts = clauseCounts.map { case (verbId, counts) =>
      verbId -> counts.map { case (clause, count) =>
        clauseVocab.getIndex(clause) -> count
      }
    }
  } yield new MinEntropyClustering(indexedClauseCounts, clauseVocab.size)
}

object AnswerEntropy extends FrameInductionModel[QuestionId, MinEntropyClustering.ClusterMixture] {
  override def create[VerbType](
    features: Features[VerbType], verbType: VerbType
  ) = for {
    // val pronouns = Set(
    //   "he", "him", "it", "she", "her", "they", "them", "we", "us"
    // ).map(_.lowerCase)
    // val tokensToSkip = pronouns

    // TODO TF-IDF thing

    sentences <- features.sentences.full.get
    tokenCounts <- features.instancesByQuestionId.full.get.map(
      _.apply(verbType).toList.foldMap { case (qid, spanSets) =>
          val sentenceTokens = sentences.value(qid.verbId.sentenceId)
          val numTokens = spanSets.foldMap(_.foldMap(_.length))
          val singleCount = 1.0 / numTokens
          val tokenPseudocounts = spanSets.foldMap(
            _.foldMap(span =>
              sentenceTokens.slice(span.begin, span.endExclusive)
                .map(_.lowerCase)
                .foldMap(t => Map(t -> singleCount))
                // .filter(t => !tokensToSkip.contains(t))
            )
          )
          Map(qid -> tokenPseudocounts)
        }
    )

    tokenVocab = Vocab.make(tokenCounts.unorderedFoldMap(_.keySet))

    indexedTokenCounts = tokenCounts.map { case (qid, qTokenCounts) =>
      qid -> qTokenCounts.map { case (tok, pcount) => tokenVocab.getIndex(tok) -> pcount }
    }
  } yield new MinEntropyClustering(indexedTokenCounts, tokenVocab.size)
}
