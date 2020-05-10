package qfirst.frame

import jjm.implicits._

import qfirst.frame.models._
import qfirst.clause.ArgStructure

import cats.effect.IO
import cats.data.NonEmptyVector
import cats.implicits._

// sealed trait VerbClusteringModel[P] {
//   def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]
//   def create[VerbType, Arg](
//     features: Features[VerbType, Arg], verbType: VerbType
//   ): IO[AgglomerativeClusteringAlgorithm { type Index = VerbId; type ClusterParam = P }]
// }

// sealed trait RoleClusteringModel[P] {
//   def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]
//   def create[VerbType, Arg](
//     features: Features[VerbType, Arg], verbType: VerbType
//   ): IO[AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg]; type ClusterParam = P }]
// }

sealed trait ClusteringModel[I[_], P[_]] {
  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]
  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[AgglomerativeClusteringAlgorithm { type Index = I[Arg]; type ClusterParam = P[Arg] }]
}
object ClusteringModel {
  type VerbModel[P] = ClusteringModel[Lambda[A => VerbId], Lambda[A => P]]
  type ArgumentModel[P] = ClusteringModel[ArgumentId, Lambda[A => P]]
  type JointModel[P] = ClusteringModel[
    Lambda[A => VerbId],
    Lambda[A => NonEmptyVector[(MergeTree[ArgumentId[A]], P)]]
  ]

  case class Composite[F[_], P1[_], P2[_]](
    spec1: (ClusteringModel[F, P1], Double),
    spec2: (ClusteringModel[F, P2], Double)
  ) extends ClusteringModel[F, Lambda[A => (P1[A], P2[A])]] {
    override def init[VerbType, Arg](features: Features[VerbType, Arg]) =
      spec1._1.init(features) >> spec2._1.init(features)
    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
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

  case class Joint[P, Arg](
    innerModel: ClusteringModel.ArgumentModel[P]
  ) extends ClusteringModel.JointModel[P] {
    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = innerModel.init(features)
    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = {
      // TODO make this a parameter
      val getLossPenalty = (numClusters: Int) => scala.math.pow(numClusters, 2.0) / 2.0

      for {
        argumentAlgorithm <- innerModel.create(features, verbType)
        verbIdToArgIds <- features.verbArgSets.full.get.map(
          _.apply(verbType).value.map { case (verbId, argIds) =>
            // TODO: maybe do something to handle the case of no args for a verb...
            // what should the clustering do in this case?
            verbId -> NonEmptyVector.fromVector(argIds.toVector).get.map(arg => ArgumentId(verbId, arg))
          }
        )
      } yield new JointAgglomerativeClusteringAlgorithm(
        argumentAlgorithm,
        verbIdToArgIds,
        getLossPenalty
      )
    }
  }

  object QuestionEntropy extends ArgumentModel[MinEntropyClustering.ClusterMixture] {
    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argQuestionDists.full.get.as(())
    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      // TODO
      questionDists <- features.argQuestionDists.full.get.map(_.apply(verbType))
      questionVocab = Vocab.make(questionDists.value.values.toList.foldMap(_.keySet))
      // TODO add tf-idf transform?
      indexedInstances = questionDists.value.map { case (argId, questionDist) =>
        argId -> questionDist.map { case (q, p) => questionVocab.getIndex(q) -> p }
      }
    } yield new MinEntropyClustering(indexedInstances, questionVocab.size)
  }
}


// object VerbSqDist extends FrameInductionModel[VectorMeanClustering.ClusterMean] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.elmoVecs.full.get.as(())
//   override def create[VerbType, Instance](
//     features: Features[VerbType, Instance], verbType: VerbType
//   ) = for {
//     vectors <- features.elmoVecs.full.get.map(
//       _.apply(verbType).toList.foldMap { case (sid, verbs) =>
//         NonMergingMap(
//           verbs.value.map { case (verbIndex, vec) =>
//             VerbId(sid, verbIndex) -> vec
//           }
//         )
//       }
//     )
//   } yield new VectorMeanClustering(vectors.value)
// }

// object VerbClauseEntropy extends FrameInductionModel[MinEntropyClustering.ClusterMixture] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.verbArgSets.full.get.as(())
//   override def create[VerbType, Instance](
//     features: Features[VerbType, Instance], verbType: VerbType
//   ) = for {
//     clauseCounts <- features.verbArgSets.full.get.map(
//       _.apply(verbType).map {
//         case (verbId, qaPairs) =>
//           verbId -> qaPairs.keys.toList.foldMap { question =>
//             Map(question.clauseTemplate -> 1.0)
//           }
//       }
//     )
//     clauseVocab = Vocab.make(clauseCounts.values.toList.foldMap(_.keySet))
//     indexedClauseCounts = clauseCounts.map { case (verbId, counts) =>
//       verbId -> counts.map { case (clause, count) =>
//         clauseVocab.getIndex(clause) -> count
//       }
//     }
//   } yield new MinEntropyClustering(indexedClauseCounts, clauseVocab.size)
// }

// object AnswerEntropy extends FrameInductionModel[MinEntropyClustering.ClusterMixture] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
//     sentences <- features.sentences.full.get
//     tokenCounts <- features.instancesByQuestionId.full.get
//   } yield ()

//   override def create[VerbType, Instance](
//     features: Features[VerbType, Instance], verbType: VerbType
//   ) = for {
//     // val pronouns = Set(
//     //   "he", "him", "it", "she", "her", "they", "them", "we", "us"
//     // ).map(_.lowerCase)
//     // val tokensToSkip = pronouns

//     // TODO TF-IDF thing

//     sentences <- features.sentences.full.get
//     tokenCounts <- features.instancesByQuestionId.full.get.map(
//       _.apply(verbType).toList.foldMap { case (qid, spanSets) =>
//           val sentenceTokens = sentences.value(qid.verbId.sentenceId)
//           val numTokens = spanSets.foldMap(_.foldMap(_.length))
//           val singleCount = 1.0 / numTokens
//           val tokenPseudocounts = spanSets.foldMap(
//             _.foldMap(span =>
//               sentenceTokens.slice(span.begin, span.endExclusive)
//                 .map(_.lowerCase)
//                 .foldMap(t => Map(t -> singleCount))
//                 // .filter(t => !tokensToSkip.contains(t))
//             )
//           )
//           Map(qid -> tokenPseudocounts)
//         }
//     )

//     tokenVocab = Vocab.make(tokenCounts.unorderedFoldMap(_.keySet))

//     indexedTokenCounts = tokenCounts.map { case (qid, qTokenCounts) =>
//       qid -> qTokenCounts.map { case (tok, pcount) => tokenVocab.getIndex(tok) -> pcount }
//     }
//   } yield new MinEntropyClustering(indexedTokenCounts, tokenVocab.size)
// }

// import breeze.linalg.DenseVector

// object AnswerNLL extends FrameInductionModel[MixtureOfStatesClustering.StateCounts] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
//     answerNLLInfo <- features.answerNLLs.full.get
//   } yield ()

//   import jjm.ling.en.InflectedForms
//   import qasrl.ArgumentSlot
//   import qasrl.Frame
//   def getQuestionTemplate(templateQ: (ArgStructure, ArgumentSlot)): String = {
//     val frame = Frame(
//       verbInflectedForms = InflectedForms.generic,
//       args = templateQ._1.args,
//       tense = qasrl.PresentTense,
//       isPerfect = false,
//       isPassive = templateQ._1.isPassive,
//       isNegated = false, isProgressive = false)
//     frame.questionsForSlot(templateQ._2).head
//   }

//   override def create[VerbType, Instance](
//     features: Features[VerbType, Instance], verbType: VerbType
//   ) = for {
//     answerNLLInfo <- features.answerNLLs.full.get.map(_.apply(verbType))
//     templateQVocab = Vocab.make(
//       answerNLLInfo.values.flatten
//         .map(_._1)
//         .map(getQuestionTemplate)
//         .toSet
//     )
//     templateQVectorsByQid = answerNLLInfo.map { case (qid, qsWithNLLs) =>
//       val qsWithNLLsMap = qsWithNLLs
//         .groupBy(p => getQuestionTemplate(p._1))
//         .map { case (k, nlls) => k -> nlls.map(_._2).min }
//       qid -> MixtureOfStatesClustering.StateInstance(
//         templateQVocab.getIndex(getQuestionTemplate(qid.question.template)),
//         DenseVector.tabulate(templateQVocab.size)(i =>
//           qsWithNLLsMap(templateQVocab.getItem(i))
//         )
//       )
//     }
//   } yield new MixtureOfStatesClustering[QuestionId](
//     getInstance = templateQVectorsByQid,
//     vocabSize = templateQVocab.size
//   )
// }
