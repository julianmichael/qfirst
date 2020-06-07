package qfirst.frame

import qfirst.frame.util.Vocab

import jjm.implicits._

import qfirst.frame.clustering._
import qfirst.clause.ArgStructure

import cats.effect.IO
import cats.data.NonEmptyVector
import cats.implicits._

sealed trait ClusteringModel[Alg[_]] {
  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]
  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[Alg[Arg]]
}
object ClusteringModel {
  type ArgumentModel[FP, AP] = ClusteringModel[
    Lambda[A =>
      (FlatClusteringAlgorithm { type Index = ArgumentId[A]; type ClusterParam = FP },
       AgglomerativeClusteringAlgorithm { type Index = ArgumentId[A]; type ClusterParam = AP })
    ]]
  type VerbModel[P] = ClusteringModel[
    Lambda[A =>
      AgglomerativeClusteringAlgorithm { type Index = VerbId; type ClusterParam = P }
    ]]
  type JointModel[P] = ClusteringModel[
    Lambda[A =>
      AgglomerativeClusteringAlgorithm {
        type Index = VerbId;
        type ClusterParam = NonEmptyVector[(MergeTree[ArgumentId[A]], P)]
      }]]

  // case class Composite[F[_], P1[_], P2[_]](
  //   spec1: (ClusteringModel[F, P1], Double),
  //   spec2: (ClusteringModel[F, P2], Double)
  // ) extends ClusteringModel[F, Lambda[A => (P1[A], P2[A])]] {
  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) =
  //     spec1._1.init(features) >> spec2._1.init(features)
  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = for {
  //     alg1 <- spec1._1.create(features, verbType)
  //     alg2 <- spec2._1.create(features, verbType)
  //   } yield new CompositeAgglomerativeClusteringAlgorithm {
  //     val _1 = alg1
  //     val _1Lambda = spec1._2
  //     val _2 = alg2
  //     val _2Lambda = spec2._2
  //   }
  // }
  // object Composite {
  //   def argument[P1, P2](spec1: (ArgumentModel[P1], Double), spec2: (ArgumentModel[P2], Double)) =
  //     Composite[ArgumentId, Lambda[A => P1], Lambda[A => P2]](spec1, spec2)
  //   def verb[P1, P2](spec1: (VerbModel[P1], Double), spec2: (VerbModel[P2], Double)) =
  //     Composite[Lambda[A => VerbId], Lambda[A => P1], Lambda[A => P2]](spec1, spec2)
  //   def withJoint[P1, P2](spec1: (VerbModel[P1], Double), spec2: (JointModel[P2], Double)) =
  //     Composite[Lambda[A => VerbId], Lambda[A => P1], Lambda[A => NonEmptyVector[(MergeTree[ArgumentId[A]], P2)]]](spec1, spec2)

  // }

  // case class Joint[P, Arg](
  //   innerModel: ClusteringModel.ArgumentModel[P]
  // ) extends ClusteringModel.JointModel[P] {
  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = innerModel.init(features)
  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = {
  //     // TODO make this a parameter // TODO actually FIX this .... see the alg. seems wrongo
  //     val getLossPenalty = (numClusters: Int) => scala.math.pow(numClusters, 2.0) / 2.0

  //     for {
  //       argumentAlgorithm <- innerModel.create(features, verbType).map(_._2)
  //       verbIdToArgIds <- features.verbArgSets.get.map(
  //         _.apply(verbType).value.map { case (verbId, argIds) =>
  //           // TODO: maybe do something to handle the case of no args for a verb...
  //           // what should the clustering do in this case?
  //           verbId -> NonEmptyVector.fromVector(argIds.toVector).get.map(arg => ArgumentId(verbId, arg))
  //         }
  //       )
  //     } yield new JointAgglomerativeClusteringAlgorithm(
  //       argumentAlgorithm,
  //       verbIdToArgIds,
  //       getLossPenalty
  //     )
  //   }
  // }

  // case object QuestionEntropy extends ArgumentModel[DenseMultinomial, MinEntropyClusteringSparse.ClusterMixture] {
  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argQuestionDists.get.as(())
  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = for {
  //     // TODO
  //     questionDists <- features.argQuestionDists.get.map(_.apply(verbType))
  //     questionVocab = Vocab.make(questionDists.value.values.toList.foldMap(_.keySet))
  //     // TODO add tf-idf transform?
  //     indexedInstances = questionDists.value.map { case (argId, questionDist) =>
  //       argId -> questionDist.map { case (q, p) => questionVocab.getIndex(q) -> p }
  //     }
  //   } yield (
  //     new DirichletMAPClusteringSparse(indexedInstances, questionVocab.size, 0.01),
  //     new MinEntropyClusteringSparse(indexedInstances, questionVocab.size)
  //   )
  // }

  // import breeze.linalg.DenseVector

  // case class ArgMLMEntropy(mode: String) extends ArgumentModel[DenseVector[Float], MinEntropyClusteringDense.ClusterMixture] {
  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.getArgMLMFeatures(mode).get.as(())
  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = for {
  //     argMLMFeatures <- features.getArgMLMFeatures(mode).get.map(_.apply(verbType))
  //     // TODO add tf-idf transform?
  //   } yield (
  //     new DirichletMAPClusteringDense(argMLMFeatures, features.argMLMFeatureDim, 0.01f),
  //     new MinEntropyClusteringDense(argMLMFeatures, features.argMLMFeatureDim)
  //   )
  // }

  case object VerbSqDist extends VerbModel[VectorMeanClustering.ClusterMean] {
    override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.elmoVecs.get.as(())
    override def create[VerbType, Instance](
      features: Features[VerbType, Instance], verbType: VerbType
    ) = for {
      vectors <- features.elmoVecs.get.map(_.apply(verbType))
    } yield new VectorMeanClustering(vectors.value)
  }

  case object AnswerEntropy extends ArgumentModel[DenseMultinomial, MinEntropyClusteringSparse.ClusterMixture] {
    override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
      sentences <- features.sentences.get
      tokenCounts <- features.argSpans.get
    } yield ()

    override def create[VerbType, Instance](
      features: Features[VerbType, Instance], verbType: VerbType
    ) = for {
      // val pronouns = Set(
      //   "he", "him", "it", "she", "her", "they", "them", "we", "us"
      // ).map(_.lowerCase)
      // val tokensToSkip = pronouns

      // TODO TF-IDF thing

      sentences <- features.sentences.get
      tokenProbs <- features.argSpans.get.map(
        _.apply(verbType).value.toList.foldMap { case (argId, spanScores) =>
          val sentenceTokens = sentences.value(argId.verbId.sentenceId)
          // val numTokens = spanSets.foldMap(_.foldMap(_.length))
          // val singleCount = 1.0 / numTokens
          val tokenPseudocounts = spanScores.toList.foldMap { case (span, score) =>
            sentenceTokens.slice(span.begin, span.endExclusive)
              .map(_.lowerCase)
              .foldMap(t => Map(t -> score))
            // .filter(t => !tokensToSkip.contains(t))
          }
          val total = tokenPseudocounts.values.sum
          val normalizedTokenCounts = tokenPseudocounts.transform { case (_, s) => s / total }
          Map(argId -> normalizedTokenCounts)
        }
      )
      tokenVocab = Vocab.make(tokenProbs.unorderedFoldMap(_.keySet))
      indexedTokenProbs = tokenProbs.map { case (qid, qTokenProbs) =>
        qid -> qTokenProbs.map { case (tok, pcount) => tokenVocab.getIndex(tok) -> pcount }
      }
    } yield (
      new DirichletMAPClusteringSparse(indexedTokenProbs, tokenVocab.size, 0.01),
      new MinEntropyClusteringSparse(indexedTokenProbs, tokenVocab.size)
    )
  }
}

// object VerbClauseEntropy extends FrameInductionModel[MinEntropyClusteringSparse.ClusterMixture] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.verbArgSets.get.as(())
//   override def create[VerbType, Instance](
//     features: Features[VerbType, Instance], verbType: VerbType
//   ) = for {
//     clauseCounts <- features.verbArgSets.get.map(
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
//   } yield new MinEntropyClusteringSparse(indexedClauseCounts, clauseVocab.size)
// }

// import breeze.linalg.DenseVector

// object AnswerNLL extends FrameInductionModel[MixtureOfStatesClustering.StateCounts] {
//   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
//     answerNLLInfo <- features.answerNLLs.get
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
//     answerNLLInfo <- features.answerNLLs.get.map(_.apply(verbType))
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
