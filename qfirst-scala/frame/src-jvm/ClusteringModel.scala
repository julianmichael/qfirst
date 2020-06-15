package qfirst.frame

import qfirst.frame.clustering._
import qfirst.frame.features.Features
import qfirst.frame.util.Vocab

import cats.Order
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.implicits._

import jjm.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

sealed trait ClusteringModel
object ClusteringModel {
  def fromString(x: String): Option[ClusteringModel] = {
    if(x.startsWith("arg/")) {
      BaselineArgumentModel.fromString(x.drop("arg/".length))
        .orElse(FullArgumentModel.fromString(x.drop("arg/".length)))
    } else if(x.startsWith("verb/")) VerbModel.fromString(x.drop("verb/".length))
    else None
  }
}

sealed trait ArgumentModel extends ClusteringModel {
  def getArgumentClusters[VerbType, Arg : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]
}
object ArgumentModel {
  def fromString(x: String): Option[ArgumentModel] =
    ClusteringModel.fromString(x).collect {
      case model: ArgumentModel => model
    }
}

case class BaselineArgumentModel(setting: String) extends ArgumentModel {

  def getArgumentClusters[VerbType, Arg : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      allVerbArgSets <- features.verbArgSets.get
      allArgs <- features.args.get
      argSyntacticFunctionsOpt <- {
        if(setting == "syntf+") features.argSyntacticFunctionsConverted.get.map(Some(_))
        else if(setting == "syntf") features.argSyntacticFunctions.get.map(Some(_))
        else if(setting == "random") IO.pure(None)
        else ??? // should never happen
      }
      syntFuncCountsOpt <- argSyntacticFunctionsOpt.traverse(argSyntacticFunctions =>
        allArgs.toList.infoBarFoldMapM("Counting syntactic functions") { case (verbType, argIds) =>
          IO(
            argIds.toList.foldMap(argId =>
              Map(argSyntacticFunctions(verbType)(argId) -> 1)
            )
          )
        }
      )
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering arguments") { case (verbType, verbs) =>
        Log.info(features.renderVerbType(verbType)) >> {
          val getOrderedVerbSets = (argSyntacticFunctionsOpt, syntFuncCountsOpt).mapN { (argSyntacticFunctions, syntFuncCounts) =>
            val argSyntFuncsForVerb = argSyntacticFunctions(verbType)
            (argIds: Set[ArgumentId[Arg]]) => argIds.groupBy(argSyntFuncsForVerb)
              .toVector.sortBy(p => syntFuncCounts(p._1)).map(_._2)
          }.getOrElse {
            // random baseline
            (argIds: Set[ArgumentId[Arg]]) => scala.util.Random.shuffle(
              argIds.toVector.map(Set(_))
            )
          }

          // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
          NonEmptyVector.fromVector(allArgs(verbType).toVector).traverse { args =>
            IO {
              verbType -> getOrderedVerbSets(args.toVector.toSet)
                .map(MergeTree.Leaf(0.0, _))
                .reduceLeft[MergeTree[Set[ArgumentId[Arg]]]](MergeTree.Merge(0.0, _, _))
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
  override def toString = BaselineArgumentModel.toString(this)
}
object BaselineArgumentModel {

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[BaselineArgumentModel] = x match {
    case x @ ("syntf" | "syntf+" | "random") => Some(BaselineArgumentModel(x))
    case _ => None
  }
  def toString(model: BaselineArgumentModel): String = {
    "arg/" + model.setting
  }
}

case class FullArgumentModel private (
  lossTerms: Map[FullArgumentModel.LossTerm, Double]
) extends ArgumentModel {

  override def toString: String = FullArgumentModel.toString(this)

  type FlatAlg[Arg] = FlatClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type AgglomAlg[Arg] = AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type AlgPair[Arg] = (FlatAlg[Arg], AgglomAlg[Arg])

  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit] = {
    lossTerms.keySet.toList.traverse(_.init(features)).void
  }

  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[AlgPair[Arg]] = {
    val termVec = lossTerms.toVector
    val lambdas = termVec.map(_._2)
    termVec.traverse(_._1.create(features, verbType): IO[AlgPair[Arg]])
      .map(_.unzip[FlatAlg[Arg], AgglomAlg[Arg]])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[ArgumentId[Arg]](flatAlgs.zip(lambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[ArgumentId[Arg]](agglomAlgs.zip(lambdas))
        (weightedFlatAlg, weightedAgglomAlg)
    }
  }

  def getArgumentClusters[VerbType, Arg : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      allVerbArgSets <- features.verbArgSets.get
      allArgs <- features.args.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering arguments") { case (verbType, verbs) =>
        Log.info(features.renderVerbType(verbType)) >> {
          // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
          NonEmptyVector.fromVector(allArgs(verbType).toVector).traverse { args =>
            this.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
              val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
              Clustering.runCombinedClustering(args, flatAlgorithm, agglomAlgorithm).map {
                case (argTree, _) => verbType -> argTree
              }
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
}

object FullArgumentModel {

  def apply(terms: (LossTerm, Double)*): FullArgumentModel = {
    val total = terms.map(_._2).sum
    val termMap = terms.map { case (term, weight) =>
      term -> (scala.math.round(weight / total * 100.0) / 100.0)
    }.toMap
    require(terms.size == termMap.size)
    new FullArgumentModel(termMap)
  }

  val termIndex = List[(LossTerm, String)](
    QuestionEntropy -> "qent",
    SyntacticFunction -> "syntf",
  ) ++ List("masked", "symm_both", "symm_left", "symm_right").map(mode =>
    MLMEntropy(mode) -> s"mlm_$mode",
  )
  val termToString = termIndex.toMap
  val stringToTerm = termIndex.map(_.swap).toMap

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[FullArgumentModel] = {
    if(stringToTerm.contains(x)) Some(FullArgumentModel(stringToTerm(x) -> 1.0))
    else scala.util.Try {
      val termStrings = x.split("\\+").toList
      val terms = termStrings.map { term =>
        if(stringToTerm.contains(term)) (stringToTerm(term) -> 1.0)
        else {
          val components = term.split("\\*")
          stringToTerm(components(0)) -> components(1).toDouble
        }
      }
      FullArgumentModel(terms: _*)
    }.toOption
  }
  def toString(model: FullArgumentModel): String = {
    "arg/" + model.lossTerms.toList.map { case (term, weight) =>
      if(weight == 1.0) termToString(term)
      else f"${termToString(term)}%s*${weight}%.2f"
    }.sorted.mkString("+")
  }

  sealed trait LossTerm {

    type FlatParam
    type AgglomParam

    def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]

    def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ): IO[
      (FlatClusteringAlgorithm { type Index = ArgumentId[Arg]; type ClusterParam = FlatParam },
       AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg]; type ClusterParam = AgglomParam })
    ]
  }

  case object QuestionEntropy extends LossTerm {
    type FlatParam = DenseMultinomial
    type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argQuestionDists.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      questionDists <- features.argQuestionDists.get.map(_.apply(verbType))
      questionVocab = Vocab.make(questionDists.value.values.toList.foldMap(_.keySet))
      // TODO add tf-idf transform?
      indexedInstances = questionDists.value.map { case (argId, questionDist) =>
        argId -> questionDist.map { case (q, p) => questionVocab.getIndex(q) -> p }
      }
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, questionVocab.size, 0.01),
      new MinEntropyClusteringSparse(indexedInstances, questionVocab.size)
    )
  }

  case object SyntacticFunction extends LossTerm {
    type FlatParam = DenseMultinomial
    type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argSyntacticFunctions.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      argSyntacticFunctions <- features.argSyntacticFunctions.get.map(_.apply(verbType))
      syntFuncVocab = Vocab.make(argSyntacticFunctions.value.values.toSet)
      // TODO add tf-idf transform?
      indexedInstances = argSyntacticFunctions.value.map { case (argId, syntFunc) =>
        argId -> Map(syntFuncVocab.getIndex(syntFunc) -> 1.0)
      }
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, syntFuncVocab.size, 0.01),
      new MinEntropyClusteringSparse(indexedInstances, syntFuncVocab.size)
    )
  }

  // TODO
  // case object MaximumEntropy extends LossTerm {
  //   type FlatParam = DenseMultinomial
  //   type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argSyntacticFunctions.get.as(())

  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = for {
  //     argSyntacticFunctions <- features.argSyntacticFunctions.get.map(_.apply(verbType))
  //     syntFuncVocab = Vocab.make(argSyntacticFunctions.value.values.toSet)
  //     // TODO add tf-idf transform?
  //     indexedInstances = argSyntacticFunctions.value.map { case (argId, syntFunc) =>
  //       argId -> Map(syntFuncVocab.getIndex(syntFunc) -> 1.0)
  //     }
  //   } yield (
  //     new DirichletMAPClusteringSparse(indexedInstances, syntFuncVocab.size, 0.01),
  //     new MinEntropyClusteringSparse(indexedInstances, syntFuncVocab.size)
  //   )
  // }

  import breeze.linalg.DenseVector

  case class MLMEntropy(mode: String) extends LossTerm {
    type FlatParam = DenseVector[Float]
    type AgglomParam = MinEntropyClusteringDense.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.getArgMLMFeatures(mode).get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      argMLMFeatures <- features.getArgMLMFeatures(mode).get.map(_.apply(verbType))
      // TODO add tf-idf transform?
    } yield (
      new DirichletMAPClusteringDense(argMLMFeatures, features.mlmFeatureDim, 0.01f),
      new MinEntropyClusteringDense(argMLMFeatures, features.mlmFeatureDim)
    )
  }
}

case class VerbModel private (
  lossTerms: Map[VerbModel.LossTerm, Double]
) extends ClusteringModel {

  type Out = MergeTree[Set[VerbId]]

  override def toString: String = VerbModel.toString(this)

  type FlatAlg = FlatClusteringAlgorithm { type Index = VerbId }
  type AgglomAlg = AgglomerativeClusteringAlgorithm { type Index = VerbId }
  type AlgPair = (FlatAlg, AgglomAlg)

  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit] = {
    lossTerms.keySet.toList.traverse(_.init(features)).void
  }

  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[AlgPair] = {
    val termVec = lossTerms.toVector
    val lambdas = termVec.map(_._2)
    termVec.traverse(_._1.create(features, verbType): IO[AlgPair])
      .map(_.unzip[FlatAlg, AgglomAlg])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[VerbId](flatAlgs.zip(lambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[VerbId](agglomAlgs.zip(lambdas))
        (weightedFlatAlg, weightedAgglomAlg)
    }
  }

  def getVerbClusters[VerbType, Arg](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[VerbId]]]] = {
    import VerbModel._
    val model = MLMEntropy("masked")
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      allVerbArgSets <- features.verbArgSets.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        val verbIds = verbs.keySet
        Log.info(features.renderVerbType(verbType)) >> {
          // TODO I don't think any of these should be empty
          NonEmptyVector.fromVector(verbIds.toVector).traverse { args =>
            this.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
              val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
              Clustering.runCombinedClustering(args, flatAlgorithm, agglomAlgorithm).map {
                case (verbTree, _) => verbType -> verbTree
              }
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
}

object VerbModel {

  def apply(terms: (LossTerm, Double)*): VerbModel = {
    val total = terms.map(_._2).sum
    val termMap = terms.map { case (term, weight) =>
      term -> (scala.math.round(weight / total * 100.0) / 100.0)
    }.toMap
    require(terms.size == termMap.size)
    new VerbModel(termMap)
  }

  val termIndex = List[(LossTerm, String)](
    // QuestionEntropy -> "qent"
  ) ++ List("masked", "symm_both", "symm_left", "symm_right").map(mode =>
    MLMEntropy(mode) -> s"mlm_$mode",
  )
  val termToString = termIndex.toMap
  val stringToTerm = termIndex.map(_.swap).toMap

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[VerbModel] = {
    if(stringToTerm.contains(x)) Some(VerbModel(stringToTerm(x) -> 1.0))
    else scala.util.Try {
      val termStrings = x.split("\\+").toList
      val terms = termStrings.map { term =>
        if(stringToTerm.contains(term)) (stringToTerm(term) -> 1.0)
        else {
          val components = term.split("\\*")
          stringToTerm(components(0)) -> components(1).toDouble
        }
      }
      VerbModel(terms: _*)
    }.toOption
  }
  def toString(model: VerbModel): String = {
    "verb/" + model.lossTerms.toList.map { case (term, weight) =>
      if(weight == 1.0) termToString(term)
      else f"${termToString(term)}%s*${weight}%.2f"
    }.sorted.mkString("+")
  }

  sealed trait LossTerm {

    type FlatParam
    type AgglomParam

    def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]

    def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ): IO[
      (FlatClusteringAlgorithm { type Index = VerbId; type ClusterParam = FlatParam },
       AgglomerativeClusteringAlgorithm { type Index = VerbId; type ClusterParam = AgglomParam })
    ]
  }

  import breeze.linalg.DenseVector

  case class MLMEntropy(mode: String) extends LossTerm {
    type FlatParam = DenseVector[Float]
    type AgglomParam = MinEntropyClusteringDense.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.getVerbMLMFeatures(mode).get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      verbMLMFeatures <- features.getVerbMLMFeatures(mode).get.map(_.apply(verbType))
      // TODO add tf-idf transform?
    } yield (
      new DirichletMAPClusteringDense(verbMLMFeatures, features.mlmFeatureDim, 0.01f),
      new MinEntropyClusteringDense(verbMLMFeatures, features.mlmFeatureDim)
    )
  }
}

//   // case class Joint[P, Arg](
//   //   innerModel: ClusteringModel.ArgumentModel[P]
//   // ) extends ClusteringModel.JointModel[P] {
//   //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = innerModel.init(features)
//   //   override def create[VerbType, Arg](
//   //     features: Features[VerbType, Arg], verbType: VerbType
//   //   ) = {
//   //     // TODO make this a parameter // TODO actually FIX this .... see the alg. seems wrongo
//   //     val getLossPenalty = (numClusters: Int) => scala.math.pow(numClusters, 2.0) / 2.0

//   //     for {
//   //       argumentAlgorithm <- innerModel.create(features, verbType).map(_._2)
//   //       verbIdToArgIds <- features.verbArgSets.get.map(
//   //         _.apply(verbType).value.map { case (verbId, argIds) =>
//   //           // TODO: maybe do something to handle the case of no args for a verb...
//   //           // what should the clustering do in this case?
//   //           verbId -> NonEmptyVector.fromVector(argIds.toVector).get.map(arg => ArgumentId(verbId, arg))
//   //         }
//   //       )
//   //     } yield new JointAgglomerativeClusteringAlgorithm(
//   //       argumentAlgorithm,
//   //       verbIdToArgIds,
//   //       getLossPenalty
//   //     )
//   //   }
//   // }

//   case object VerbSqDist extends VerbModel[VectorMeanClustering.ClusterMean] {
//     override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.elmoVecs.get.as(())
//     override def create[VerbType, Instance](
//       features: Features[VerbType, Instance], verbType: VerbType
//     ) = for {
//       vectors <- features.elmoVecs.get.map(_.apply(verbType))
//     } yield new VectorMeanClustering(vectors.value)
//   }

//   case object AnswerEntropy extends FullArgumentModel[DenseMultinomial, MinEntropyClusteringSparse.ClusterMixture] {
//     override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
//       sentences <- features.sentences.get
//       tokenCounts <- features.argSpans.get
//     } yield ()

//     override def create[VerbType, Instance](
//       features: Features[VerbType, Instance], verbType: VerbType
//     ) = for {
//       // val pronouns = Set(
//       //   "he", "him", "it", "she", "her", "they", "them", "we", "us"
//       // ).map(_.lowerCase)
//       // val tokensToSkip = pronouns

//       // TODO TF-IDF thing

//       sentences <- features.sentences.get
//       tokenProbs <- features.argSpans.get.map(
//         _.apply(verbType).value.toList.foldMap { case (argId, spanScores) =>
//           val sentenceTokens = sentences.value(argId.verbId.sentenceId)
//           // val numTokens = spanSets.foldMap(_.foldMap(_.length))
//           // val singleCount = 1.0 / numTokens
//           val tokenPseudocounts = spanScores.toList.foldMap { case (span, score) =>
//             sentenceTokens.slice(span.begin, span.endExclusive)
//               .map(_.lowerCase)
//               .foldMap(t => Map(t -> score))
//             // .filter(t => !tokensToSkip.contains(t))
//           }
//           val total = tokenPseudocounts.values.sum
//           val normalizedTokenCounts = tokenPseudocounts.transform { case (_, s) => s / total }
//           Map(argId -> normalizedTokenCounts)
//         }
//       )
//       tokenVocab = Vocab.make(tokenProbs.unorderedFoldMap(_.keySet))
//       indexedTokenProbs = tokenProbs.map { case (qid, qTokenProbs) =>
//         qid -> qTokenProbs.map { case (tok, pcount) => tokenVocab.getIndex(tok) -> pcount }
//       }
//     } yield (
//       new DirichletMAPClusteringSparse(indexedTokenProbs, tokenVocab.size, 0.01),
//       new MinEntropyClusteringSparse(indexedTokenProbs, tokenVocab.size)
//     )
//   }
// }

// // object VerbClauseEntropy extends FrameInductionModel[MinEntropyClusteringSparse.ClusterMixture] {
// //   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.verbArgSets.get.as(())
// //   override def create[VerbType, Instance](
// //     features: Features[VerbType, Instance], verbType: VerbType
// //   ) = for {
// //     clauseCounts <- features.verbArgSets.get.map(
// //       _.apply(verbType).map {
// //         case (verbId, qaPairs) =>
// //           verbId -> qaPairs.keys.toList.foldMap { question =>
// //             Map(question.clauseTemplate -> 1.0)
// //           }
// //       }
// //     )
// //     clauseVocab = Vocab.make(clauseCounts.values.toList.foldMap(_.keySet))
// //     indexedClauseCounts = clauseCounts.map { case (verbId, counts) =>
// //       verbId -> counts.map { case (clause, count) =>
// //         clauseVocab.getIndex(clause) -> count
// //       }
// //     }
// //   } yield new MinEntropyClusteringSparse(indexedClauseCounts, clauseVocab.size)
// // }

// // import breeze.linalg.DenseVector

// // object AnswerNLL extends FrameInductionModel[MixtureOfStatesClustering.StateCounts] {
// //   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
// //     answerNLLInfo <- features.answerNLLs.get
// //   } yield ()

// //   import jjm.ling.en.InflectedForms
// //   import qasrl.ArgumentSlot
// //   import qasrl.Frame
// //   def getQuestionTemplate(templateQ: (ArgStructure, ArgumentSlot)): String = {
// //     val frame = Frame(
// //       verbInflectedForms = InflectedForms.generic,
// //       args = templateQ._1.args,
// //       tense = qasrl.PresentTense,
// //       isPerfect = false,
// //       isPassive = templateQ._1.isPassive,
// //       isNegated = false, isProgressive = false)
// //     frame.questionsForSlot(templateQ._2).head
// //   }

// //   override def create[VerbType, Instance](
// //     features: Features[VerbType, Instance], verbType: VerbType
// //   ) = for {
// //     answerNLLInfo <- features.answerNLLs.get.map(_.apply(verbType))
// //     templateQVocab = Vocab.make(
// //       answerNLLInfo.values.flatten
// //         .map(_._1)
// //         .map(getQuestionTemplate)
// //         .toSet
// //     )
// //     templateQVectorsByQid = answerNLLInfo.map { case (qid, qsWithNLLs) =>
// //       val qsWithNLLsMap = qsWithNLLs
// //         .groupBy(p => getQuestionTemplate(p._1))
// //         .map { case (k, nlls) => k -> nlls.map(_._2).min }
// //       qid -> MixtureOfStatesClustering.StateInstance(
// //         templateQVocab.getIndex(getQuestionTemplate(qid.question.template)),
// //         DenseVector.tabulate(templateQVocab.size)(i =>
// //           qsWithNLLsMap(templateQVocab.getItem(i))
// //         )
// //       )
// //     }
// //   } yield new MixtureOfStatesClustering[QuestionId](
// //     getInstance = templateQVectorsByQid,
// //     vocabSize = templateQVocab.size
// //   )
// // }
