package qfirst.frame

import qfirst.frame.clustering._
import qfirst.frame.util.Vocab

import cats.Order
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

case class ArgumentModel private (
  lossTerms: Map[ArgumentModel.LossTerm, Double]
) {

  override def toString: String = ArgumentModel.toString(this)

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
    features: Features[VerbType, Arg],
    renderVerbType: VerbType => String)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]] = {
    import ArgumentModel._
    val model = MLMEntropy("masked")
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      allVerbArgSets <- features.verbArgSets.get
      allArgs <- features.args.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        Log.info(renderVerbType(verbType)) >> {
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

object ArgumentModel {

  def apply(terms: (LossTerm, Double)*): ArgumentModel = {
    val total = terms.map(_._2).sum
    val termMap = terms.map { case (term, weight) =>
      term -> (scala.math.round(weight / total * 100.0) / 100.0)
    }.toMap
    require(terms.size == termMap.size)
    new ArgumentModel(termMap)
  }

  val termIndex = List[(LossTerm, String)](
    QuestionEntropy -> "qent"
  ) ++ List("masked", "symm_both", "symm_left", "symm_right").map(mode =>
    MLMEntropy(mode) -> s"mlm_$mode",
  )
  val termToString = termIndex.toMap
  val stringToTerm = termIndex.map(_.swap).toMap

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[ArgumentModel] = {
    if(stringToTerm.contains(x)) Some(ArgumentModel(stringToTerm(x) -> 1.0))
    else scala.util.Try {
      val termStrings = x.split("\\+").toList
      val terms = termStrings.map { term =>
        if(stringToTerm.contains(term)) (stringToTerm(term) -> 1.0)
        else {
          val components = term.split("\\*")
          stringToTerm(components(0)) -> components(1).toDouble
        }
      }
      ArgumentModel(terms: _*)
    }.toOption
  }
  def toString(model: ArgumentModel): String = {
    model.lossTerms.toList.map { case (term, weight) =>
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
      new DirichletMAPClusteringDense(argMLMFeatures, features.argMLMFeatureDim, 0.01f),
      new MinEntropyClusteringDense(argMLMFeatures, features.argMLMFeatureDim)
    )
  }
}

