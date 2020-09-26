package qfirst.frame.browse

import qfirst.frame.ArgumentId
import qfirst.frame.QuestionTemplate
import qfirst.frame.SentenceInfo
import qfirst.frame.VerbId

import jjm.DotKleisli

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

object FeatureService extends FeatureServiceCompanionPlatformExtensions {
  // implicit class RichFeatureService[F[_], VerbType, Arg](fs: FeatureService[F, VerbType, Arg]) extends AnyVal {
  //   def questionDists(verbType: VerbType): F[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]] = {
  //     val req: FeatureReq[VerbType, Arg] { type Out = Map[ArgumentId[Arg], Map[QuestionTemplate, Double]] } =
  //       FeatureReq(FeatureKey.QuestionDists[Arg](), verbType)
  //     fs.apply(req)
  //   }
  // }
}

@JsonCodec sealed trait FeatureReq[VerbType, Arg] { type Out }
object FeatureReq {
  case class Sentence[VerbType, Arg](sentenceId: String) extends FeatureReq[VerbType, Arg] {
    type Out = SentenceInfo[VerbType, Arg]
  }
  case class Sentences[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Set[String]
  }
  case class QuestionDists[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]
  }
  // case class ArgMLMDist[Arg](pattern: String) extends ArgFeatureKey[Arg, Map[QuestionTemplate, Double]]
  // case class VerbMLMDist(pattern: String) extends VerbFeatureKey[Map[QuestionTemplate, Double]]

  implicit def featureReqDotEncoder[VerbType: Encoder, Arg: Encoder] = new DotKleisli[Encoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Encoder[req.Out] = req match {
      case Sentence(_) => implicitly[Encoder[SentenceInfo[VerbType, Arg]]]
          .asInstanceOf[Encoder[req.Out]]
      case Sentences(_) => implicitly[Encoder[Set[String]]]
          .asInstanceOf[Encoder[req.Out]]
      case QuestionDists(_) => implicitly[Encoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
    }
  }

  implicit def featureReqDotDecoder[VerbType: Decoder, Arg: Decoder] = new DotKleisli[Decoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Decoder[req.Out] = req match {
      case Sentence(_) => implicitly[Decoder[SentenceInfo[VerbType, Arg]]]
          .asInstanceOf[Decoder[req.Out]]
      case Sentences(_) => implicitly[Decoder[Set[String]]]
          .asInstanceOf[Decoder[req.Out]]
      case QuestionDists(_) => implicitly[Decoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
    }
  }
}
