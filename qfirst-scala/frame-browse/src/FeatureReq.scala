package qfirst.frame.browse

import qfirst.frame.ArgumentId
import qfirst.frame.QuestionTemplate
import qfirst.frame.VerbId

import jjm.DotKleisli

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

@JsonCodec sealed trait FeatureKey[Arg] { type Out }
object FeatureKey {
  sealed trait VerbFeatureKey[A, Arg] extends FeatureKey[Arg] {
    type Out = Map[VerbId, A]
  }
  sealed trait ArgFeatureKey[Arg, A] extends FeatureKey[Arg] {
    type Out = Map[ArgumentId[Arg], A]
  }
  case class QuestionDist[Arg]() extends ArgFeatureKey[Arg, Map[QuestionTemplate, Double]]
  // case class ArgMLMDist[Arg](pattern: String) extends ArgFeatureKey[Arg, Map[QuestionTemplate, Double]]
  // case class VerbMLMDist(pattern: String) extends VerbFeatureKey[Map[QuestionTemplate, Double]]
}

@JsonCodec case class FeatureReq[VerbType, Arg](
  key: FeatureKey[Arg],
  verbType: VerbType) {
  type Out = key.Out
}
object FeatureReq {
  import FeatureKey._

  implicit def featureReqDotEncoder[VerbType, Arg: Encoder] = new DotKleisli[Encoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Encoder[req.Out] = req.key match {
      case QuestionDist() => implicitly[Encoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
    }
  }

  implicit def featureReqDotDecoder[VerbType, Arg: Decoder] = new DotKleisli[Decoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Decoder[req.Out] = req.key match {
      case QuestionDist() => implicitly[Decoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
    }
  }
}
