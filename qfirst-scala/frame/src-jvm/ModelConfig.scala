package qfirst.frame

import io.circe.generic.JsonCodec

@JsonCodec sealed trait ModelConfig {
  import ModelConfig._
  override def toString = this match {
    case Joint => "joint"
    case SingleCluster => "single"
    case EntropyOnly => "entropy"
    case ELMoOnly => "elmo"
    case i @ Interpolated(entropyLambda) =>
      f"entropy$entropyLambda%.2f-elmo${i.elmoLambda}%.2f"
  }
}
object ModelConfig {
  case object Joint extends ModelConfig
  sealed trait NonJoint extends ModelConfig
  case object SingleCluster extends NonJoint
  case object EntropyOnly extends NonJoint
  case object ELMoOnly extends NonJoint
  // case object SetOnly extends ModelConfig
  case class Interpolated(
    entropyLambda: Double
  ) extends NonJoint {
    val elmoLambda: Double = 1.0 - entropyLambda
    def lambdas = List(entropyLambda, elmoLambda)
    assert(lambdas.forall(l => 0.0 < l && l < 1.0))
  }
  object DoubleMatch {
    def unapply(s: String) = scala.util.Try(s.toDouble).toOption
  }
  def fromString(s: String) = s match {
    case "joint" => Some(Joint)
    case "single" => Some(SingleCluster)
    case "entropy" => Some(EntropyOnly)
    case "elmo" => Some(ELMoOnly)
    case DoubleMatch(d) => Some(Interpolated(d))
    case _ => None
  }
}
