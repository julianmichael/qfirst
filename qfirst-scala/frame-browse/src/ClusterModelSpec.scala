package qfirst.frame.browse

import io.circe.generic.JsonCodec

@JsonCodec sealed trait ClusterModelSpec {
  import ClusterModelSpec._
  override def toString = this match {
    case SyntF => "SyntF"
    case HumQQ => "HUM-QQ"
  }

  def specString: String = this match {
    case SyntF => "arg/s->noop"
    case HumQQ => "arg/qent+dv"
  }

}
object ClusterModelSpec {
  case object SyntF extends ClusterModelSpec
  case object HumQQ extends ClusterModelSpec

  def all = List(SyntF, HumQQ)

  def fromString(x: String): Option[ClusterModelSpec] = all.find(_.toString == x)
}
