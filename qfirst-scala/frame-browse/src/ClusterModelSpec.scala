package qfirst.frame.browse

import io.circe.generic.JsonCodec

@JsonCodec sealed trait ClusterModelSpec {
  import ClusterModelSpec._
  override def toString = this match {
    case SyntF => "SyntF"
    case SyntFLex => "SyntF + lex"
    case SyntFPassAct => "SyntF + pass->act"
    case SyntFAll => "SyntF + all rules"
    case HumQQ => "HUM-QQ"
    case HumQQLex => "HUM-QQ + lex"
  }

  def specString: String = this match {
    case SyntF => "arg/s->noop"
    case SyntFLex => "arg/mnds->noop"
    case SyntFPassAct  => "arg/S->noop"
    case SyntFAll => "arg/mndS->noop"
    case HumQQ => "arg/qent+dv"
    case HumQQLex => "arg/mnd->qent+dv"
  }

}
object ClusterModelSpec {
  case object SyntF extends ClusterModelSpec
  case object SyntFLex extends ClusterModelSpec
  case object SyntFPassAct extends ClusterModelSpec
  case object SyntFAll extends ClusterModelSpec
  case object HumQQ extends ClusterModelSpec
  case object HumQQLex extends ClusterModelSpec

  def all = List(SyntF, SyntFLex, SyntFPassAct, SyntFAll, HumQQ, HumQQLex)

  def fromString(x: String): Option[ClusterModelSpec] = all.find(_.toString == x)
}
