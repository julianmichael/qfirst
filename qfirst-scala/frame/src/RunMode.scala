package qfirst.paraphrase

import io.circe.generic.JsonCodec

@JsonCodec sealed trait RunMode {
  import RunMode._
  override def toString = this match {
    case Sanity => "sanity"
    case Dev => "dev"
    case Test => "test"
  }
  def devOrTest: String = this match {
    case Sanity => "dev"
    case Dev => "dev"
    case Test => "test"
  }
  def sanity = this match {
    case Sanity => true
    case _ => false
  }
  def test = this match {
    case Test => true
    case _ => false
  }
}
object RunMode {
  case object Sanity extends RunMode
  case object Dev extends RunMode
  case object Test extends RunMode

  def fromString(s: String) = s match {
    case "sanity" => Some(Sanity)
    case "dev" => Some(Dev)
    case "test" => Some(Test)
    case _ => None
  }
}
