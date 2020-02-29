package qfirst.frame

import io.circe.generic.JsonCodec

@JsonCodec sealed trait RunMode {
  import RunMode._
  override def toString = this match {
    case Sanity => "sanity"
    case Dev => "dev"
    case Test => "test"
  }
  def input: String = this match {
    case Sanity => "dev"
    case Dev => "train"
    case Test => "train"
  }
  def eval: String = this match {
    case Sanity => "dev"
    case Dev => "dev"
    case Test => "test"
  }
  def isSanity = this match {
    case Sanity => true
    case _ => false
  }
  def isDev = this match {
    case Dev => true
    case _ => false
  }
  def isTest = this match {
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
