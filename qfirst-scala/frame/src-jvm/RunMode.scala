package qfirst.frame

import io.circe.generic.JsonCodec

import cats.effect.IO

@JsonCodec sealed trait RunMode {
  import RunMode._

  // run mode details / data choices etc. are defined here

  def get[A](data: RunData[A]): IO[A] = {
    if(isSanity) data.dev
    else data.train
  }

  def shouldEvaluate = isSanity || isTest

  override def toString = this match {
    case Sanity => "sanity"
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
