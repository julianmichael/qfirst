package freelog

import cats.Order
import cats.Show
import cats.implicits._

sealed trait LogLevel {
  import LogLevel._
  def index = this match {
    case Debug => 0
    case Trace => 1
    case Info  => 2
    case Warn  => 3
    case Error => 4
  }

  override def toString = this match {
    case Debug => "DEBUG"
    case Trace => "TRACE"
    case Info  => "INFO"
    case Warn  => "WARN"
    case Error => "ERROR"
  }
}
object LogLevel {
  case object Debug extends LogLevel
  case object Trace extends LogLevel
  case object Info  extends LogLevel
  case object Warn  extends LogLevel
  case object Error extends LogLevel

  def fromString(s: String): Option[LogLevel] = s match {
    case "DEBUG" => Some(Debug)
    case "TRACE" => Some(Trace)
    case "INFO"  => Some(Info )
    case "WARN"  => Some(Warn )
    case "ERROR" => Some(Error)
    case _ => None
  }

  implicit val logLevelOrder: Order[LogLevel] = {
    Order.by[LogLevel, Int](_.index)
  }

  implicit val logLevelOrdering: Ordering[LogLevel] = {
    logLevelOrder.toOrdering
  }

  // implicit val logLevelShow: Show[LogLevel] = {
  //   Show.instance[LogLevel](_.toString)
  // }

  import io.circe.{Encoder, Decoder}
  import io.circe.syntax._

  implicit val logLevelEncoder = Encoder.instance[LogLevel](_.toString.asJson)
  implicit val logLevelDecoder = Decoder.instance[LogLevel](c =>
    c.as[String].flatMap(
      str => LogLevel.fromString(str).fold[Decoder.Result[LogLevel]](
        Left(io.circe.DecodingFailure(s"Not a valid log level string: $str", c.history))
      )(Right(_))
    )
  )

}
