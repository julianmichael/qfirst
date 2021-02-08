package freelog

import cats.implicits._

import scala.concurrent.duration
import scala.concurrent.duration.FiniteDuration

object util extends UtilPlatformExtensions {

  val bigUnitSpecs = {
    import duration._
    List[(TimeUnit, FiniteDuration => Long, String)](
      (DAYS,         (_.toDays),    "d"),
      (HOURS,        (_.toHours),   "h"),
      (MINUTES,      (_.toMinutes), "m"),
      (SECONDS,      (_.toSeconds), "s")
    )
  }

  val smallUnitSpecs = {
    import duration._
    List[(TimeUnit, FiniteDuration => Long, String)](
      (MILLISECONDS, (_.toMillis),  "ms"),
      (NANOSECONDS,  (_.toMillis),  "ns")
    )
  }

  def getTimingString(_delta: FiniteDuration): String = {
    var delta = _delta
    import duration._
    val bigRes = bigUnitSpecs.flatMap { case (unit, convert, label) => 
      val numUnits = convert(delta)
      if(numUnits > 0) {
        delta = delta - FiniteDuration(numUnits, unit)
        Some(s"${numUnits}${label}")
      } else None
    }.mkString(" ")
    if(bigRes.nonEmpty) bigRes else {
      smallUnitSpecs.map { case (unit, convert, label) =>
        val numUnits = convert(delta)
        if(numUnits > 0) {
          Some(s"${numUnits}${label}")
        } else None
      }.foldK.getOrElse("instant")
    }
  }
}
