package freelog
import freelog.implicits._

import cats.Applicative
import cats.Monad
import cats.implicits._

trait ProgressBarLogger[F[_]] extends EphemeralLogger[F, String] {
  val F: Monad[F]

  def getLoggableLineLength: F[Option[Int]]

  val defaultBarLength = 50
  // val leftBoundaryChar = "\u2588"
  val leftBoundaryChar = "\u2595"
  val rightBoundaryChar = "\u258f"
  val barChar = "\u2588"
  // val spaceChar = "\u2508"
  val spaceChar = "\u254c"
  // val spaceChar = "\u2574"
  val intermediateBarChars = Vector(
     "\u258F", "\u258E",
     "\u258D", "\u258C",
     "\u258B", "\u258A",
     "\u2589", "\u2588"
  )

  def emitProgress(
    prefix: Option[String],
    sizeHint: Option[Long],
    logLevel: LogLevel,
    current: Long
  ): F[Unit] = {
    F.flatMap(getLoggableLineLength) { lineLength =>
      val msg = sizeHint.fold(getIterationLabel(prefix, current))(
        total => getProgressBar(
          prefix, lineLength.getOrElse(defaultBarLength), total, current
        )
      )
      emit(msg, logLevel)
    }
  }

  def getIterationLabel(prefixOpt: Option[String], cur: Long) = {
    val fullPrefix = prefixOpt.filter(_.nonEmpty).fold("")(_ + ": ")
    s"${fullPrefix}${cur}it"
  }

  def getProgressBar(
    prefix: Option[String],
    length: Int,
    total: Long,
    cur: Long
  ): String = {
    val fullPrefix = prefix.filter(_.nonEmpty).fold("")(_ + ": ")

    // val proportionStrLength = (total.toString.length * 2) + 1
    // val proportionStr = {
    //   val trial = f"$cur%d/$total%d"
    //   val numMissingSpaces = proportionStrLength - trial.length
    //   val preTrial = if(numMissingSpaces > 0) " " * numMissingSpaces else ""
    //   preTrial + trial
    // }
    val proportionStr = f"$cur%d/$total%d"

    val pct = math.round(cur.toDouble / total * 100.0).toInt
    // val pctString = f"$pct%3d%%"
    val pctString = f"$pct%d%%"

    // 2 from [] around bar
    val barLength = length - (fullPrefix.length + pctString.length + 2 + proportionStr.length)
    val bar = if(barLength < 1) "|" else {
      val numEigths = math.round(cur.toDouble / total * barLength * 8).toInt
      val numWholes = numEigths / 8
      val numRemEigths = numEigths % 8
      val bar = if(numRemEigths == 0) {
        val bars = barChar * numWholes
        val spaces = spaceChar * (barLength - numWholes)
        bars + spaces
      } else {
        val bars = (barChar * numWholes) + intermediateBarChars(numRemEigths)
        val spaces = spaceChar * (barLength - numWholes - 1)
        bars + spaces
      }
      s"$leftBoundaryChar$bar$rightBoundaryChar"
    }

    f"${fullPrefix}$pctString%s$bar%s$proportionStr%s"
  }
}
