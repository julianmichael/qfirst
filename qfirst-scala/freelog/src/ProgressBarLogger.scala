package freelog
import freelog.implicits._

import cats.Applicative
import cats.Monad
import cats.implicits._

case class ProgressInput[Msg](
  prefix: Option[Msg],
  total: Option[Long],
  length: Option[Int]
)

trait ProgressBarLogger[F[_]] extends EphemeralLogger[F, String] {
  val F: Monad[F]

  def getLoggableLineLength: F[Option[Int]]

  def emitProgress(
    prefix: Option[String],
    sizeHint: Option[Long],
    logLevel: LogLevel,
    current: Long
  ): F[Unit] = {
    F.flatMap(getLoggableLineLength) { lineLength =>
      val progressInput = ProgressInput(prefix, sizeHint, lineLength)
      emit(renderProgress(progressInput)(current), logLevel)
    }
  }

  val getIterationLabel = (prefixOpt: Option[String]) => (cur: Long) => {
    val fullPrefix = prefixOpt.filter(_.nonEmpty).fold("")(_ + ": ")
    s"${fullPrefix}${cur}it"
  }

  def getProgressBar(
    input: ProgressInput[String], defaultLength: Int,
    barChar: String, spaceChar: String
  )(
    total: Long)(
    cur: Long
  ): String = {
    val length = input.length.getOrElse(defaultLength)
    val fullPrefix = input.prefix.filter(_.nonEmpty).fold("")(_ + ": ")

    val proportionStrLength = (total.toString.length * 2) + 1
    val proportionStr = {
      val trial = f"$cur%d/$total%d"
      val numMissingSpaces = proportionStrLength - trial.length
      val preTrial = if(numMissingSpaces > 0) " " * numMissingSpaces else ""
      preTrial + trial
    }

    // 8 = 4 from percentage (ie "100%") plus 4 from spaces and [] around bar
    val barLength = length - (fullPrefix.length + 8 + proportionStrLength)
    val bar = if(barLength < 1) "|" else {
      val num = math.round(cur.toDouble / total * barLength).toInt
      val bars = barChar * num
      val spaces = spaceChar * (barLength - num)
      s"[$bars$spaces]"
    }

    val pct = math.round(cur.toDouble / total * 100.0).toInt

    f"${fullPrefix}$pct%3d%% $bar%s $proportionStr%s"
  }

  val defaultBarLength = 50

  val renderProgress = (
    (input: ProgressInput[String]) => input.total.fold(
      getIterationLabel(input.prefix))(
      getProgressBar(input, defaultBarLength, "#", "-"))
  )
}
