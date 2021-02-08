package freelog

case class ProgressInput[Msg](
  prefix: Option[Msg],
  total: Option[Long],
  length: Option[Int]
)

case class ProgressSpec[Msg](
  renderProgress: ProgressInput[Msg] => Long => Msg
)
object ProgressSpec {

  val getSimpleIterationLabel = (prefixOpt: Option[String]) => (cur: Long) => {
    val fullPrefix = prefixOpt.filter(_.nonEmpty).fold("")(_ + ": ")
    s"${fullPrefix}${cur}it"
  }

  def getSimpleProgressBar(
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

  def simple(defaultBarLength: Int) = ProgressSpec[String](
    input => input.total.fold(
      getSimpleIterationLabel(input.prefix))(
      getSimpleProgressBar(input, defaultBarLength, "#", "-"))
  )
}
