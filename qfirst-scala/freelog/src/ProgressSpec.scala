package freelog

case class ProgressSpec[Msg](
  renderProgress: (Option[Msg], Option[Long]) => Long => Msg
)
object ProgressSpec {

  val getSimpleIterationLabel = (prefixOpt: Option[String]) => (cur: Long) => {
    val fullPrefix = prefixOpt.filter(_.nonEmpty).fold("")(_ + ": ")
    s"${fullPrefix}${cur}it"
  }

  def getSimpleProgressBar(length: Int, prefixOpt: Option[String])(total: Long)(cur: Long): String = {
    val fullPrefix = prefixOpt.filter(_.nonEmpty).fold("")(_ + ": ")
    val num = math.round(cur.toDouble / total * length).toInt
    val pct = math.round(cur.toDouble / total * 100.0).toInt
    val bars = "#" * num
    val spaces = " " * (length - num)
    f"${fullPrefix}$cur%d/$total%d [$bars%s$spaces%s] $pct%3d%%"
  }

  def simple(barLength: Int) = ProgressSpec[String](
    (prefixOpt, sizeOpt) => sizeOpt.fold(
      getSimpleIterationLabel(prefixOpt))(
      getSimpleProgressBar(barLength, prefixOpt))
  )
}
