package freelog

case class ProgressSpec[Msg](
  renderProgress: (String, Option[Long]) => Long => Msg
)
object ProgressSpec {

  val getSimpleIterationLabel = (prefix: String) => (cur: Long) => s"$prefix: ${cur}it"

  def getSimpleProgressBar(length: Int, prefix: String)(total: Long)(cur: Long): String = {
    val num = math.round(cur.toDouble / total * length).toInt
    val pct = math.round(cur.toDouble / total * 100.0).toInt
    val bars = "#" * num
    val spaces = " " * (length - num)
    f"$prefix: $pct%3d%% [$bars%s$spaces%s] $cur%d/$total%d"
  }

  def simple(barLength: Int) = ProgressSpec[String](
    (prefix, sizeOpt) => sizeOpt.fold(
      getSimpleIterationLabel(prefix))(
      getSimpleProgressBar(barLength, prefix))
  )
}
