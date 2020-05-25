package freelog

object emitters {

  type Emitter[Msg] = (Msg, LogLevel) => Msg

  def ignoreLevel[Msg]: Emitter[Msg] = (x, _) => x

  def getPrefix(level: LogLevel) = level match {
    case LogLevel.Debug => s"[DEBUG] "
    case LogLevel.Trace => s"[TRACE] "
    case LogLevel.Info  => s"[INFO ] "
    case LogLevel.Warn  => s"[WARN ] "
    case LogLevel.Error => s"[ERROR] "
  }

  val prefixLevel: Emitter[String] = (x, level) => {
    val prefix = getPrefix(level)
    prefix + x.replaceAll("\n", s"\n$prefix")
  }

  val fansiColorMap: Map[LogLevel, fansi.Attr] = Map(
     LogLevel.Debug -> fansi.Color.Green,
     LogLevel.Trace -> fansi.Color.Cyan,
     LogLevel.Info  -> fansi.Attr.Reset,
     LogLevel.Warn  -> fansi.Color.Yellow,
     LogLevel.Error -> fansi.Color.Red
  )

  val fansiColor: Emitter[String] = (x, level) => level match {
    case LogLevel.Debug => fansi.Color.Green(x).toString
    case LogLevel.Trace => fansi.Color.Cyan(x).toString
    case LogLevel.Info  => fansi.Attr.Reset(x).toString
    case LogLevel.Warn  => fansi.Color.Yellow(x).toString
    case LogLevel.Error => fansi.Color.Red(x).toString
  }
}
