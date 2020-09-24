package qfirst.frame.browse
import qfirst.frame.RunMode

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  verbApiUrl: String,
  featureApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  mode: RunMode,
  jsDepsPath: String,
  jsPath: String
)
