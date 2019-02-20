package qfirst.frames.annotation

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  annApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  jsDepsPath: String,
  jsPath: String
)
