package qfirst.frames.annotation

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  jsDepsPath: String,
  jsPath: String
)
