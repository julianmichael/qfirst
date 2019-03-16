package qfirst.frames.verbal

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  verbApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  jsDepsPath: String,
  jsPath: String
)
