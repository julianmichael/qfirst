package qfirst.paraphrase.browse

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  verbApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  dev: Boolean,
  jsDepsPath: String,
  jsPath: String
)
