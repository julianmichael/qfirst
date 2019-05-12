package qfirst.paraphrase.browse
import qfirst.paraphrase.RunMode

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  verbApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  mode: RunMode,
  jsDepsPath: String,
  jsPath: String
)
