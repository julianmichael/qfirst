package qfirst.cafe.browse

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  apiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  jsDepsLocation: String,
  jsLocation: String,
  dataMetaIndexContent: String
)
