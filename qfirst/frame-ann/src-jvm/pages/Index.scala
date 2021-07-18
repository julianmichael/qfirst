package qfirst.frame.ann.pages
import scalatags.Text.all._
import qfirst.frame.ann._

object Index {
  def apply(config: PageConfig): scalatags.Text.all.Frag =
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        config.bootstrapLink,
        tag("title")("Annotate QA-SRL Verb Frames"),
      ),
      body(
        div(id := SharedConstants.mainDivElementId)(
          p("Loading JavaScript...")
        ),
        input(
          `type` := "hidden",
          value := config.docApiUrl,
          id := SharedConstants.docApiUrlElementId
        ),
        input(
          `type` := "hidden",
          value := config.verbApiUrl,
          id := SharedConstants.verbApiUrlElementId
        ),
        config.bootstrapScripts,
        script(src := "/" + config.jsDepsPath),
        script(src := "/" + config.jsPath)
      )
    )
  def sourcePath = "pages/Index.scalatex"
}


