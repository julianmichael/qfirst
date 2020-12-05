package qfirst.frame.eval

import qfirst.metrics._
// import qfirst.metrics.HasMetrics.ops._

import cats.Show
import cats.implicits._

object Html {

  import scalatags.Text.all._

  def prfTableHtml[A : Show](stats: Map[A, WeightedPR]) = {

    def prfCells(prf: WeightedPR, total: Double, totalError: Double) = {
      val (p, r, f1) = prf.prf
      val pc = prf.pseudocount
      val err = 1.0 - f1
      val werr = err * (pc / total)
      val pctErr = werr / totalError
      List(
        td(f"$p%.2f"),
        td(f"$r%.2f"),
        td(f"$f1%.2f"),
        td(pc.toInt),
        td(f"$err%.4f"),
        td(f"$werr%.4f"),
        td(f"$pctErr%.4f")
      )
    }


    def prfTable(
      metricName: String,
      metricByLabel: Map[A, WeightedPR]
    ) = {
      val total = metricByLabel.unorderedFoldMap(_.pseudocount)
      val all = metricByLabel.values.toList.combineAll
      val totalError = 1.0 - all.f1
      table(
        `class` := "pure-table sortable",
        thead(
          tr(
            td(),
            td("Prec."),
            td("Rec."),
            td("F1"),
            td("Count"),
            td("Error"),
            td("Weighted Error"),
            td("Pct Error"),
          )
        ),
        tbody(
          tr(td("All"))(
            prfCells(all, total, totalError): _*
          ))(
          metricByLabel.toList.sortBy(-_._2.pseudocount).map { case (label, prf) =>
            tr(td(label.show))(prfCells(prf, total, totalError): _*)
          }: _*
        )
      )
    }

    html(
      head(
        link(
          rel := "stylesheet",
          href := "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
        ),
        script(
          src := "https://www.kryogenix.org/code/browser/sorttable/sorttable.js"
        ),
        scalatags.Text.tags2.style(
          """tbody tr:nth-child(odd) { background-color: #eee; }"""
        )
        // style()
      ),
      body(
        prfTable("B3 P/R/F1", stats)
      )
    )
  }
}
