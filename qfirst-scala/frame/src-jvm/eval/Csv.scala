package qfirst.frame.eval

import qfirst.metrics._
// import qfirst.metrics.HasMetrics.ops._
// import qfirst.metrics.Functions

import cats.Show
import cats.effect.IO
import cats.implicits._

object Csv {

  import com.github.tototoshi.csv._

  import scalatags.Text.all._

  def writePrfTableCsv[A : Show, B : Show](
    path: java.nio.file.Path,
    firstHeader: String,
    secondHeader: String,
    stats: Map[A, Map[B, WeightedPR]]
  ): IO[Unit] = IO {
    // val f1SmoothingCount = 1.0

    def calculateSmoothedF1(target: WeightedPR, mean: WeightedPR, smoothing: Double) = {
      val smoothedP = (
        WeightedNumbers(target.precision, target.pseudocount) |+|
          WeightedNumbers(mean.precision, smoothing)
      ).stats.weightedMean
      val smoothedR = (
        WeightedNumbers(target.recall, target.pseudocount) |+|
          WeightedNumbers(mean.recall, smoothing)
      ).stats.weightedMean
      Functions.harmonicMean(smoothedP, smoothedR)
    }

    def prfCells(prf: WeightedPR, all: WeightedPR) = {
      val total = all.pseudocount
      val totalError = 1.0 - all.f1

      val (p, r, f1) = prf.prf
      val pcount = prf.pseudocount
      def smoothedF1(n: Int) = calculateSmoothedF1(prf, all, n.toDouble)

      val percent = pcount / total
      val error = 1.0 - f1
      val weightedError = error * percent
      val percentError = weightedError / totalError
      List(
        f"$p%.2f",
        f"$r%.2f",
        f"$f1%.2f",
        pcount.toInt.toString,
        f"$percent%.4f",
        f"$error%.4f",
        f"$weightedError%.4f",
        f"$percentError%.4f",
        f"${smoothedF1(10)}%.2f",
        f"${smoothedF1(50)}%.2f",
        f"${smoothedF1(100)}%.2f"
      )
    }

    val headers = List(
      firstHeader,
      secondHeader,
      "Prec.",
      "Rec.",
      "F1",
      "Count",
      "Percent",
      "Error",
      "Weighted Error",
      "Pct Error",
      "Smoothed F1 (10)",
      "Smoothed F1 (50)",
      "Smoothed F1 (100)"
    )

    val all = stats.values.toList.foldMap(_.values.toList.combineAll)
    val rows = List(
      "All" :: "All" :: prfCells(all, all)
    ) ++ stats.toList.flatMap {
      case (a, bs) =>
        bs.map { case (b, prf) =>
          List(a.show, b.show) ++ prfCells(prf, all)
        }
      }

    val file = new java.io.File(path.toString)

    val writer = CSVWriter.open(file)
    writer.writeRow(headers)
    rows.foreach(writer.writeRow)
    writer.close()
  }
}
