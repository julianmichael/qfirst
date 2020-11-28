package qfirst.frame.eval

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

import jjm.implicits._

import qfirst.metrics.Functions

import qfirst.frame.logLevel
import com.cibo.evilplot.plot.renderers.PlotElementRenderer
import com.cibo.evilplot.plot.aesthetics.Colors

object Plotting {

  import com.cibo.evilplot._
  import com.cibo.evilplot.colors._
  import com.cibo.evilplot.geometry._
  import com.cibo.evilplot.numeric._
  import com.cibo.evilplot.plot._
  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  import com.cibo.evilplot.plot.aesthetics.Theme
  import com.cibo.evilplot.plot.renderers._

  import qfirst.frame.util.Duad
  import cats.Order
  import cats.Show

  def plotNPMI[A : Order : Show](
    stats: Map[Duad[A], Double],
    title: String
  ): Plot = {
    val axis = stats.keySet.flatMap(p => Set(p.min, p.max)).toVector.sorted
    val data = axis.map(x =>
      axis.map(y => stats(Duad(x, y)))
    )
    val colors = ScaledColorBar.apply(List(HTMLNamedColors.red, HTMLNamedColors.white, HTMLNamedColors.blue), -1.0, 1.0)

    Heatmap2(data, colors)
      .title(title)
      .xAxis(axis.map(_.show))
      .yAxis(axis.map(_.show))
      .frame().rightLegend()
  }
}

