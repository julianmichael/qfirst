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

  def plotHeterogeneousNPMI[X : Order : Show, Y : Order : Show](
    stats: Map[(X, Y), Double],
    title: String,
    xKeys: List[X] = Nil,
    yKeys: List[Y] = Nil,
  ): Plot = {
    val xAxis = if(xKeys.nonEmpty) xKeys else stats.keySet.map(_._1).toVector.sorted
    val yAxis = if(yKeys.nonEmpty) yKeys else stats.keySet.map(_._2).toVector.sorted
    val data = yAxis.map(y =>
      xAxis.map(x => stats((x, y)))
    )
    val colors = ScaledColorBar.apply(List(HTMLNamedColors.red, HTMLNamedColors.white, HTMLNamedColors.blue), -1.0, 1.0)

    Heatmap2(data, colors)
      .title(title)
      .xAxis(xAxis.map(_.show))
      .yAxis(yAxis.map(_.show))
      .frame().rightLegend()
  }

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

