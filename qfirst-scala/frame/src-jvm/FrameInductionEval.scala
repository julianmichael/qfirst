package qfirst.frame

object FrameInductionEval {

  // def doPropBankClusterDebugging(
  //   config: Config,
  //   senseLabels: Instances.PropBankLabels,
  //   verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
  //   chosenThresholds: Map[VerbSenseConfig, Double])(
  //   implicit Log: TreeLogger[IO, String]
  // ): IO[Unit] = for {
  //   _ <- Log.infoBranch("Writing loss graph")(
  //     writeLossGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("loss-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing depth graph")(
  //     writeDepthGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("depth-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing PropBank gold sense graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       lemma: String,
  //       numOccurrences: Int,
  //       numSenses: Int,
  //       val x: Double,
  //       val y: Double) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.x) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = senseLabels.values.iterator.map { case (lemma, sentenceMap) =>
  //       val instances = sentenceMap.iterator.flatMap(_._2.values.iterator).toList
  //       val senses = instances.toSet
  //       ThisPoint(lemma, instances.size, senses.size, scala.math.min(1000, instances.size + noise), senses.size + (noise * 10))
  //     }.toList

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => "gold"), size = Some(2.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("propbank-sense-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition sizes graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numInstances: Int,
  //       numClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.values.toList.map { model =>
  //         val numInstances = model.clusterTree.size.toInt
  //         val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //         val numClusters = clusters.size
  //         ThisPoint(vsConfig, numInstances, numClusters, scala.math.min(1000, numInstances.toDouble + noise), numClusters.toDouble + (noise * 10))
  //       }
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("predicted-cluster-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition size comparison graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numGoldClusters: Int,
  //       numPredictedClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 4.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.toList.flatMap { case (verbLemma, model) =>
  //         senseLabels.values.get(verbLemma).map { verbSenseLabels =>
  //           val numGoldClusters = verbSenseLabels.values.iterator.flatMap(_.values.iterator).toSet.size
  //           val numInstances = model.clusterTree.size
  //           val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //           val numPredictedClusters = clusters.size
  //           ThisPoint(vsConfig, numGoldClusters, numPredictedClusters, numGoldClusters.toDouble + noise, numPredictedClusters.toDouble + noise)
  //         }
  //       }
  //     }

  //     val pearsonR = {
  //       val num = data.map(d => d.numGoldClusters * d.numPredictedClusters).sum
  //       val denomGold = data.map(d => d.numGoldClusters * d.numGoldClusters).sum
  //       val denomPredicted = data.map(d => d.numPredictedClusters * d.numPredictedClusters).sum
  //       num.toDouble / scala.math.exp((scala.math.log(denomGold) + scala.math.log(denomPredicted)) / 2)
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     Log.info("Pearson's R between gold and predicted number of senses: " + pearsonR) >>
  //       config.globalPropBankResultsDir.flatMap(path =>
  //         IO(plot.render().write(new java.io.File(path.resolve("cluster-num-correlation.png").toString)))
  //       )
  //   }
  //   // _ <- verbModelsByConfig.toList.traverse { case (vsConfig, verbModels) =>
  //   //   val maxLoss = chosenThresholds(vsConfig)
  //   //   verbModels.toList.traverse { case (verbLemma, verbModel) =>
  //   //     val clusters = verbModel.clusterTree.splitWhile(_.loss > maxLoss)
  //   //     IO.unit // TODO
  //   //   }
  //   // }
  // } yield ()

}
