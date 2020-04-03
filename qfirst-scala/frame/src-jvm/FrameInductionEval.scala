package qfirst.frame

object FrameInductionEval {

  // case class PropBankEvaluationPoint(
  //   model: VerbSenseConfig,
  //   maxLoss: Double,
  //   precision: Double,
  //   recall: Double
  // ) {
  //   val f1 = 2 * precision * recall / (precision + recall)
  // }
  // object PropBankEvaluationPoint {
  //   implicit val propBankEvaluationPointHasMetrics: HasMetrics[PropBankEvaluationPoint] = {
  //     new HasMetrics[PropBankEvaluationPoint] {
  //       def getMetrics(p: PropBankEvaluationPoint) = MapTree.fromPairs(
  //         "max loss" -> Metric.double(p.maxLoss),
  //         "precision" -> Metric.double(p.precision),
  //         "recall" -> Metric.double(p.recall),
  //         "f1" -> Metric.double(p.f1)
  //       )
  //     }
  //   }
  // }

  // def evaluatePropBankVerbClusters(
  //   config: Config,
  //   verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
  //   propBankSenseLabels: Instances.PropBankLabels)(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[Map[VerbSenseConfig, PropBankEvaluationPoint]] = {
  //   for {
  //     instances <- config.propBankFullInstances.get
  //     fullEvaluationResultsByConfig <- verbModelsByConfig.toList.infoBarTraverse("Tuning all verb models") { case (vsConfig, modelsByVerb) =>
  //       Log.info(vsConfig.modelName) >>
  //         modelsByVerb.toList.sortBy(-_._2.clauseSets.size).infoBarTraverse("Tuning verbs") { case (verbLemma, verbModel) =>
  //           propBankSenseLabels.values.get(verbLemma).foldMapM { verbSenseLabels =>
  //             Log.trace(verbLemma) >> IO {
  //               val predictedVerbIds = verbModel.clusterTree.unorderedFoldMap(Set(_))
  //               // only includes IDs that were covered in the predictions as well
  //               val verbSenseToIds = verbSenseLabels.toList.foldMap { case (sentenceId, verbMap) =>
  //                 verbMap.toList.foldMap { case (verbIndex, verbSense) =>
  //                   Option(VerbId(sentenceId, verbIndex))
  //                     .filter(predictedVerbIds.contains)
  //                     .foldMap(vid => Map(verbSense -> Set(vid)))
  //                 }
  //               }
  //               verbSenseLabels.iterator.flatMap { case (sentenceId, verbMap) =>
  //                 verbMap.iterator.flatMap { case (verbIndex, verbSense) =>
  //                   verbModel.clusterTree.clustersForValue(VerbId(sentenceId, verbIndex)).iterator.map { clusters =>
  //                     val goldIds = verbSenseToIds(verbSense)
  //                     clusters.map { cluster =>
  //                       val predictedIds = cluster.values.toSet
  //                       val maxLoss = if(cluster.isLeaf) 0.0 else (cluster.loss / predictedIds.size)
  //                       val tp = (predictedIds intersect goldIds).size
  //                       val precision = tp / predictedIds.size
  //                       val recall = tp / goldIds.size
  //                       PropBankEvaluationPoint(vsConfig, maxLoss, precision, recall)
  //                     }
  //                   }
  //                 }
  //               }.toList
  //             }
  //           }.map(_.flatten)
  //         }.map(vsConfig -> _)
  //     }.map(_.toMap)
  //     allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  //       val lossThresholds = evaluationItemResults.foldMap(_.map(_.maxLoss).toSet).toList.sortBy(-_)
  //       lossThresholds.map { maxLoss =>
  //         val chosenResults = evaluationItemResults.map(
  //           _.find(_.maxLoss <= maxLoss).get
  //         )
  //         PropBankEvaluationPoint(
  //           vsConfig,
  //           maxLoss,
  //           chosenResults.map(_.precision).sum / chosenResults.size,
  //           chosenResults.map(_.recall).sum / chosenResults.size
  //         )
  //       }
  //     }
  //     bestModels <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  //       val best = allPoints.maxBy(_.f1)
  //       val resString = getMetricsString(best)
  //       Log.info(s"${vsConfig.modelName} propbank metrics: " + resString) >>
  //         config.propBankResultsPath(vsConfig).flatMap(path =>
  //           FileUtil.writeString(path.resolve("propbank-results.txt"))(resString)
  //         ).as(vsConfig -> best)
  //     }.map(_.toMap)
  //     _ <- {
  //       import com.cibo.evilplot._
  //       import com.cibo.evilplot.numeric._
  //       import com.cibo.evilplot.plot._
  //       import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //       import com.cibo.evilplot.plot.renderers.PointRenderer

  //       case class PRPoint(
  //         model: VerbSenseConfig,
  //         recall: Double,
  //         precision: Double) extends Datum2d[PRPoint] {
  //         val x = recall
  //         val y = precision
  //         def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  //       }

  //       val rand = new scala.util.Random(2643642L)
  //       def noise = scala.math.abs(rand.nextGaussian / 200.0)

  //       val data = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //         PRPoint(vsTuningPoint.model, vsTuningPoint.recall + noise, vsTuningPoint.precision + noise)
  //       }

  //       val plot = ScatterPlot(
	// 	      data,
	// 	      pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//       ).xAxis().yAxis().frame().rightLegend()

  //       config.globalPropBankResultsDir.flatMap(path =>
  //         IO(plot.render().write(new java.io.File(path.resolve("propbank-sense.png").toString)))
  //       )
  //     }
  //   } yield bestModels
  // }

  // @JsonCodec case class VerbSenseTuningPoint(
  //   model: VerbSenseConfig,
  //   maxLoss: Double,
  //   minClauseProb: Double,
  //   lbConf: BinaryConf.Stats,
  //   ubConf: BinaryConf.Stats
  // )

  // def evaluateVerbClusters(
  //   config: Config,
  //   verbClustersByConfig: Map[VerbSenseConfig, Map[InflectedForms, MergeTree[VerbId]]],
  //   goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
  //   evaluationItems: Vector[(InflectedForms, String, Int)])(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): IO[Map[VerbSenseConfig, VerbSenseTuningPoint]] = {
  //   val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
  //     goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
  //   }
  //   if(presentEvaluationItems.isEmpty) IO.pure {
  //     verbClustersByConfig.transform { case (vsConfig, verbClusters) =>
  //       VerbSenseTuningPoint(vsConfig, verbClusters.values.map(_.loss).max, 0.0, BinaryConf.Stats(), BinaryConf.Stats()) // better max than inf i guess
  //     }
  //   } else {
  //     for {
  //       instances <- config.fullInstances.get
  //       _ <- writeLossGraph(
  //         verbClustersByConfig,
  //         config.globalResultsDir.map(_.resolve("loss-trends.png"))
  //       )
  //       fullEvaluationResultsByConfig = verbClustersByConfig.transform { case (vsConfig, clustersByVerb) =>
  //         presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
  //           val verbClauseSets = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
  //             verbMap.map { case (vi, qMap) =>
  //               val questions = qMap.keySet.toList
  //               val clauses = ClauseResolution.getResolvedFramePairs(
  //                 verbInflectedForms, questions
  //               ).map(_._1).map(ClauseResolution.getClauseTemplate).toSet
  //               VerbId(sid, vi) -> clauses
  //             }
  //           }
  //           val inputClauseSet = verbClauseSets(VerbId(sentenceId, verbIndex))

  //           val verbClusters = clustersByVerb(verbInflectedForms)
  //           val instanceClusters = verbClusters.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present
  //                                                                                                   // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
  //           val clusterMetricsByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = instanceClusters.map { tree =>
  //             val clusterSize = tree.size
  //             val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss / clusterSize
  //             val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
  //               verbClauseSets(vid).toList.foldMap(c => Map(c -> 1))
  //             }
  //             val predictedClausesWithProbsIncreasing = clauseCounts
  //               .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
  //               .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
  //               .sortBy(_._2)
  //             val confsForClauseThresholds = predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
  //               val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
  //               val predictedClauses = clausesWithProbs.map(_._1).toList.toSet
  //               val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
  //               val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
  //               val lbConf = BinaryConf.Stats(
  //                 tp = (predictedClauses intersect correctClauses).size,
  //                 tn = 0, // we don't need this for p/r/f
  //                 fp = (predictedClauses -- correctClauses).size,
  //                 fn = (correctClauses -- predictedClauses).size
  //               )
  //               val ubConf = BinaryConf.Stats(
  //                 tp = (predictedClauses -- incorrectClauses).size,
  //                 tn = 0, // we don't need this for p/r/f
  //                 fp = (predictedClauses intersect incorrectClauses).size,
  //                 fn = (correctClauses -- predictedClauses).size
  //               )
  //               clauseThreshold -> (lbConf -> ubConf)
  //             }.toList
  //             lossThreshold -> confsForClauseThresholds
  //           }
  //           clusterMetricsByThresholds
  //         }
  //       }
  //       allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  //         val lossThresholds = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
  //         lossThresholds.flatMap { maxLoss =>
  //           val chosenClusters = evaluationItemResults.map(
  //             _.find(_._1 <= maxLoss).get._2
  //           )
  //           val clauseProbThresholds = chosenClusters.flatMap(_.map(_._1)).toSet
  //           clauseProbThresholds.map { minClauseProb =>
  //             val confPairs = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
  //             val lbConf = confPairs.foldMap(_._1)
  //             val ubConf = confPairs.foldMap(_._2)
  //             VerbSenseTuningPoint(vsConfig, maxLoss, minClauseProb, lbConf, ubConf)
  //           }
  //         }
  //       }
  //       bestModels <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  //         val lbBest = allPoints.maxBy(_.lbConf.f1)
  //         val ubBest = allPoints.maxBy(_.ubConf.f1)
  //         val lbResString = getMetricsString(lbBest.lbConf)
  //         val ubResString = getMetricsString(ubBest.ubConf)
  //         Log.info(s"${vsConfig.modelName} clause lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} clause lb metrics: " + lbResString) >>
  //           Log.info(s"${vsConfig.modelName} clause ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
  //           Log.info(s"${vsConfig.modelName} clause ub metrics: " + ubResString) >>
  //           config.resultsPath(vsConfig).flatMap(path =>
  //             FileUtil.writeString(path.resolve("verb-sense-lb-results.txt"))(lbResString) >>
  //               FileUtil.writeString(path.resolve("verb-sense-ub-results.txt"))(ubResString) >>
  //               FileUtil.writeJson(path.resolve("verb-sense-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
  //               FileUtil.writeJson(path.resolve("verb-sense-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
  //           ).as(vsConfig -> ubBest)
  //       }.map(_.toMap)
  //       _ <- {
  //         import com.cibo.evilplot._
  //         import com.cibo.evilplot.numeric._
  //         import com.cibo.evilplot.plot._
  //         import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //         import com.cibo.evilplot.plot.renderers.PointRenderer

  //         case class PRPoint(
  //           model: VerbSenseConfig,
  //           recall: Double,
  //           precision: Double) extends Datum2d[PRPoint] {
  //           val x = recall
  //           val y = precision
  //           def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  //         }

  //         val rand = new scala.util.Random(2643642L)
  //         def noise = scala.math.abs(rand.nextGaussian / 200.0)

  //         val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.model, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
  //         }
  //         val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  //           PRPoint(vsTuningPoint.model, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
  //         }

  //         val lbPlot = ScatterPlot(
	// 	        lbData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()
  //         val ubPlot = ScatterPlot(
	// 	        ubData,
	// 	        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	//         ).xAxis().yAxis().frame().rightLegend()

  //         config.globalResultsDir.flatMap(path =>
  //           IO(lbPlot.render().write(new java.io.File(path.resolve("verb-sense-lb.png").toString))) >>
  //             IO(ubPlot.render().write(new java.io.File(path.resolve("verb-sense-ub.png").toString)))
  //         )
  //       }
  //     } yield bestModels
  //   }
  // }

  // @JsonCodec case class ParaphraseTuningPoint(
  //   vsConfig: VerbSenseConfig,
  //   minClauseProb: Double,
  //   minCoindexingProb: Double,
  //   lbConf: BinaryConf.Stats,
  //   ubConf: BinaryConf.Stats
  // )

  // // def tuningParaphraseEvaluation(
  // //   config: Config,
  // //   verbFramesetsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbFrameset]],
  // //   goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
  // //   evaluationItems: Vector[(InflectedForms, String, Int)])(
  // //   implicit Log: Logger[IO, String]
  // // ): IO[Map[VerbSenseConfig, ParaphraseTuningPoint]] = {
  // //   val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
  // //     goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
  // //   }
  // //   if(presentEvaluationItems.isEmpty) IO.pure {
  // //     verbFramesetsByConfig.transform { case (vsConfig, verbClusters) =>
  // //       ParaphraseTuningPoint(vsConfig, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
  // //     }
  // //   } else {
  // //     for {
  // //       instances <- config.fullInstances.get
  // //       fullEvaluationResultsByConfig = verbFramesetsByConfig.transform { case (vsConfig, framesets) =>
  // //         presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
  // //           val verbClausalQs = instances.values(verbInflectedForms).flatMap { case (sid, verbMap) =>
  // //             verbMap.map { case (vi, qMap) =>
  // //               val questions = qMap.keySet.toList
  // //               val clausalQs = ClauseResolution.getResolvedStructures(questions).toSet
  // //                 .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
  // //               VerbId(sid, vi) -> clausalQs
  // //             }
  // //           }
  // //           val verbId = VerbId(sentenceId, verbIndex)
  // //           val inputClausalQs = verbClausalQs(verbId)
  // //           val verbFrameset = framesets(verbInflectedForms)
  // //           val verbFrame = verbFrameset.frames.find(_.verbIds.contains(verbId)).get
  // //           // each is a (loss threshold, vector of (clause threshold, (perf lower bound, perf upper bound)))
  // //           val paraphrasingMetricByThresholds: List[(Double, List[(Double, (BinaryConf.Stats, BinaryConf.Stats))])] = {
  // //             verbFrame.clauseTemplates.sortBy(_.probability).tails.toList.map { clauses =>
  // //               val minClauseProb = clauses.headOption.fold(1.0)(_.probability)
  // //               val clauseTemplates = clauses.map(_.args).toSet
  // //               // each list is sorted increasing by minCoindexingProb
  // //               val confPairLists = inputClausalQs.toList.map { cq =>
  // //                 verbFrame.coindexingTree.clustersForValue(cq).get.map { tree =>
  // //                   val clusterSize = tree.size
  // //                   val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
  // //                   val minCoindexingProb = 1.0 - maxLoss
  // //                   val predictedParaphrases = tree.values.toSet - cq
  // //                   val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
  // //                   val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
  // //                     instanceParaphrases.incorrectClauses.flatMap(ct =>
  // //                       getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
  // //                     )

  // //                   val lbConf = BinaryConf.Stats(
  // //                     tp = (predictedParaphrases intersect correctParaphrases).size,
  // //                     tn = 0, // we don't need this for p/r/f
  // //                     fp = (predictedParaphrases -- correctParaphrases).size,
  // //                     fn = (correctParaphrases -- predictedParaphrases).size
  // //                   )
  // //                   val ubConf = BinaryConf.Stats(
  // //                     tp = (predictedParaphrases -- incorrectParaphrases).size,
  // //                     tn = 0, // we don't need this for p/r/f
  // //                     fp = (predictedParaphrases intersect incorrectParaphrases).size,
  // //                     fn = (correctParaphrases -- predictedParaphrases).size
  // //                   )
  // //                   minCoindexingProb -> (lbConf -> ubConf)
  // //                 }
  // //               }
  // //               val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
  // //               minClauseProb -> allMinCoindexingProbs.map { minCoindexingProb =>
  // //                 val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
  // //                 val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
  // //                 minCoindexingProb -> (lbConf -> ubConf)
  // //               }
  // //             }
  // //           }
  // //           paraphrasingMetricByThresholds
  // //         }
  // //       }
  // //       allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  // //         val minClauseProbs = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sorted
  // //         minClauseProbs.flatMap { minClauseProb =>
  // //           val chosenClauseSets = evaluationItemResults.map(
  // //             _.find(_._1 >= minClauseProb).get._2
  // //           )
  // //           val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList
  // //           minCoindexingProbs.map { minCoindexingProb =>
  // //             val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
  // //             val lbConf = confPairs.foldMap(_._1)
  // //             val ubConf = confPairs.foldMap(_._2)
  // //             ParaphraseTuningPoint(vsConfig, minClauseProb, minCoindexingProb, lbConf, ubConf)
  // //           }
  // //         }
  // //       }
  // //       bestThresholds <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  // //         val lbBest = allPoints.maxBy(_.lbConf.f1)
  // //         val ubBest = allPoints.maxBy(_.ubConf.f1)
  // //         // val ubBest = allPoints.maxBy(_.ubConf.f1)
  // //         // val ubBestThresholds = CalibratedVerbSenseModel(
  // //         //   vsConfig, verbClustersByConfig(vsConfig), ubBest.maxLoss, ubBest.minClauseProb
  // //         // )
  // //         // (lbBestModel, lbBest.lbConf, ubBestModel, ubBest.ubConf)
  // //         // Log.info(s"${vsConfig.modelName}: " + getMetricsString(ubBest.ubConf))

  // //         val lbResString = getMetricsString(lbBest.lbConf)
  // //         val ubResString = getMetricsString(ubBest.ubConf)
  // //         Log.info(s"${vsConfig.modelName} paraphrase lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
  // //           Log.info(s"${vsConfig.modelName} paraphrase lb metrics: " + lbResString) >>
  // //           Log.info(s"${vsConfig.modelName} paraphrase ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
  // //           Log.info(s"${vsConfig.modelName} paraphrase ub metrics: " + ubResString) >>
  // //           config.resultsPath(vsConfig).flatMap(path =>
  // //             FileUtil.writeString(path.resolve("questions-lb-results.txt"))(lbResString) >>
  // //               FileUtil.writeString(path.resolve("questions-ub-results.txt"))(ubResString) >>
  // //               FileUtil.writeJson(path.resolve("questions-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
  // //               FileUtil.writeJson(path.resolve("questions-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
  // //           ).as(vsConfig -> ubBest)
  // //       }.map(_.toMap)
  // //       _ <- {
  // //         import com.cibo.evilplot._
  // //         import com.cibo.evilplot.numeric._
  // //         import com.cibo.evilplot.plot._
  // //         import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  // //         import com.cibo.evilplot.plot.renderers._

  // //         case class PRPoint(
  // //           model: VerbSenseConfig,
  // //           recall: Double,
  // //           precision: Double) extends Datum2d[PRPoint] {
  // //           val x = recall
  // //           val y = precision
  // //           def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  // //         }

  // //         // val linePointsByModel = allPointsByModel.transform { case (model, vsTuningPoints) =>
  // //         //   NonEmptyList.fromList(vsTuningPoints.sortBy(-_.lbConf.recall)).get
  // //         //     .reduceLeftTo(NonEmptyList.of(_)) { (best, next) =>
  // //         //       if(next.lbConf.precision > best.head.lbConf.precision) {
  // //         //         if(next.lbConf.recall == best.head.lbConf.recall) best
  // //         //         else NonEmptyList(next, best.toList)
  // //         //       } else best
  // //         //     }.toList
  // //         // }
  // //         // val lbLineData = linePointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //         //   PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall, vsTuningPoint.lbConf.precision)
  // //         // }
  // //         // val lbLinePlot = LinePlot(
	// // 	      //   lbData,
	// // 	      //   pathRenderer = Some(PathRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         // ).xAxis().yAxis().frame().rightLegend()


  // //         val rand = new scala.util.Random(2643642L)
  // //         def noise = scala.math.abs(rand.nextGaussian / 200.0)

  // //         val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //           PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
  // //         }
  // //         val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //           PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
  // //         }

  // //         val lbPlot = ScatterPlot(
	// // 	        lbData,
	// // 	        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         ).xAxis().yAxis().frame().rightLegend()
  // //         val ubPlot = ScatterPlot(
	// // 	        ubData,
	// // 	        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         ).xAxis().yAxis().frame().rightLegend()

  // //         config.globalResultsDir.flatMap(path =>
  // //           // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
  // //             IO(lbPlot.render().write(new java.io.File(path.resolve("question-lb.png").toString))) >>
  // //             IO(ubPlot.render().write(new java.io.File(path.resolve("question-ub.png").toString)))
  // //         )
  // //       }
  // //     } yield bestThresholds
  // //   }
  // // }

  // @JsonCodec case class ModelMetrics(
  //   clauseLBConf: BinaryConf.Stats,
  //   clauseUBConf: BinaryConf.Stats,
  //   questionLBConf: BinaryConf.Stats,
  //   questionUBConf: BinaryConf.Stats)
  // object ModelMetrics {
  //   implicit val modelMetricsHasMetrics: HasMetrics[ModelMetrics] = {
  //     new HasMetrics[ModelMetrics] {
  //       def getMetrics(mm: ModelMetrics) = MapTree.fork(
  //         "clause lb" -> mm.clauseLBConf.getMetrics,
  //         "clause ub" -> mm.clauseUBConf.getMetrics,
  //         "question lb" -> mm.questionLBConf.getMetrics,
  //         "question ub" -> mm.questionUBConf.getMetrics
  //       )
  //     }
  //   }
  // }

  // def writeLossGraph[VerbType](
  //   verbClustersByConfig: Map[VerbSenseConfig, Map[VerbType, MergeTree[VerbId]]],
  //   getPath: IO[NIOPath]
  // ): IO[Unit] = {
  //   import com.cibo.evilplot._
  //   import com.cibo.evilplot.numeric._
  //   import com.cibo.evilplot.plot._
  //   import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //   import com.cibo.evilplot.plot.renderers.PointRenderer

  //   case class LossDatum(
  //     model: VerbSenseConfig,
  //     numInstances: Long,
  //     maxLoss: Double,
  //     val x: Double,
  //     val y: Double
  //   ) extends Datum2d[LossDatum] {
  //     def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //   }
  //   val lossData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
  //     clustersByVerb.toList.map { case (_, clusterTree) =>
  //       val numInstances = clusterTree.size
  //       LossDatum(vsConfig, numInstances, clusterTree.loss, numInstances.toDouble, clusterTree.loss)
  //     }
  //   }
  //   val plot = ScatterPlot(
	// 	  lossData,
	// 	  pointRenderer = Some(PointRenderer.colorByCategory(lossData, ((x: LossDatum) => x.model.modelName), size = Some(2.0)))
	//   ).xAxis().yAxis().frame().rightLegend()
  //   getPath.flatMap(path =>
  //     IO(plot.render().write(new java.io.File(path.toString)))
  //   )
  // }

  // def writeDepthGraph[VerbType](
  //   verbClustersByConfig: Map[VerbSenseConfig, Map[VerbType, MergeTree[VerbId]]],
  //   getPath: IO[NIOPath]
  // ): IO[Unit] = {
  //   import com.cibo.evilplot._
  //   import com.cibo.evilplot.numeric._
  //   import com.cibo.evilplot.plot._
  //   import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //   import com.cibo.evilplot.plot.renderers.PointRenderer

  //   case class DepthDatum(
  //     model: VerbSenseConfig,
  //     numInstances: Long,
  //     avgDepth: Double,
  //     val x: Double,
  //     val y: Double
  //   ) extends Datum2d[DepthDatum] {
  //     def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //   }
  //   val depthData = verbClustersByConfig.toList.flatMap { case (vsConfig, clustersByVerb) =>
  //     clustersByVerb.toList.map { case (_, clusterTree) =>
  //       val numInstances = clusterTree.size
  //       case class DepthAcc(depthSum: Int, numLeaves: Int) {
  //         def merge(that: DepthAcc) = {
  //           val totalLeaves = this.numLeaves + that.numLeaves
  //           val totalDepth = this.depthSum + that.depthSum + totalLeaves
  //           DepthAcc(totalDepth, totalLeaves)
  //         }
  //         def avgDepth = depthSum.toDouble / numLeaves
  //       }
  //       val avgDepth = clusterTree.cata[DepthAcc](
  //         leaf = (_, _) => DepthAcc(0, 1),
  //         merge = (_, l, r) => l.merge(r)
  //       ).avgDepth
  //       DepthDatum(vsConfig, numInstances, avgDepth, numInstances.toDouble, avgDepth)
  //     }
  //   }
  //   val plot = ScatterPlot(
	// 	  depthData,
	// 	  pointRenderer = Some(PointRenderer.colorByCategory(depthData, ((x: DepthDatum) => x.model.modelName), size = Some(2.0)))
	//   ).xAxis().yAxis().frame().rightLegend()
  //   getPath.flatMap(path =>
  //     IO(plot.render().write(new java.io.File(path.toString)))
  //   )
  // }

  // @JsonCodec case class FullTuningPoint(
  //   verbSenseConfig: VerbSenseConfig,
  //   maxVerbClusterLoss: Double,
  //   minClauseProb: Double,
  //   minCoindexingProb: Double,
  //   lbConf: BinaryConf.Stats,
  //   ubConf: BinaryConf.Stats
  // )

  // // def tuningFullEvaluation(
  // //   config: Config,
  // //   verbModelsByConfig: Map[VerbSenseConfig, Map[InflectedForms, VerbClusterModel]],
  // //   goldParaphrases: Map[String, Map[Int, VerbParaphraseLabels]],
  // //   evaluationItems: Vector[(InflectedForms, String, Int)])(
  // //   implicit Log: TreeLogger[IO, String]
  // // ): IO[Map[VerbSenseConfig, FullTuningPoint]] = {
  // //   val presentEvaluationItems = evaluationItems.flatMap { case (forms, sid, vi) =>
  // //     goldParaphrases.get(sid).flatMap(_.get(vi)).map(labels => (forms, sid, vi, labels))
  // //   }
  // //   if(presentEvaluationItems.isEmpty) {
  // //     Log.warn("No gold items to evaluate on. Returning empty evaluation.").as(
  // //       verbModelsByConfig.transform { case (vsConfig, _) =>
  // //         FullTuningPoint(vsConfig, 0.0, 0.0, 0.0, BinaryConf.Stats(), BinaryConf.Stats())
  // //       }
  // //     )
  // //   } else {
  // //     for {
  // //       _ <- Log.infoBranch("Writing loss graph")(
  // //         writeLossGraph(
  // //           verbModelsByConfig.mapValues(_.mapValues(_.verbClusterTree)),
  // //           config.globalResultsDir.map(_.resolve("loss-trends.png"))
  // //         )
  // //       )
  // //       _ <- Log.infoBranch("Writing depth graph")(
  // //         writeDepthGraph(
  // //           verbModelsByConfig.mapValues(_.mapValues(_.verbClusterTree)),
  // //           config.globalResultsDir.map(_.resolve("depth-trends.png"))
  // //         )
  // //       )
  // //       instances <- config.fullInstances.get
  // //       fullEvaluationResultsByConfig = verbModelsByConfig.transform { case (vsConfig, verbModels) =>
  // //         presentEvaluationItems.map { case (verbInflectedForms, sentenceId, verbIndex, instanceParaphrases) =>
  // //           val verbId = VerbId(sentenceId, verbIndex)
  // //           val goldQs = instances.values(verbInflectedForms)(sentenceId)(verbIndex).keySet
  // //           val goldClausalQs = ClauseResolution.getResolvedStructures(goldQs.toList).toSet
  // //             .filter(p => p._2 match { case qasrl.Adv(_) => false; case _ => true }) // don't include adverbial questions
  // //           val goldClauseSet = goldClausalQs.map(_._1)
  // //           val verbModel = verbModels(verbInflectedForms)
  // //           val instanceClusters = verbModel.verbClusterTree.clustersForValue(VerbId(sentenceId, verbIndex)).get // verb id must be present

  // //           // max verb cluster loss/elt (dec) -> min clause prob (inc) -> min coindexing prob (inc) -> stats
  // //           instanceClusters.map { tree =>
  // //             val clusterSize = tree.size
  // //             val lossThreshold = if(tree.isLeaf) 0.0 else tree.loss / clusterSize
  // //             val clauseCounts: Map[ArgStructure, Int] = tree.values.foldMap { vid =>
  // //               verbModel.clauseSets(vid).toList.foldMap(c => Map(c -> 1))
  // //             }
  // //             val predictedClausesWithProbsIncreasing = clauseCounts
  // //               // .filter(p => !inputClauseSet.contains(p._1)) // remove clauses already present in gold
  // //               .toList.map { case (clause, count) => clause -> (count.toDouble / clusterSize)} // prob of clause appearing for a verb
  // //               .sortBy(_._2)
  // //             lossThreshold -> predictedClausesWithProbsIncreasing.tails.map { clausesWithProbs =>
  // //               val clauseThreshold = clausesWithProbs.headOption.fold(1.0)(_._2) // predict nothing
  // //               val frameClauses = clausesWithProbs.map(_._1).toList.toSet
  // //               val coindexingTreeOpt = if(frameClauses.isEmpty) None else Some(
  // //                 Coindexing.getCoindexingTree(frameClauses, verbModel.coindexingScores)
  // //               )
  // //               val confPairLists = goldClausalQs.toList.map { cq =>
  // //                 val correctParaphrases = instanceParaphrases.paraphrases.equivalenceClass(cq) - cq
  // //                 val incorrectParaphrases = instanceParaphrases.paraphrases.apartSet(cq) ++
  // //                   instanceParaphrases.incorrectClauses.flatMap(ct =>
  // //                     getArgumentSlotsForClauseTemplate(ct).map(ct -> _)
  // //                   )
  // //                 coindexingTreeOpt.flatMap(_.clustersForValue(cq)) match {
  // //                   case None =>
  // //                     val lbConf = BinaryConf.Stats(fn = correctParaphrases.size)
  // //                     val ubConf = lbConf
  // //                     List(1.0 -> (lbConf -> ubConf))
  // //                   case Some(clustersForQuestion) => clustersForQuestion.map { tree =>
  // //                     val clusterSize = tree.size
  // //                     val maxLoss = if(tree.isLeaf) 0.0 else tree.loss
  // //                     val minCoindexingProb = 1.0 - maxLoss
  // //                     val predictedParaphrases = tree.values.toSet - cq
  // //                     val lbConf = BinaryConf.Stats(
  // //                       tp = (predictedParaphrases intersect correctParaphrases).size,
  // //                       tn = 0, // we don't need this for p/r/f
  // //                       fp = (predictedParaphrases -- correctParaphrases).size,
  // //                       fn = (correctParaphrases -- predictedParaphrases).size
  // //                     )
  // //                     val ubConf = BinaryConf.Stats(
  // //                       tp = (predictedParaphrases -- incorrectParaphrases).size,
  // //                       tn = 0, // we don't need this for p/r/f
  // //                       fp = (predictedParaphrases intersect incorrectParaphrases).size,
  // //                       fn = (correctParaphrases -- predictedParaphrases).size
  // //                     )
  // //                     minCoindexingProb -> (lbConf -> ubConf)
  // //                   }
  // //                 }
  // //               }
  // //               val allMinCoindexingProbs = confPairLists.foldMap(_.map(_._1).toSet).toList.sorted
  // //               clauseThreshold -> allMinCoindexingProbs.map { minCoindexingProb =>
  // //                 val lbConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._1)
  // //                 val ubConf = confPairLists.foldMap(_.find(_._1 >= minCoindexingProb).get._2._2)
  // //                 minCoindexingProb -> (lbConf -> ubConf)
  // //               }
  // //             }.toList
  // //           }
  // //           // clause stats
  // //           // val correctClauses = instanceParaphrases.correctClauses.filter(c => !inputClauseSet.contains(c))
  // //           // val incorrectClauses = instanceParaphrases.incorrectClauses.filter(c => !inputClauseSet.contains(c))
  // //           // val lbConf = BinaryConf.Stats(
  // //           //   tp = (predictedClauses intersect correctClauses).size,
  // //           //   tn = 0, // we don't need this for p/r/f
  // //           //   fp = (predictedClauses -- correctClauses).size,
  // //           //   fn = (correctClauses -- predictedClauses).size
  // //           // )
  // //           // val ubConf = BinaryConf.Stats(
  // //           //   tp = (predictedClauses -- incorrectClauses).size,
  // //           //   tn = 0, // we don't need this for p/r/f
  // //           //   fp = (predictedClauses intersect incorrectClauses).size,
  // //           //   fn = (correctClauses -- predictedClauses).size
  // //           // )
  // //           // clauseThreshold -> (lbConf -> ubConf)
  // //         }
  // //       }
  // //       allPointsByModel = fullEvaluationResultsByConfig.transform { case (vsConfig, evaluationItemResults) =>
  // //         val maxLosses = evaluationItemResults.foldMap(_.map(_._1).toSet).toList.sortBy(-_)
  // //         maxLosses.flatMap { maxLoss =>
  // //           val chosenClusters = evaluationItemResults.map(
  // //             _.find(_._1 <= maxLoss).get._2
  // //           )
  // //           val minClauseProbs = chosenClusters.foldMap(_.map(_._1).toSet).toList.sorted
  // //           minClauseProbs.flatMap { minClauseProb =>
  // //             val chosenClauseSets = chosenClusters.map(_.find(_._1 >= minClauseProb).get._2)
  // //             val minCoindexingProbs = chosenClauseSets.flatMap(_.map(_._1)).toSet.toList.sorted
  // //             minCoindexingProbs.map { minCoindexingProb =>
  // //               val confPairs = chosenClauseSets.map(_.find(_._1 >= minCoindexingProb).get._2)
  // //               val lbConf = confPairs.foldMap(_._1)
  // //               val ubConf = confPairs.foldMap(_._2)
  // //               FullTuningPoint(vsConfig, maxLoss, minClauseProb, minCoindexingProb, lbConf, ubConf)
  // //             }
  // //           }
  // //         }
  // //       }
  // //       bestTuningPoints <- allPointsByModel.toList.traverse { case (vsConfig, allPoints) =>
  // //         val lbBest = allPoints.maxBy(_.lbConf.f1)
  // //         val ubBest = allPoints.maxBy(_.ubConf.f1)
  // //         val lbResString = getMetricsString(lbBest.lbConf)
  // //         val ubResString = getMetricsString(ubBest.ubConf)

  // //         Log.info(s"${vsConfig.modelName} question lb model: " + io.circe.Printer.spaces2.pretty(lbBest.asJson)) >>
  // //           Log.info(s"${vsConfig.modelName} question lb metrics: " + lbResString) >>
  // //           Log.info(s"${vsConfig.modelName} question ub model: " + io.circe.Printer.spaces2.pretty(ubBest.asJson)) >>
  // //           Log.info(s"${vsConfig.modelName} question ub metrics: " + ubResString) >>
  // //           config.resultsPath(vsConfig).flatMap(path =>
  // //             FileUtil.writeString(path.resolve("full-lb-results.txt"))(lbResString) >>
  // //               FileUtil.writeString(path.resolve("full-ub-results.txt"))(ubResString) >>
  // //               FileUtil.writeJson(path.resolve("full-lb-model.json"), io.circe.Printer.spaces2)(lbBest) >>
  // //               FileUtil.writeJson(path.resolve("full-ub-model.json"), io.circe.Printer.spaces2)(ubBest)
  // //           ).as(vsConfig -> ubBest)
  // //       }.map(_.toMap)
  // //       _ <- {
  // //         import com.cibo.evilplot._
  // //         import com.cibo.evilplot.numeric._
  // //         import com.cibo.evilplot.plot._
  // //         import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  // //         import com.cibo.evilplot.plot.renderers.PointRenderer

  // //         case class PRPoint(
  // //           model: VerbSenseConfig,
  // //           recall: Double,
  // //           precision: Double) extends Datum2d[PRPoint] {
  // //           val x = recall
  // //           val y = precision
  // //           def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
  // //         }

  // //         val rand = new scala.util.Random(2643642L)
  // //         def noise = scala.math.abs(rand.nextGaussian / 200.0)

  // //         val lbData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //           PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.lbConf.recall + noise, vsTuningPoint.lbConf.precision + noise)
  // //         }
  // //         val ubData = allPointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //           PRPoint(vsTuningPoint.verbSenseConfig, vsTuningPoint.ubConf.recall + noise, vsTuningPoint.ubConf.precision + noise)
  // //         }

  // //         val lbPlot = ScatterPlot(
	// // 	        lbData,
	// // 	        pointRenderer = Some(PointRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         ).xAxis().yAxis().frame().rightLegend()
  // //         val ubPlot = ScatterPlot(
	// // 	        ubData,
	// // 	        pointRenderer = Some(PointRenderer.colorByCategory(ubData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         ).xAxis().yAxis().frame().rightLegend()

  // //         // val linePointsByModel = allPointsByModel.transform { case (model, vsTuningPoints) =>
  // //         //   NonEmptyList.fromList(vsTuningPoints.sortBy(-_.lbConf.recall)).get
  // //         //     .reduceLeftTo(NonEmptyList.of(_)) { (best, next) =>
  // //         //       if(next.lbConf.precision > best.head.lbConf.precision) {
  // //         //         if(next.lbConf.recall == best.head.lbConf.recall) best
  // //         //         else NonEmptyList(next, best.toList)
  // //         //       } else best
  // //         //     }.toList
  // //         // }
  // //         // val lbLineData = linePointsByModel.values.toList.flatten.map { vsTuningPoint =>
  // //         //   PRPoint(vsTuningPoint.vsConfig, vsTuningPoint.lbConf.recall, vsTuningPoint.lbConf.precision)
  // //         // }
  // //         // val lbLinePlot = LinePlot(
	// // 	      //   lbData,
	// // 	      //   pathRenderer = Some(PathRenderer.colorByCategory(lbData, ((x: PRPoint) => x.model.modelName), size = Some(1.0)))
	// //         // ).xAxis().yAxis().frame().rightLegend()

  // //         // IO(lbLinePlot.render().write(new java.io.File(path.resolve("question-lb-line.png").toString))) >>
  // //         config.globalResultsDir.flatMap(path =>
  // //           IO(lbPlot.render().write(new java.io.File(path.resolve("full-question-lb.png").toString))) >>
  // //             IO(ubPlot.render().write(new java.io.File(path.resolve("full-question-ub.png").toString)))
  // //         )
  // //       }
  // //     } yield bestTuningPoints
  // //   }
  // // }

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

  // // TODO: elmo loss is ~175x greater; tune around this number
  // // _ <- {
  // //   import qfirst.metrics._
  // //   val dist = Numbers(verbClusters.values.toVector.filterNot(_.loss == 0.0).map(tree => tree.loss / tree.size))
  // //   Log.info(getMetricsString(dist))
  // // }

  // @JsonCodec case class ModelParams(
  //   config: VerbSenseConfig,
  //   maxClusterLoss: Double,
  //   minClauseThreshold: Double,
  //   minCoindexingThreshold: Double)

}