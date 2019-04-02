// package qfirst.topics

// import qfirst._
// import qfirst.frames.implicits._

// import cats.Id
// import cats.effect._
// import cats.implicits._

// import java.nio.file._

// import qasrl._
// import qasrl.data._
// import qasrl.data.JsonCodecs._
// import qasrl.labeling._
// import qasrl.util._
// import qasrl.util.implicits._

// import qasrl.bank._

// import nlpdata.datasets.wiktionary._
// import nlpdata.util.LowerCaseStrings._

// import io.circe.Json
// import io.circe.generic.JsonCodec
// import io.circe.syntax._

// import scala.util.Random

// import qfirst.paraphrase.browse.VerbFrameData

// import scala.collection.mutable
// class TarjanUnionFind[A] private (
//   cells: mutable.Map[A, TarjanUnionFind.UFCell[A]]
// ) {
//   import TarjanUnionFind._

//   def add(a: A): Unit = {
//     if(!cells.contains(a)) cells.put(a, new UFCell(a))
//   }

//   def find(a: A): Option[A] = {
//     findCell(a).map(_.value)
//   }
//   private[this] def findCell(a: A): Option[UFCell[A]] = {
//     this.cells.get(a).map(ufA => findCellAux(ufA, Nil))
//   }
//   private[this] def findCellAux(a: UFCell[A], prevCells: List[UFCell[A]]): UFCell[A] = {
//     if(a.parent == a) {
//       prevCells.foreach(_.parent = a)
//       a
//     } else {
//       findCellAux(a.parent, a :: prevCells)
//     }
//   }

//   def union(a: A, b: A): Option[A] = {
//     (findCell(a), findCell(b)).mapN { (x, y) =>
//       if(x.rank < y.rank) {
//         x.parent = y
//         y.value
//       } else if(y.rank < x.rank) {
//         y.parent = x
//         x.value
//       } else {
//         x.parent = y
//         y.rank = y.rank + 1
//         y.value
//       }
//     }
//   }
// }
// object TarjanUnionFind {
//   private class UFCell[A](val value: A) {
//     var parent: UFCell[A] = this
//     var rank: Int = 0
//   }
//   def empty[A] = new TarjanUnionFind(mutable.Map.empty[A, UFCell[A]])
// }

// object CoindexingApp extends IOApp {

//   import ClauseResolution.ArgStructure

//   type ClausalQ = (ArgStructure, ArgumentSlot)

//   @JsonCodec case class ClauseQAQuestion(
//     clause: String,
//     slot: String
//   ) {
//     val clauseTemplate = io.circe.parser.decode[ArgStructure](clause).right.get
//     val answerSlot = ArgumentSlot.fromString(slot).get
//     def clausalQ = (clauseTemplate, answerSlot)
//   }

//   def adjacencyProb(
//     x: List[(AnswerSpan, Double)],
//     y: List[(AnswerSpan, Double)]
//   ): Double = {
//     val xMap = x.toMap
//     val yMap = y.toMap
//     xMap.keySet.intersect(yMap.keySet).iterator.map { span =>
//       xMap(span) * yMap(span)
//     }.sum
//   }

//   @JsonCodec case class ClauseQAOutput(
//     question: ClauseQAQuestion,
//     spans: List[(AnswerSpan, Double)]
//   )

//   @JsonCodec case class SentenceQAOutput(
//     sentenceId: String,
//     verbs: Map[String, List[ClauseQAOutput]]
//   )

//   def coindexAllFrames(
//     dataset: Dataset,
//     frameData: VerbFrameData,
//     qaOutputs: Map[String, SentenceQAOutput]
//   ): VerbFrameData = {
//     VerbFrameData(
//       inflectionCounts = frameData.inflectionCounts,
//       allFrames = frameData.allFrames.map { case (verbForms, frameset) =>
//         verbForms -> frameset.copy(
//           frames = frameset.frames.map { frame =>
//             val clauseTemplateSet = frame.clauseTemplates.map(_.args).toSet
//             val frameQAOutputs: List[List[ClauseQAOutput]] = ??? // TODO after getting full results as input
//               // frame.instances.flatMap { sid =>
//               //   dataset.sentences(SentenceId.toString(sid))
//               //     .verbEntries.values.filter(_.verbInflectedForms == verbForms)
//               //     .flatMap(verb =>
//               //       qaOutputs(SentenceId.toString(sid)).verbs.get(verb.verbIndex.toString)
//               //         .filter(_.map(_.question.clauseTemplate)
//               //                   .forall(clauseTemplateSet.contains)
//               //         )
//               //     )
//               // }
//             val adjacencyPseudoCounts = frameQAOutputs.foldLeft(
//               Map.empty[(ClausalQ, ClausalQ), (Double, Int)]
//             ) { case (adjCounts, qaOutputs) =>
//                 qaOutputs.tails.toList.foldLeft(adjCounts) {
//                   case (counts, Nil) => counts
//                   case (counts, headQA :: otherQAs) =>
//                     otherQAs.foldLeft(counts) {
//                       case (cs, otherQA) =>
//                         val (curProb, curCounts) = cs.get(headQA.question.clausalQ -> otherQA.question.clausalQ).getOrElse(0.0 -> 0)
//                         val totalAdjProb = curProb + adjacencyProb(headQA.spans, otherQA.spans)
//                         cs + ((headQA.question.clausalQ, otherQA.question.clausalQ) -> (totalAdjProb -> (curCounts + 1)))
//                     }
//                 }
//             }
//             val symmetricAvgAdjacencyPCounts = adjacencyPseudoCounts.map { case (pair, (pcount, numTotal)) =>
//               val (otherPcount, otherTotal) = adjacencyPseudoCounts.get(pair.swap).getOrElse(0.0 -> 0)
//               val avg = (pcount + otherPcount) / (numTotal + otherTotal)
//               pair -> avg
//             }
//             val adjacencyCountThreshold = 0.40
//             val clauseqUF = TarjanUnionFind.empty[ClausalQ]
//             val clausalQs = adjacencyPseudoCounts.keySet.flatMap(p => Set(p._1, p._2))
//             clausalQs.foreach(clauseqUF.add)
//             symmetricAvgAdjacencyPCounts.toList.sortBy(-_._2)
//               .takeWhile(_._2 >= adjacencyCountThreshold)
//               .map(_._1).foreach(Function.tupled(clauseqUF.union(_, _)))
//             val clausalQClusters = clausalQs.groupBy(cq => clauseqUF.find(cq).get)
//             val baseArgSigils = List("X", "Y", "X", "A", "B", "C")
//             val argSigils = baseArgSigils ++ baseArgSigils.map(_ + "2") ++ baseArgSigils.map(_ + "3")
//             val argMappingsByClause = clausalQClusters.toList.filter(_._2.size > 1).foldLeft(
//               Map.empty[ArgStructure, Map[ArgumentSlot, String]] -> argSigils) {
//               case ((assignments, sigil :: remainingSigils), (_, clausalQs)) =>
//                 clausalQs.foldLeft(assignments) {
//                   case (as, (argStructure, argSlot)) =>
//                     val clauseMap = as.get(argStructure).getOrElse(Map.empty[ArgumentSlot, String])
//                     val newClauseMap = clauseMap + (argSlot -> sigil)
//                     as + (argStructure -> newClauseMap)
//                 } -> remainingSigils
//             }._1
//             frame.copy(
//               clauseTemplates = frame.clauseTemplates.map { clauseTemplate =>
//                 clauseTemplate.copy(
//                   argMapping = argMappingsByClause.get(clauseTemplate.args).getOrElse(Map.empty[ArgumentSlot, String])
//                 )
//               }
//             )
//           }
//         )
//       })
//   }

//   lazy val trainOrig = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz"))
//   lazy val trainExpanded = Data.readDataset(Paths.get("qasrl-v2_1").resolve("expanded").resolve("train.jsonl.gz"))
//   lazy val dev = Data.readDataset(Paths.get("qasrl-v2_1").resolve("orig").resolve("dev.jsonl.gz"))
//   lazy val devMini = Data.readDataset(Paths.get("dev-mini.jsonl.gz"))

//   def run(args: List[String]): IO[ExitCode] = {
//     val dataset = args(0) match {
//       case "train" | "train-expanded" => trainExpanded
//       case "train-orig" => trainOrig
//       case "dev" => dev
//       case "dev-mini" => devMini
//     }
//     val frameDataPath = Paths.get(args(1))
//     val qaOutputPath = Paths.get(args(2))
//     val outPath = Paths.get(args(3))
//     for {
//       verbFrameData <- FileUtil.readJson[VerbFrameData](frameDataPath)
//       qaOutput <- FileUtil.readJsonLines[SentenceQAOutput](qaOutputPath).map(output => output.sentenceId -> output).compile.toList
//       _ <- FileUtil.writeJson(outPath, io.circe.Printer.noSpaces)(coindexAllFrames(dataset, verbFrameData, qaOutput.toMap))
//     } yield ExitCode.Success
//   }
// }
