package qfirst

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.monovore.decline._

import java.nio.file.{Path => NIOPath}

import nlpdata.util.LowerCaseStrings._

import qasrl.data.Dataset
import qasrl.data.VerbEntry
import qasrl.bank.Data

import HasMetrics.ops._

object Main {

  def filterGold(minNumAnswers: Int, maxNumInvalid: Int) = (verb: VerbEntry) => {
    val (invalids, valids) = verb.questionLabels.toList.flatMap {
      case (questionString, qLabel) =>
        val judgments = qLabel.answerJudgments.toList.map(_.judgment)
        val numInvalid = judgments.filter(_.isInvalid).size
        val numAnswers = judgments.size
        if(numAnswers >= minNumAnswers) {
          if(numInvalid <= maxNumInvalid) Some(Right(questionString -> qLabel))
          else Some(Left(questionString -> qLabel))
        } else None
    }.separate
    invalids.toMap -> valids.toMap
  }

  val filterGoldNonDense = filterGold(3, 0)
  val filterGoldDense = filterGold(6, 1)

  import Instances.Bucketers

  val questionBucketers = Map(
    "wh" -> Bucketers.wh,
    "prep" -> Bucketers.prep
  )

  val templateBucketers = Map(
    "wh" -> Bucketers.Templated.wh,
    "prep" -> Bucketers.Templated.prep
  )

  val alignedSpanBucketers = Map(
    "gold-dep-len" -> Bucketers.goldDepLength(NonEmptyList.of(1, 2, 3, 5, 8, 12, 18, 27)),
    // "pred-dep-len" -> Bucketers.predDepLength(NonEmptyList.of(1, 2, 3, 5, 8, 12, 18, 27))
  )


  val sortSpec = {
    import MetricValue._
    import MapTree.SortQuery._
    val double = value[String](
      (mv: MetricValue) => mv match {
        case MetricInt(x) => x.toDouble
        case MetricDouble(x) => x
        case MetricIntOfTotal(x, _) => x.toDouble
      }
    )
    List(
      "full question" :: "f1" :: double
    )
  }

  val filterPred = BeamFilter(
    questionThreshold = 0.1,
    spanThreshold = 0.5,
    invalidThreshold = 0.9,
    shouldRemoveSpansBelowInvalidProb = true)

  val allAFirstFilters = (5 to 95 by 5)
    .map(_.toDouble / 100)
    .toList.map(spanThreshold =>
    BeamFilter(
      questionThreshold = 0.1,
      spanThreshold = spanThreshold,
      invalidThreshold = 0.9,
      shouldRemoveSpansBelowInvalidProb = false)
  )

  def nullBucketer[I] = Map.empty[String, I => String]

  def verbBucketers(verbFreq: (LowerCaseString => Int)) = Map(
    "verb-freq" -> Bucketers.verbFreq(
      verbFreq,
      NonEmptyList.of(0, 10, 50, 150, 250, 500, 750, 1000))
  )

  def runDenseMetrics(
    getVerbFrequency: => (LowerCaseString => Int),
    gold: Dataset,
    pred: Map[String, SentencePrediction]
  ) = {
    import qfirst.{Instances => I}
    import qfirst.{Metrics => M}
    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._

    val computeMetrics = M.split(I.sentenceToVerbs) {
      M.hchoose(
        "num verbs" ->> ((vi: I.VerbInstance) => 1) ::
          "full question" ->> (
            M.choose(allAFirstFilters) { filterPred =>
              I.verbToQASet(filterGoldDense, filterPred) andThen
              M.split(I.qaSetToQuestions) {
                // M.bucket(questionBucketers) {
                I.getQuestionBoundedAcc
                // }
              }
            }
          ) :: HNil
      )
    }

    val rawResult = Instances.foldMapInstances(gold, pred)(computeMetrics)
    // NOTE: kinda just a test; maybe we can do this less intrusively
    val result = rawResult.updateWith("full question")(
      _.map(acc => "acc" ->> acc ::
              "questions per verb" ->> (acc.predicted.toDouble / rawResult("num verbs")) :: HNil
      )
    )
    println(result.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
  }

  def runNonDenseMetrics(
    getVerbFrequency: => (LowerCaseString => Int),
    gold: Dataset,
    pred: Map[String, SentencePrediction]
  ) = {
    import qfirst.{Instances => I}
    import qfirst.{Metrics => M}

    import shapeless._
    import shapeless.syntax.singleton._
    import shapeless.record._

    val computeMetrics = M.split(I.sentenceToVerbs) {
      // M.bucket(verbBucketers(getVerbFrequency)) {
        I.verbToQASet(filterGoldNonDense, filterPred) andThen
        M.choose(
          // "span" -> (
          //   I.qaSetToSpanSet andThen
          //     I.getSpanSetConf
          //     // M.split(I.spanSetToAlignedSpans) {
          //     //   M.bucket(alignedSpanBucketers) {
          //     //     I.getAlignedSpanConf
          //     //   }
          //     // }
          // ),
          "full question" -> M.split(I.qaSetToQuestions) {
            // M.bucket(questionBucketers) {
              I.getQuestionConf
            // }
          }
          // "question template" -> (
          //   I.qaSetToQATemplateSet andThen
          //     M.split(I.qaTemplateSetToQuestionTemplates) {
          //       M.bucket(templateBucketers) {
          //         I.getQuestionTemplateConf
          //       }
          //     }
          // )
        )
      // }
    }

    val rawResult = Instances.foldMapInstances(gold, pred)(computeMetrics)
    val result = rawResult
    println(result.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec))
  }

    // take initial params, maxes, etc.
    // println(result.getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("wh")
    //           .all.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(metrics
    //           .collapseBuckets("prep")
    //           .all.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("prep")
    //             // .filter(_.numGold >= 50)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    // println(result.map(b =>
    //           b.mapBucketValues("prep", Bucketers.Mappers.prepIsPresent)
    //             .mapBucketValues("wh", Bucketers.Mappers.whAdv)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("prep")
    //             .mapBucketValues("wh", Bucketers.Mappers.whAdv)
    //         ).getMetrics.toStringPretty(identity, x => x.render))
    // println("================================")
    // println(result.map(b =>
    //           b.collapseBuckets("wh")
    //             .mapBucketValues("prep", Bucketers.Mappers.prepIsPresent)
    //         ).getMetrics.toStringPretty(identity, x => x.render))

  def readPredictions(path: NIOPath) = {
    import ammonite.ops._
    import io.circe.jawn
    read.lines(Path(path, pwd)).toList
      .traverse(jawn.decode[SentencePrediction])
      .map(_.map(pred => pred.sentenceId -> pred).toMap)
  }

  def readVerbFrequencies(trainFile: NIOPath) = {
    val data = Data.readDataset(trainFile)
    data.sentences.iterator
      .flatMap(s => s._2.verbEntries.values.map(_.verbInflectedForms.stem).iterator)
      .foldLeft(Map.empty[LowerCaseString, Int].withDefaultValue(0)) {
      (counts, stem) => counts + (stem -> (counts(stem) + 1))
    }
  }

  def program(trainFile: NIOPath, goldFile: NIOPath, predFile: NIOPath, dense: Boolean) = {
    lazy val verbFrequencies = readVerbFrequencies(trainFile)
    val gold = Data.readDataset(goldFile)
    val predEither = readPredictions(predFile)
    predEither match {
      case Left(error) => System.err.println(error)
      case Right(pred) =>
        if(dense) runDenseMetrics(verbFrequencies, gold, pred)
        else runNonDenseMetrics(verbFrequencies, gold, pred)
    }
  }

  val runMetrics = Command(
    name = "mill qfirst.run",
    header = "Calculate metrics."
  ) {
    val trainPath = Opts.option[NIOPath](
      "train", metavar = "path", help = "Path to the training data file."
    )
    val goldPath = Opts.option[NIOPath](
      "gold", metavar = "path", help = "Path to the gold data file."
    )
    val predPath = Opts.option[NIOPath](
      "pred", metavar = "path", help = "Path to the directory of predictions."
    )
    val dense = Opts.flag(
      "dense", help = "Whether to run the dense eval."
    ).orFalse

    (trainPath, goldPath, predPath, dense).mapN(program)
  }

  def main(args: Array[String]): Unit = {
    val result = runMetrics.parse(args) match {
      case Left(help) => IO { System.err.println(help) }
      case Right(main) => IO { main }
    }
    result.unsafeRunSync
  }
}
