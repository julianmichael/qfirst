package qfirst.clause.align

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._

import com.monovore.decline._
import com.monovore.decline.effect._

import shapeless.syntax.singleton._
// import shapeless.labelling._
import shapeless._

import cats.effect._
import cats.implicits._

import fs2.Stream

import jjm.DependentMap
import jjm.ling.en.InflectedForms
import jjm.implicits._

import qasrl._
import qasrl.bank.Data
import qasrl.data.Dataset
import qasrl.labeling.SlotBasedLabel

import qfirst.clause._
import qfirst.metrics._
import qfirst.metrics.HasMetrics.ops._

object Convert extends CommandIOApp(
  name = "mill -i qfirst.clause-align.jvm.run",
  header = "Runs clause resolution on CSV inputs and reports results.") {

  def main: Opts[IO[ExitCode]] = {
    val inPathO = Opts.option[NIOPath](
      "in", metavar = "path", help = "Input CSV path."
    )
    val outPathO = Opts.option[NIOPath](
      "out", metavar = "path", help = "Output path."
    )
    (inPathO, outPathO).mapN(run)
  }

  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

  def readCSV[A](path: NIOPath)(f: (List[String], Stream[IO, Map[String, String]]) => IO[A]): IO[A] = {
    val format = if(path.toString.endsWith(".tsv")) new TSVFormat{} else new DefaultCSVFormat {}
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVReader.open(new File(path.toString))(format)))(
      use = reader => {
        IO(reader.readNext.get) >>= (headers =>
          f(headers,
            Stream.fromIterator[IO, Map[String, String]](
              reader.all.iterator.map(line => headers.zip(line).toMap)
            )
          )
        )
      })(
      release = reader => IO(reader.close()))
  }

  def writeCSV(path: NIOPath, headers: List[String])(lines: Stream[IO, Map[String, String]]): IO[Unit] = {
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVWriter.open(new File(path.toString))))(
      use = writer => {
        IO(writer.writeRow(headers)) >>
          lines.evalMap { line =>
            IO(writer.writeRow(headers.map(line)))
          }.compile.drain
      })(
      release = writer => IO(writer.close()))
  }

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(dec)
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  val newHeaders = List(
    "aligned_question",
    "arg_slot",
    "clause",
    "templated_clause",
    "ng_clause",
    "neg_flipped_ng_clause",
    "to_clause",
    "inv_clause",
    "inv_neg_would_clause"
  )


  case class VerbInfo(
    sentenceId: String,
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    sentenceTokens: Vector[String],
    originalLines: List[Map[String, String]]
  ) {
    val questions = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
      sentenceTokens, verbInflectedForms, originalLines.map(_.apply("question"))
    ).map(_.getOrElse((println(this): Id[Unit]) >> ???))

    val resolvedQAPairs = ClauseResolution.getResolvedFramePairs(
      verbInflectedForms, questions
    ).zip(originalLines.map(_.apply("answer")))

    val resolvedQAPairsByFrame = resolvedQAPairs.groupBy(_._1._1)

    val resolvedQAPairsByStructure = resolvedQAPairs.groupBy(
      p => ArgStructure(p._1._1.args, p._1._1.isPassive).forgetAnimacy
    )

    val resolvedQAPairsByObj2lessStructure = resolvedQAPairs.groupBy(
      p => ArgStructure(p._1._1.args remove Obj2, p._1._1.isPassive).forgetAnimacy
    )

    val intransitive = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false)),
      isPassive = false
    )
    val transitive = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false))
        .put(Obj, Noun(false)),
      isPassive = false
    )
    val passive = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false)),
      isPassive = true
    )
    val transitiveWhere = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false))
        .put(Obj, Noun(false))
        .put(Obj2, Locative),
      isPassive = false
    )
    val intransitiveWhere = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false))
        .put(Obj2, Locative),
      isPassive = false
    )
    val passiveWhere = ArgStructure(
      DependentMap.empty[ArgumentSlot.Aux, Id]
        .put(Subj, Noun(false))
        .put(Obj2, Locative),
      isPassive = true
    )

    type ClausalQ = (ArgStructure, ArgumentSlot)
    type ClausalQPair = (ClausalQ, ClausalQ)

    val structuralMappingPairs = Vector[ClausalQPair](
      (transitive -> Subj) -> (transitiveWhere -> Subj),
      (transitive -> Obj) -> (transitiveWhere -> Obj),
      (intransitive -> Subj) -> (intransitiveWhere -> Subj),
      (intransitive -> Obj) -> (intransitiveWhere -> Obj),
      (transitive -> Obj) -> (passive -> Subj),
      (transitiveWhere -> Obj) -> (passiveWhere -> Subj),
      (transitiveWhere -> Obj2) -> (transitive -> Adv("where".lowerCase))
    )

    val structuralMapping = structuralMappingPairs
      .foldMap(p =>
        Map(
          p._1 -> Vector(p._2),
          p._2 -> Vector(p._1),
        )
      )

    val recapitalize = (s: String) => {
      if(s.isEmpty) s
      else if(
        s.tail.headOption.exists(_.isUpper) ||
          (s.dropWhile(_ != ' ').nonEmpty &&
             s.dropWhile(_ != ' ').tail.headOption.exists(_.isUpper))) s
      else s.updated(0, s.charAt(0).toLower)
    }

    def removeObj2(structure: ArgStructure): ArgStructure = {
      ArgStructure.args.modify(_ remove Obj2)(structure)
    }

    def getAlignedAnswers(clausalQ: (ArgStructure, ArgumentSlot)): Option[List[(ArgumentSlot, String)]] = {
      val queries = clausalQ +: structuralMapping.get(clausalQ).foldK
      queries.map(q =>
        resolvedQAPairsByStructure.get(q._1).flatMap(
          _.find(_._1._2 == q._2).map(_._2).map(
            _.split("~!~").toList.map(recapitalize).map(clausalQ._2 -> _)
          )
        )
      ).foldK.orElse {
        if(clausalQ._2 != Subj) None else {
          val obj2lessCQ = removeObj2(clausalQ._1) -> clausalQ._2
          val obj2lessQueries = obj2lessCQ +: structuralMapping.get(obj2lessCQ).foldK.filter(_._2 == Subj)
          obj2lessQueries.map(q =>
            resolvedQAPairsByObj2lessStructure.get(q._1).flatMap(
              _.find(_._1._2 == q._2).map(_._2).map(
                _.split("~!~").toList.map(recapitalize).map(Subj -> _)
              )
            )
          ).foldK
        }
      }
      //   .orElse {
      //   if(clausalQ._2 != Obj2) None else {
      //     clausalQ._1.args.get(Obj2).get match {
      //       case Prep(prep, _) if !prep.endsWith("do".lowerCase) && !prep.endsWith("doing".lowerCase) =>
      //         val obj2lessTemplate = removeObj2(clausalQ._1)
      //         resolvedQAPairsByObj2lessStructure.get(obj2lessTemplate).map(
      //           _.collect { case ((_, Adv(_)), answers) =>
      //             answers.split("~!~").toList
      //               .filter(_.startsWith(prep))
      //               .filter(_.length > prep.length)
      //               .map(_.substring(prep.length + 1))
      //           }.flatten.map(Obj2 -> _)
      //         ).filter(_.nonEmpty)
      //       case _ => None
      //     }
      //   }
      // }
    }

    val newLines = (originalLines, resolvedQAPairs).zipped.map {
      case (fields, ((frame, slot), answer)) =>
        val frame2 = Frame2.fromFrame(frame)
        val templatedFrame = frame.copy(
          tense = PresentTense, isPerfect = false,
          isProgressive = false, isNegated = false
        )
        val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
        val argMappings = clauseTemplate.args.keys.toList.flatMap { (slot: ArgumentSlot) =>
          getAlignedAnswers(clauseTemplate -> slot)
        }.sequence.map(_.toMap)

        val tenseLens = Frame2.tense
        val negLens = Frame2.isNegated

        // val argMappings = resolvedQAPairsByStructure(clauseTemplate)
        //   .map { case ((_, slot), answer) =>
        //     answer.split("~!~").toList.map(slot -> _)
        //   }.sequence.map(_.toMap)

        def renderFrameStrings(getString: Map[ArgumentSlot, String] => String) =
          argMappings.map(getString).toSet.mkString("~!~")

        val decl = renderFrameStrings(
          argValues => frame2.clausesWithArgs(argValues).head
        )

        val ngClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense2.NonFinite.Gerund)(frame2)
            .clausesWithArgs(argValues).head
        )
        val negFlippedNgClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense2.NonFinite.Gerund)(
              negLens.modify(!_)(frame2)
            ).clausesWithArgs(argValues).head
        )
        val toClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense2.NonFinite.To)(frame2)
            .clausesWithArgs(argValues).head
        )
        val invClause = renderFrameStrings(
          argValues => frame2
            .questionsForSlotWithArgs(None, argValues).head.init
        )
        val invNegWouldClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense2.Finite.Modal("would".lowerCase))(
              negLens.set(true)(frame2)
            )
            .questionsForSlotWithArgs(None, argValues).head.init
        )

        fields +
          ("aligned_question" -> renderFrameStrings(argMapping =>
             frame.questionsForSlotWithArgs(slot, argMapping).head)) +
          ("arg_slot" -> slot.toString) +
          ("clause" -> decl) +
          ("templated_clause" -> templatedFrame.clauses.head) +
          ("ng_clause" -> ngClause) +
          ("neg_flipped_ng_clause" -> negFlippedNgClause) +
          ("to_clause" -> toClause) +
          ("inv_clause" -> invClause) +
          ("inv_neg_would_clause" -> invNegWouldClause)
    }

    val metrics = {
      "placeholder coverage (same clause (no tense), by clause (no tense))" ->>
        resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
          val (covered, uncovered) = frame.args.keySet
            .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
          Proportion.Stats(covered.size, uncovered.size)
        } ::
      "placeholder coverage (same clause (no tense))" ->>
        resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
          val total = questions.size
          val (covered, uncovered) = frame.args.keySet
            .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
          Proportion.Stats(covered.size * total, uncovered.size * total)
        } ::
      "uncovered placeholders (same clause (no tense))" ->>
        resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
          val total = questions.size
          val (covered, uncovered) = frame.args.keySet
            .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
          FewClassCount(uncovered.toVector.map(unc => (frame -> unc).toString))
        } ::
      HNil
    }
  }

  def runForFile(qasrlData: Dataset, inPath: NIOPath, outPath: NIOPath) = for {
    (verbs, headers) <- readCSV(inPath)(
      (headers, lines) => lines
        .groupAdjacentBy(fields => fields("qasrl_id") -> fields("verb_idx"))
        .map { case ((sid, verbIndex), chunk) =>
          val sentence = qasrlData.sentences(sid)
          VerbInfo(
            sid,
            verbIndex.toInt,
            sentence.verbEntries(verbIndex.toInt).verbInflectedForms,
            sentence.sentenceTokens,
            chunk.toList)
        }.compile.toList.map(_ -> headers)
    )
    _ <- writeCSV(outPath, headers ++ newHeaders)(
      Stream.emits(verbs.flatMap(_.newLines))
    )
  } yield verbs.foldMap(_.metrics)

  def run(inPath: NIOPath, outPath: NIOPath): IO[ExitCode] = for {
    qasrlData <- IO(Data.readFromQasrlBank(qasrlBankPath).get).map(_.all)
    metrics <- IO(Files.isDirectory(inPath)).ifM(
      IO(new File(inPath.toString).listFiles.toList.map(f => Paths.get(f.getPath))) >>= (paths =>
        paths.filter(_.toString.endsWith(".silver.csv")).foldMapM { inPath =>
          val newName = inPath.getFileName.toString.replaceAll("\\.[^.]*$", ".aligned.csv")
          val newPath = outPath.resolve(newName)
          runForFile(qasrlData, inPath, newPath) <* IO(
            println(s"Wrote alignments of $inPath to $newPath")
          )
        }
      ),
      runForFile(qasrlData, inPath, outPath)
    )
    _ <- IO(println(getMetricsString(metrics)))
  } yield ExitCode.Success
}
