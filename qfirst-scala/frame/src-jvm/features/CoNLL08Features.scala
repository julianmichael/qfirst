package qfirst.frame.features

import qfirst.frame._
import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached
import qfirst.frame.util.NonMergingMap
import qfirst.frame.util.VectorFileUtil

import java.nio.file._

import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileUtil
import jjm.implicits._

import cats.Order
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

import monocle.function.{all => Optics}

import freelog._
import freelog.implicits._

import qfirst.datasets.PredArgStructure
import qfirst.datasets.PropBankPredicate
import qfirst.datasets.SyntaxTree
import qfirst.datasets.SyntaxTreeBranch

class CoNLL08Features(
  mode: RunMode,
  assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[Int](mode, assumeGoldVerbSense)(cs, Log) {

  import qfirst.datasets.conll08._

  override val rootDir = Paths.get("frame-induction/conll08")

  val dataService = new CoNLL08FileSystemService(Paths.get("data/conll08st"))

  val splits = RunData[CoNLL08Split](
    train = CoNLL08Split.Train,
    dev = CoNLL08Split.Dev,
    test = CoNLL08Split.TestWSJ
  )

  def keepOnlyVerbalPredicates(sentence: CoNLL08Sentence) = {
    import jjm.ling.en.PTBPosTags
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .filter { pas =>
          val predPos = sentence.tokens(pas.predicateIndex).pos
          PTBPosTags.verbs.contains(predPos)
        }
    )
  }

  // unused
  def keepOnlyCommonPredicates(sentence: CoNLL08Sentence, lemmasToKeep: Set[String]) = {
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .filter { pas =>
          lemmasToKeep.contains(pas.predicate.lemma)
        }
    )
  }

  // unused
  def keepOnlyCommonPredicatesInDataset(ds: NonMergingMap[String, CoNLL08Sentence]) = {
    val predLemmaCounts = ds.value.unorderedFoldMap(_.predicateArgumentStructures.map(_.predicate.lemma).counts)
    val lemmasToKeep = predLemmaCounts.filter(_._2 > 20).keySet
    NonMergingMap(ds.value.map { case (sid, sent) => sid -> keepOnlyCommonPredicates(sent, lemmasToKeep) })
  }

  def dropCorefAndResumptiveRoles(sentence: CoNLL08Sentence) = {
    import jjm.ling.en.PTBPosTags
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .map { pas =>
          // NOTE: the commented bits below print out edge case phenomena (assuming verbal predicates).

          // there are a few cases of this remaning for verbal predicates.
          // AFAICT they are annotation mistakes in gold.
          // if(pas.arguments.map(_._2).contains(pas.predicate.index)) {
          //   System.err.println("Argument coincides with predicate: " + pas.toString)
          //   System.err.println("Argument coincides with predicate: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // Normally pred/arg indices coinciding, or SU roles, are used for non-verbal predicates.
          // there's a (less) small number of these. I assume we should keep them..
          // But I also kinda think they're mistakes. Unclear.
          // if(pas.arguments.map(_._1).contains("SU")) {
          //   System.err.println("SU argument: " + pas.toString)
          //   System.err.println("SU argument: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // There is a not-huge number of these. Unclear how AM-TM is different from AM-TMP.
          // It only appears twice in the trainins set so I think it's a mistake.
          // and I can't totally figure out what AA is about.
          // if(pas.arguments.map(_._1).contains("AA") || pas.arguments.map(_._1).contains("AM-TM")) {
          //   System.err.println("Weird argument: " + pas.toString)
          //   System.err.println("Weird argument: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // potential processing to bring the number of gold labels down to 22.
          // doesn't change the results for the baseline.
          // pas.copy(
          //   arguments = pas.arguments.filterNot(
          //     l => l._1.startsWith("R-") || l._1.startsWith("C-") || l._1 == "SU"
          //   ).map { case (label, index) =>
          //       (if(label == "AM-TM") "AM-TMP" else label) -> index
          //   }
          // )

          pas.copy(
            arguments = pas.arguments.filterNot(
              l => l._1.startsWith("R-") || l._1.startsWith("C-")
            )
          )
        }
    )
  }

  // TODO add unfiltered dataset separately for feature generation

  val dataset: RunDataCell[NonMergingMap[String, CoNLL08Sentence]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    splits.flatMap(split =>
      dataService
        .streamSentences[IO](split)
        .map(s => NonMergingMap(Map(s.id.toString -> dropCorefAndResumptiveRoles(keepOnlyVerbalPredicates(s)))))
        .infoCompile(s"Reading CoNLL 2008 data ($split)")(_.foldMonoid)
    ).toCell("CoNLL 2008 dataset")
  }

  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    dataset.data.map(sents =>
      NonMergingMap(
        sents.value.map { case (sid, sent) =>
          sid -> sent.tokens.map(_.token)
        }
      )
    ).toCell("CoNLL 2008 sentence index")

  val predArgStructures: RunDataCell[Map[String, NonMergingMap[VerbId, PredArgStructure[PropBankPredicate, Int]]]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbId = VerbId(sid, pas.predicateIndex)
          Map(verbType -> NonMergingMap(Map(verbId -> pas)))
        }
      }
    ).toCell("CoNLL 2008 predicate-argument structures")
  }

  override val verbArgSets: RunDataCell[Map[String, Map[VerbId, Set[Int]]]] =
    predArgStructures.data.map(allPAStructures =>
      allPAStructures.mapVals { verbs =>
        verbs.value.mapVals { pas =>
          pas.arguments.map(_._2).toSet
        }
      }
    ).toCell("CoNLL 2008 verb arg sets")

  override val verbSenseLabels =
    mapVerbFeats(predArgStructures.data) { pas =>
      val predicate = pas.predicate
      s"${predicate.lemma}.${predicate.sense}"
    }

  override val argRoleLabels: CachedArgFeats[PropBankRoleLabel] =
    predArgStructures.data.map(predArgStructures =>
      predArgStructures.mapVals { verbs =>
        verbs.value.toList.foldMap { case (verbId, pas) =>
          pas.arguments.foldMap { case (roleLabel, index) =>
            val argId = ArgumentId(verbId, index)
            val label = PropBankRoleLabel(pas.predicate.sense, roleLabel)
            NonMergingMap(argId -> label)
          }
        }
      }
    ).toCell("CoNLL 2008 argument role labels")


  // other things that could be used as argument keys

  // val position = {
  //   if(argIndex < verbIndex) "left"
  //   else if(argIndex > verbIndex) "right"
  //   else "same" // don't generally expect this to happen
  // }
  // val preposition = {
  //   val argToken = sentence.tokens(argIndex)
  //   if(argToken.pos == "IN") argToken.lemma.toLowerCase
  //   else ""
  // }

  val argSyntacticFunctions: CachedArgFeats[String] = {
    cacheArgFeats("CoNLL 2008 argument syntactic functions")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          dependencies(argId.argument)._1
        }
      }
    )
  }

  val argSyntacticFunctionsConverted: CachedArgFeats[String] = {
    import qfirst.datasets.conll08.HasLemma.ops._
    cacheArgFeats("CoNLL 2008 converted argument syntactic functions")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          val verbIndex = argId.verbId.verbIndex
          val verbChildDepLabels = dependencies.filter(_._2 == verbIndex).map(_._1).toSet
          val isPassive = verbChildDepLabels.contains("LGS") || (
            sentence.tokens(verbIndex).pos == "VBN" &&
              dependencies(verbIndex)._1 == "VC" &&
              sentence.tokens(dependencies(verbIndex)._2).lemma == "be"
          )
          // val objIndices = dependencies.filter(_._2 == verbIndex).filter(_._1 == "OBJ").map(_._2).toSet

          dependencies(argId.argument)._1 match {
            // just handle passives reasonably
            case "SBJ" if isPassive => "OBJ"
            case "LGS" => "SBJ"
            // I tried these but they didn't help / reduced purity too much
            // case "OBJ" if objIndices.exists(_ < argIndex) => "OBJ2"
            // case "OBJ" if isPassive => "OBJ2"
            // case "LGS" => "SBJ-trans"
            // case "SBJ" if objIndices.isEmpty => "SBJ-intrans"
            // case "SBJ" if objIndices.nonEmpty => "SBJ-trans"
            case x => x
          }
        }
      }
    )
  }

  import scala.annotation.tailrec
  @tailrec final def getDependencyPathToRoot(
    dependencies: Vector[(String, Int)],
    index: Int,
    acc: List[(String, Int)] = Nil
  ): List[(String, Int)] = {
    if(index == -1) acc
    else {
      val dep = dependencies(index)
      getDependencyPathToRoot(dependencies, dep._2, dep :: acc)
    }
  }

  val argDependencyPaths: CachedArgFeats[String] = {
    cacheArgFeats("CoNLL 2008 predicate-argument dependency paths")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          val verbIndex = argId.verbId.verbIndex
          val argIndex = argId.argument

          val predPathToRoot = getDependencyPathToRoot(dependencies, verbIndex)
          val argPathToRoot = getDependencyPathToRoot(dependencies, argIndex)
          val predPathToLCA = predPathToRoot.takeWhile { case (_, i) =>
            i != argIndex && !argPathToRoot.exists(_._2 == i)
          }
          val argPathToLCA = argPathToRoot.takeWhile { case (_, i) =>
            i != argIndex && !predPathToRoot.exists(_._2 == i)
          }
          val pathStr = predPathToLCA.mkString("->") + "*" + argPathToLCA.reverse.mkString("<-")
          pathStr
        }
      }
    )
  }

  override def argQuestionDists: CachedArgFeats[Map[QuestionTemplate, Double]] = ???

  // lazy val ontonotesFeatures = new OntoNotes5Features(mode, assumeGoldVerbSense)

  // lazy val ontonotesAllData = new Cell(
  //   "All OntoNotes raw data",
  //   ontonotesFeatures.rawDataset.all.map(_.combineAll)
  // )

  // // val ontonotesWsjData: RunDataCell[NonMergingMap[String, CoNLLSentence]] =
  // val ontonotesWsjData = RunData.strings.zip(ontonotesFeatures.index.data)
  //   .flatMap { case (split, filePaths) =>
  //     filePaths
  //       .filter(_.source == "wsj")
  //       .infoBarFoldMapM(s"Reading WSJ files from OntoNotes ($split)") { path =>
  //         Log.trace(path.suffix) >> ontonotesFeatures.ontonotesService.getFile(path).map { file =>
  //           NonMergingMap(file.sentences.map(s => s.path.toString -> s).toMap)
  //         }
  //       }
  //   }.toCell("OntoNotes WSJ data")

  // // simple unigram-based index
  // lazy val ontonotesSentenceIndex = new Cell(
  //   "OntoNotes sentence index", {
  //     ontonotesWsjData.get.map(
  //       _.value.toList.foldMap { case (sid, sentence) =>
  //         sentence.tokens.map(_.token.lowerCase).toSet.toList.foldMap { token =>
  //           Map(token -> Map(sid -> 1))
  //         }
  //       }
  //     )
  //   }
  // )

  // // simple unigram-based index
  // lazy val ontonotesSentenceIndices = ontonotesWsjData.data.map(
  //   _.value.toList.foldMap { case (sid, sentence) =>
  //     sentence.tokens.map(_.token.lowerCase).toSet.toList.foldMap { token =>
  //       Map(token -> Map(sid -> 1))
  //     }
  //   }
  // ).toCell("OntoNotes sentence index")

  // // try and figure out how the sentences line up with each other.


  // // TODO check if there are any bad ones, and if not, change to FileCachedCell of RunData
  // lazy val ontonotesSentenceAlignments = (
  //   dataset.data, ontonotesWsjData.data, ontonotesSentenceIndices.data
  // ).mapN { (data, ontonotes, ontonotesIndex) =>
  //   var numAligned = 0
  //   var numTotal = 0
  //   var numMismatched = 0
  //   data.value.toList.infoBarFoldMapM("Aligning sentences") { case (sid, sentence) =>
  //     Log.trace(f"""Aligned: $numAligned%d (${numAligned * 100.0 / numTotal}%.2f%%)
  //                    |Mismatched: $numMismatched%d (${numMismatched * 100.0 / numTotal}%.2f%%)
  //                    |Total: $numTotal%d
  //                    |""".trim.stripMargin
  //     ) >> IO {
  //       val matchedOntonotesPaths = sentence.tokens.foldMap(tok =>
  //         ontonotesIndex.get(tok.token.lowerCase).combineAll
  //       ).toVector.sortBy(-_._2).take(5)

  //       val sentText = Text.render(sentence.tokens)
  //       val topPath = matchedOntonotesPaths.head._1
  //       val topSentText = Text.render(ontonotes(topPath).tokens)

  //       numTotal += 1
  //       if(topSentText == sentText) {
  //         numAligned += 1
  //         if(Set("train", "dev", "test").exists(split => sid.contains(split) && !topPath.contains(split))) {
  //           numMismatched += 1
  //           System.err.println("\nSPLIT MISMATCH FOR MATCHING TEXT!")
  //           System.err.println(sid)
  //           System.err.println(topPath)
  //           matchedOntonotesPaths.foreach { case (path, count) =>
  //             System.err.println(s"$count: $path")
  //             val ontonotesSentence = ontonotes(path)
  //             System.err.println(Text.render(ontonotesSentence.tokens))
  //           }
  //         }
  //         System.err.print(".")
  //         NonMergingMap(sid -> topPath)
  //       } else {
  //         System.err.println("\n==========")
  //         System.err.println(sid)
  //         System.err.println(Text.render(sentence.tokens))
  //         matchedOntonotesPaths.foreach { case (path, count) =>
  //           System.err.println(s"$count: $path")
  //           val ontonotesSentence = ontonotes(path)
  //           System.err.println(Text.render(ontonotesSentence.tokens))
  //         }
  //         System.err.println()
  //         NonMergingMap(Map[String, String]())
  //       }
  //     }
  //   }
  // }.flatMap(x => x)

  import qfirst.datasets.conll05

  val conll05Path = Paths.get("data/conll05st-release")

  val conll05Splits = {
    import conll05.CoNLL05Split._
    RunData(
      train = IO.pure(Train),
      dev = IO.pure(Dev),
      test = IO.pure(TestWSJ)
    )
  }

  lazy val conll05Sentences: RunDataCell[NonMergingMap[String, conll05.CoNLL05Sentence]] = {
    import conll05._
    import scala.concurrent.ExecutionContext.Implicits.global
    val service = new CoNLL05FileSystemService(conll05Path)
    conll05Splits.flatMap(split =>
      service.streamSentences[IO](split)
        .map(s => s.id.toString -> s)
        .infoCompile("Reading CoNLL 2005 sentences")(
          _.toList.map(l => NonMergingMap(l.toMap))
        )
    ).toCell("CoNLL 2005 Sentences")
  }

  import qfirst.datasets.ptb2
  import qfirst.datasets.propbank1

  val ptb2Path = Paths.get("data/treebank2")

  lazy val ptb2Sentences: Cell[NonMergingMap[ptb2.PTB2SentenceId, ptb2.PTB2Sentence]] = new Cell(
    "PTB2 sentences", {
      val service = new ptb2.PTB2FileSystemService(ptb2Path)
      import scala.concurrent.ExecutionContext.Implicits.global
      service.streamFiles[IO]
        .evalMap(f => Log.trace(f.path.toString).as(f))
        .flatMap(f => Stream.emits[IO, ptb2.PTB2Sentence](f.sentences))
        .map(s => NonMergingMap(s.id -> s))
        .infoCompile("Reading Penn Treebank sentences")(_.foldMonoid)
    }
  )

  lazy val ptb2TextIndex = new Cell(
    "PTB2 sentence index", {
      ptb2Sentences.get.map(
        _.value.toList.foldMap { case (sid, sentence) =>
          Map(Text.render(sentence.tokens.filter(_.pos != "-NONE-")) -> Set(sid))
        }
      )
    }
  )

  // lazy val ptb2SentenceIndex = new Cell(
  //   "PTB2 sentence index", {
  //     ptb2Sentences.get.map(
  //       _.value.toList.foldMap { case (sid, sentence) =>
  //         sentence.tokens.map(_.token.lowerCase).toSet.toList.foldMap { token =>
  //           Map(token -> Map(sid -> 1))
  //         }
  //       }
  //     )
  //   }
  // )

  lazy val conllToPTB2SentenceAlignments: Cell[NonMergingMap[String, ptb2.PTB2SentenceId]] = new Cell(
    "CoNLL 2008 to PTB 2 sentence alignments",
    cacheDir.map(_.resolve("ptb-alignments.jsonl.gz")) >>= (cachePath =>
      FileCached.get[NonMergingMap[String, ptb2.PTB2SentenceId]](
        "CoNLL 2008 to PTB 2 sentence alignments")(
        path = cachePath,
        read = path => FileUtil
          .readJsonLines[(String, ptb2.PTB2SentenceId)](path)
          .infoCompile("Reading alignment pairs")(_.toList.map(ls => NonMergingMap(ls.toMap))),
        write = (path, alignments) => FileUtil
          .writeJsonLines[(String, ptb2.PTB2SentenceId)](path)(alignments.value.toList))(
        for {
          ptbSentences <- ptb2Sentences.get
          // ptbIndex <- ptb2SentenceIndex.get
          ptbTextIndex <- ptb2TextIndex.get
          conllSentences <- dataset.data.all.map(_.combineAll)
          alignments <- conllSentences.value.toList.infoBarTraverse("Aligning CoNLL sentences") { case (sid, sentence) =>

            val collapsedTokens = sentence.tokens.zipWithIndex.foldLeft("") {
              case (str, (next, idx)) => {
                if(sentence.paddingIndices.contains(idx)) str + next.token
                else str + " " + next.token
              }
            }.trim.split(" ").toList
            val sentText = Text.render(collapsedTokens)

            val textMatches = ptbTextIndex.getOrElse(sentText, Set.empty[ptb2.PTB2SentenceId])

            val alignment = if(textMatches.size >= 1) {
              // if(textMatches.size > 1) {
              //   System.err.println("\n===== Multiple matches! =====")
              //   System.err.println(sentText)
              //   textMatches.foreach(System.err.println)
              // }
              sid -> textMatches.head
            } else {
              // val softMatches = sentence.tokens.foldMap(tok =>
              //   ptbIndex.get(tok.token.lowerCase).combineAll
              // ).toVector.sortBy(-_._2).take(5)
              // val topPath = softMatches.head._1

              System.err.println("\n===== NO TEXT MATCH! =====")
              System.err.println(sid)
              System.err.println(sentText)
              System.err.println(Text.render(sentence.tokens))
              // softMatches.foreach { case (path, count) =>
              //   System.err.println(s"$count: $path")
              //   val ptbSentence = ptbSentences(path)
              //   System.err.println(Text.render(ptbSentence.tokens.filter(_.pos != "-NONE-")))
              //   System.err.println(Text.render(ptbSentence.tokens.map(t => s"${t.token}#${t.pos}")))
              // }
              System.err.println()

              ???
              // sid -> topPath
            }

            Log.trace(f"${alignment._1}%-15s -> ${alignment._2}%s") >> IO.pure(alignment)
          }
        } yield NonMergingMap(alignments.toMap)
      )
    )
  )

  val propbank1Path = Paths.get("data/propbank_1")

  lazy val propbank1Sentences: Cell[NonMergingMap[ptb2.PTB2SentenceId, propbank1.PropBank1Sentence]] = new Cell("PropBank 1 sentences", {
    val service = new propbank1.PropBank1FileSystemService(propbank1Path)
    import scala.concurrent.ExecutionContext.Implicits.global
    service.streamSentences[IO]
      .evalMap(s => Log.trace(s.id.toString).as(NonMergingMap(s.id -> s)))
      .infoCompile("Reading PropBank sentences")(_.foldMonoid)
  })

  // lazy val conllCalculatedSpans: Cell[NonMergingMap[String, ]]

  def reindexPAStructures(
    ptbTree: SyntaxTree[ptb2.PTB2Token],
    propBankSentence: propbank1.PropBank1Sentence,
    conll08Sentence: CoNLL08Sentence
  ): List[PredArgStructure[PropBankPredicate, Set[ESpan]]] = {
    val pbArgToPtbSpan = propBankSentence.predArgStructures
      .flatMap(_.arguments).map(_._2)
      .flatMap { argBranch =>
        ???
        // ptbTree.
      }
    conll08Sentence.predicateArgumentStructures.map { origPAS =>
      ???
    }
  }

  override def setup = propbank1Sentences.get.void
  // override def setup = conllToPTB2SentenceAlignments.get >> argSpans.dev >> super.setup
  // ontonotesSentenceAlignments.dev.flatMap { (sentAlignments: NonMergingMap[String, String]) =>
  //   dataset.data.dev >>= { data =>
  //     Log.warn(s"Number of sentences: ${data.value.size}") >>
  //       Log.warn(s"Number of aligned sentences: ${sentAlignments.value.size}")
  //   }
  // } >> super.setup

  override lazy val argSpans: ArgFeats[Map[ESpan, Double]] = {

    cacheArgFeats("CoNLL 2008 arg spans extracted from CoNLL 2005")(
      (dataset.data, conll05Sentences.data, argRoleLabels.data).mapN { (data, props, argRoleLabels) =>
        for {
          ptb2Sentences <- ptb2Sentences.get
          propbank1Sentences <- propbank1Sentences.get
          conllToPTB2SentenceAlignments <- conllToPTB2SentenceAlignments.get
        } yield (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentenceId = argId.verbId.sentenceId
          val verbIndex = argId.verbId.verbIndex
          val argIndex = argId.argument
          val sentence = data(sentenceId)
          val paddingIndices = sentence.paddingIndices

          val ptbSid = conllToPTB2SentenceAlignments(sentenceId)
          val ptbSent = ptb2Sentences(ptbSid)
          val propbankSent = propbank1Sentences(ptbSid)

          // val recalculatedPAStructures = 

          System.err.println(sentenceId)
          System.err.println(Text.render(sentence.tokens))
          System.err.println(sentence.tokens.size)
          System.err.println(verbIndex + s" (${sentence.tokens(verbIndex).token})")
          System.err.println(argIndex + s"(${sentence.tokens(argIndex).token})")

          props(sentenceId).predicateArgumentStructures.foreach(System.err.println)

          val indexRemapping = (0 to sentence.tokens.size).foldLeft(List.empty[Int]) {
            case (Nil, i) => i :: Nil
            case (prev :: rest, i) =>
              if(paddingIndices.contains(i)) prev :: prev :: rest
              else (prev + 1) :: prev :: rest
          }.reverse.toVector
          val span = props(sentenceId)
            .predicateArgumentStructures(indexRemapping(verbIndex))
            .arguments.map { case (_, span) =>
              val newBegin = indexRemapping(span.begin)
              val newEnd = indexRemapping(span.end)
              ESpan(newBegin, newEnd)
            }.find(_.contains(argIndex)).get

          // TODO fix printing code to verify sentence matching, span matching, and span index correction

          System.err.println("\n" + sentenceId)
          System.err.println(Text.render(sentence.tokens))
          System.err.println(s"PREDICATE: ${sentence.tokens(verbIndex).token} ($verbIndex)")
          val roleLabel = argRoleLabels(verbType)(argId).role
          System.err.println(s"$roleLabel: ${sentence.tokens(argId.argument).token}")
          System.err.println(Text.renderSpan(sentence.tokens, span))

          // System.err.println(s"Other args: $otherArgsString")
          // System.err.println(s"Contains another argument? " + (if(containsAnotherArg) "YES" else "NO"))
          // System.err.println(Text.renderSpan(sentence.tokens, blockedSpan))
          // System.err.println(Text.renderSpan(sentence.tokens, blockedSpan2))

          Map(span -> 1.0)
        }
      }.flatMap(x => x)
    ).data

    // cacheArgFeats("CoNLL 2008 arg spans borrowed from OntoNotes")(
    //   dataset.data.zip(RunData.splits).flatMap { case (data, split) =>
    //     (ontonotesAllData.get, ontonotesSentenceIndex.get).mapN { (ontonotes, ontonotesIndex) =>
    //     // (ontonotesFeatures.rawDataset(split), ontonotesSentenceIndices(split)).mapN { (ontonotes, ontonotesIndex) =>
    //       (verbType: String) => (argId: ArgumentId[Int]) => {
    //         val argIndex = argId.argument
    //         val sid = argId.verbId.sentenceId
    //         val sentence = data(sid)
    //         val matchedOntonotesPaths = sentence.tokens.foldMap(tok =>
    //           ontonotesIndex.get(tok.token.lowerCase).combineAll
    //         ).toVector.sortBy(-_._2).take(5)

    //         val sentText = Text.render(sentence.tokens)
    //         val topPath = matchedOntonotesPaths.head._1
    //         val topSentText = Text.render(ontonotes(topPath).tokens)

    //         if(topSentText == sentText) {
    //           System.err.print(".")
    //         } else {
    //           System.err.println("\n==========")
    //           System.err.println(sid)
    //           System.err.println(Text.render(sentence.tokens))
    //           matchedOntonotesPaths.foreach { case (path, count) =>
    //             System.err.println(s"$count: $path")
    //             val ontonotesSentence = ontonotes(path)
    //             System.err.println(Text.render(ontonotesSentence.tokens))
    //           }
    //           System.err.println()
    //         }


    //         Map(ESpan(0, 1) -> 1.0) // XXX
    //       }
    //     }
    //   }
    // ).data

    // cacheArgFeats("CoNLL 2008 inferred arg spans")(
    //   dataset.data.zip(argRoleLabels.data).map { case (data, roleLabels) =>
    //     (verbType: String) => (argId: ArgumentId[Int]) => {

    //       val argIndex = argId.argument
    //       val sid = argId.verbId.sentenceId
    //       val sentence = data(sid)
    //       val dependencies = sentence.childToParentDependencies
    //       val parentToChildren = dependencies.map(_._2).zipWithIndex
    //         .foldMap { case (parent, child) => Map(parent -> Set(child)) }

    //       @tailrec def closureSpan(fibers: Map[Int, Set[Int]], curSet: Set[Int]): ESpan = {
    //         val nextSet = curSet
    //           .unorderedFoldMap(i => fibers.get(i).combineAll)
    //           .union(curSet)
    //         if(curSet == nextSet) ESpan(curSet.min, curSet.max + 1)
    //         else closureSpan(fibers, nextSet)
    //       }
    //       val span = closureSpan(parentToChildren, Set(argId.argument))

    //       val otherArgIndices = sentence.predicateArgumentStructures
    //         .find(_.predicate.index == argId.verbId.verbIndex).get
    //         .arguments.map(_._2).filter(_ != argId.argument).toSet
    //       val otherArgsString = otherArgIndices.toList.sorted
    //         .map(i => s"${sentence.tokens(i).token} ($i)").mkString(", ")
    //       val containsAnotherArg = otherArgIndices.exists(span.contains)

    //       val verbIndex = argId.verbId.verbIndex
    //       val immediateBlockedIndices = otherArgIndices + verbIndex

    //       def followVerbChain(i: Int): Set[Int] = {
    //         val (label, parent) = dependencies(i)
    //         if(parent == argIndex) Set(i) // cut off this path at the base
    //         else if(label == "VC") followVerbChain(parent) + i // keep going
    //         else Set(i) // we're done
    //       }

    //       def followDepChain(i: Int): Set[Int] = {
    //         val (label, parent) = dependencies(i)
    //         if(parent == argIndex) Set(i) // cut off this path at the base
    //         else if(parent == -1) Set(i) // we're done
    //         else followDepChain(parent) + i // keep going
    //       }

    //       // go up in the tree a bit, yeeah. this gets us to the "actual" head
    //       val blockedIndices = immediateBlockedIndices.unorderedFoldMap(followDepChain)

    //       val lowerBound = blockedIndices.filter(_ < argIndex).toList.maximumOption.getOrElse(-1)
    //       val upperBound = blockedIndices.filter(_ > argIndex).toList.minimumOption.getOrElse(sentence.tokens.size)

    //       @tailrec def cullObsoleteUnaryChains(fibers: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    //         val obsoletes = fibers.toList.filter(_._2.isEmpty).map(_._1).toSet
    //         if(obsoletes.isEmpty) fibers
    //         else cullObsoleteUnaryChains(
    //           fibers.mapVals(_.filterNot(obsoletes.contains)) -- obsoletes
    //         )
    //       }

    //       val parentToAdmissibleChildren = parentToChildren.mapVals(_.filter(i => i > lowerBound && i < upperBound))
    //       val blockedSpan = closureSpan(parentToAdmissibleChildren, Set(argId.argument))

    //       val parentToAdmissibleChildren2 = cullObsoleteUnaryChains(
    //         parentToAdmissibleChildren
    //       )
    //       val blockedSpan2 = closureSpan(parentToAdmissibleChildren2, Set(argId.argument))

    //       if(!sid.startsWith("train") && containsAnotherArg) {
    //         System.err.println("\n" + sid)
    //         System.err.println(jjm.ling.Text.render(sentence.tokens))
    //         System.err.println(s"PREDICATE: ${sentence.tokens(verbIndex).token} ($verbIndex)")
    //         val roleLabel = roleLabels(verbType)(argId).role
    //         System.err.println(s"$roleLabel: ${sentence.tokens(argId.argument).token}")
    //         System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, span))

    //         System.err.println(s"Other args: $otherArgsString")
    //         System.err.println(s"Contains another argument? " + (if(containsAnotherArg) "YES" else "NO"))
    //         System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, blockedSpan))
    //         System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, blockedSpan2))
    //       }

    //       Map(span -> 1.0)
    //     }
    //   }
    // ).data
  }

  override val argSemanticHeadIndices: ArgFeats[Int] = {
    dataset.data.map { data =>
      (verbType: String) => (argId: ArgumentId[Int]) => {
        val sentence = data(argId.verbId.sentenceId)
        val dependencies = sentence.childToParentDependencies
        val argIndex = argId.argument

        val validPrepPOS = Set("IN", "TO", "RB")
        // together these other POS tags should cover everything interesting. they can stay heads
        // val invalidPrepPOS = Set("VBG", "VBN", "VBD", "RP", "JJ")

        // if(dependencies.contains("PMOD" -> argIndex)) {
        //   if(!(validPrepPOS ++ invalidPrepPOS).contains(sentence.tokens(argIndex).pos)) {
        //     System.err.println(jjm.ling.Text.render(sentence.tokens))
        //     System.err.println(s"$argIndex: ${sentence.tokens(argIndex)}")
        //     val pmodIndex = dependencies.indexOf("PMOD" -> argIndex)
        //     System.err.println(s"$pmodIndex: ${sentence.tokens(pmodIndex)}")
        //   }
        // }

        val validToPOS = Set[String]("TO")
        // TO is the only pos that appears with IM dependents
        // val invalidToPOS = Set[String]()

        // if(dependencies.contains("IM" -> argIndex)) {
        //   if(!(validToPOS ++ invalidToPOS).contains(sentence.tokens(argIndex).pos)) {
        //     System.err.println(jjm.ling.Text.render(sentence.tokens))
        //     System.err.println(s"$argIndex: ${sentence.tokens(argIndex)}")
        //     val imIndex = dependencies.indexOf("IM" -> argIndex)
        //     System.err.println(s"$imIndex: ${sentence.tokens(imIndex)}")
        //   }
        // }

        Option(dependencies.indexOf("PMOD" -> argIndex))
          .filter(_ >= 0)
          .filter(i => validPrepPOS.contains(sentence.tokens(i).pos))
          .orElse(
            Option(dependencies.indexOf("IM" -> argIndex))
              .filter(_ >= 0)
              .filter(i => validToPOS.contains(sentence.tokens(i).pos))
          ).getOrElse(argIndex)
      }
    }
  }
}
