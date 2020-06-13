package qfirst.frame.features

import qfirst.frame._
import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached
import qfirst.frame.util.NonMergingMap
import qfirst.frame.util.VectorFileUtil

import java.nio.file._

import jjm.ling.ESpan
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

class CoNLL08Features(
  mode: RunMode,
  assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[Int](mode, assumeGoldVerbSense)(cs, Log) {

  import qfirst.conll08._

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
          val predPos = sentence.tokens(pas.predicate.index).pos
          PTBPosTags.verbs.contains(predPos)
        }
    )
  }

  def keepOnlyCommonPredicates(sentence: CoNLL08Sentence, lemmasToKeep: Set[String]) = {
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .filter { pas =>
          lemmasToKeep.contains(pas.predicate.lemma)
        }
    )
  }

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
          // NOTE: uncomment the bits below to see edge case phenomena (assuming verbal predicates).
          // They're rare and I think they're mostly annotation mistakes.
          // Normally pred/arg indices coinciding, or SU roles, are used for non-verbal predicates.

          // there are a few cases of this remaning for verbal predicates.
          // AFAICT they are annotation mistakes in gold.
          // if(pas.arguments.map(_._2).contains(pas.predicate.index)) {
          //   System.err.println("Argument coincides with predicate: " + pas.toString)
          //   System.err.println("Argument coincides with predicate: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // there's a (less) small number of these. I assume we should keep them..
          // if(pas.arguments.map(_._1).contains("SU")) {
          //   System.err.println("SU argument: " + pas.toString)
          //   System.err.println("SU argument: " + jjm.ling.Text.render(sentence.tokens))
          // }

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

  val predArgStructures: RunDataCell[VerbFeats[PredicateArgumentStructure]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbId = VerbId(sid, pas.predicate.index)
          Map(verbType -> NonMergingMap(Map(verbId -> pas))) 
        }
      }
    ).toCell("CoNLL 2008 predicate-argument structures")
  }

  override val verbSenseLabels = predArgStructures.data.map { paStructs =>
    (verbType: String) => (verbId: VerbId) => {
      val predicate = paStructs(verbType).value(verbId).predicate
      s"${predicate.lemma}.${predicate.sense}"
    }
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

  val argSyntacticFunctions: RunDataCell[ArgFeats[String]] = {
    import qfirst.conll08.HasLemma.ops._
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        val dependencies = sentence.childToParentDependencies
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma

          val verbIndex = pas.predicate.index
          val verbId = VerbId(sid, verbIndex)

          // commented-out stuff here was for producing more detailed argument keys
          // but, these aren't necessary for the syntactic function baseline I want to use

          // val verbChildDepLabels = dependencies.filter(_._2 == verbIndex).map(_._1).toSet
          // val isPassive = verbChildDepLabels.contains("LGS") || (
          //   !sentence.tokens(verbIndex).token.endsWith("ing") &&
          //     dependencies(verbIndex)._1 == "VC" &&
          //     sentence.tokens(dependencies(verbIndex)._2).lemma == "be"
          // )
          // val passive = if(isPassive) "pss" else ""

          val argSyntacticFs = pas.arguments.map(_._2).map { argIndex =>
            ArgumentId(verbId, argIndex) -> {
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
              dependencies(argIndex)._1
            }
          }.toMap

          Map(verbType -> NonMergingMap(argSyntacticFs))
        }
      }
    ).toCell("CoNLL 2008 argument-to-head syntactic rels")
  }

  val argDependencyPaths: RunDataCell[ArgFeats[String]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        val dependencies = sentence.childToParentDependencies
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbIndex = pas.predicate.index
          val verbId = VerbId(sid, verbIndex)
          val argDependencyPaths = pas.arguments.map(_._2).map { argIndex =>
            val predPathToRoot = getDependencyPathToRoot(dependencies, verbIndex)
            val argPathToRoot = getDependencyPathToRoot(dependencies, argIndex)
            val predPathToLCA = predPathToRoot.takeWhile { case (_, i) =>
              i != argIndex && !argPathToRoot.exists(_._2 == i)
            }
            val argPathToLCA = argPathToRoot.takeWhile { case (_, i) =>
              i != argIndex && !predPathToRoot.exists(_._2 == i)
            }
            val pathStr = predPathToLCA.mkString("->") + "*" + argPathToLCA.reverse.mkString("<-")
            ArgumentId(verbId, argIndex) -> pathStr
          }.toMap

          Map(verbType -> NonMergingMap(argDependencyPaths))
        }
      }
    ).toCell("CoNLL 2008 predicate-to-argument dependency paths")
  }

  override val verbArgSets: RunDataCell[VerbFeats[Set[Int]]] = predArgStructures.data.map(
    mapVerbFeats(_.arguments.map(_._2).toSet)
  ).toCell("CoNLL 2008 gold arg indices")

  override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
    RunData.strings.map(_ => ???).toCell(
      "[ERROR] no question distributions for CoNLL 2008 data."
    )
  }

  override val argSpans: RunDataCell[ArgFeats[Map[ESpan, Double]]] =
    RunData.strings.map(_ => ???).toCell(
      "[ERROR] no spans for CoNLL 2008 data."
    )

  override val argIndices: RunData[ArgFeatsNew[Int]] = {
    RunData.strings.map(_ =>
      (verbType: String) => (argId: ArgumentId[Int]) => argId.argument
    )
  }

  var allRoleLabels = Set.empty[String]

  override val argRoleLabels: RunDataCell[ArgFeats[PropBankRoleLabel]] =
    predArgStructures.data.map(
      makeArgFeats { case (verbId, pas) =>
        pas.arguments.map { case (roleLabel, index) =>
          allRoleLabels = allRoleLabels + roleLabel
          index -> PropBankRoleLabel(pas.predicate.sense, roleLabel)
        }
      }
    ).toCell("PropBank arg to role label mapping")

  // TODO refactor this, other features, gen code, and setup function into the main trait,
  // so it's all done smoothly for the cases with none of those features.

  @JsonCodec case class MLMFeatureGenInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbs: Map[String, Set[Int]],
    argsByVerb: Map[String, Set[Int]]
  )

  val mlmFeatureGenInputs = dataset.data.map { sentences =>
    Stream.emits[IO, (String, CoNLL08Sentence)](sentences.value.toList).map { case (sid, sentence) =>
      MLMFeatureGenInput(
        sentenceId = sid,
        sentenceTokens = sentence.tokens.map(_.token),
        verbs = sentence.predicateArgumentStructures.foldMap(pas =>
          Map(pas.predicate.lemma -> Set(pas.predicate.index))
        ),
        argsByVerb = sentence.predicateArgumentStructures.foldMap(pas =>
          Map(pas.predicate.lemma -> pas.arguments.map(_._2).toSet)
        )
      )
    }
  }

  def getMLMFeatureInputOutPath(split: String) = outDir
    .map(_.resolve(s"mlm-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  val writeMLMInputs = RunData.strings.zip(mlmFeatureGenInputs).flatMap { case (split, inputStream) =>
    getMLMFeatureInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"MLM feature inputs already found at $outPath."),
        Log.infoBranch(s"Logging MLM feature inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  override val setup = writeMLMInputs

}
