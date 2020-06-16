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

  val predArgStructures: RunDataCell[Map[String, NonMergingMap[VerbId, PredicateArgumentStructure]]] = {
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
    import qfirst.conll08.HasLemma.ops._
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

  override def argSpans: ArgFeats[Map[ESpan, Double]] = ???

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
