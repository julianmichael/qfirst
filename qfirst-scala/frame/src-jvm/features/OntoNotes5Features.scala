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

import qfirst.clause.ArgStructure
import qfirst.clause.ClauseResolution

import freelog._
import freelog.implicits._

// verb type is either lemma or sense, depending on assumeGoldVerbSense value (false/true resp.).
class OntoNotes5Features(
  mode: RunMode,
  assumeGoldVerbSense: Boolean
  // val filterPropBankRoles: Boolean // TODO do this in a reasonable way
)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[ESpan](mode, assumeGoldVerbSense)(cs, Log) {

  import qfirst.datasets.ontonotes._

  override val rootDir = Paths.get("frame-induction/ontonotes5")

  val ontonotesPath = Paths.get("data/conll-formatted-ontonotes-5.0")

  val ontonotesService = new CoNLLFileSystemService(ontonotesPath)

  val fullIndex = Cell("OntoNotes Index")(
    Log.infoBranch("Reading OntoNotes file paths")(
      ontonotesService.getAllPaths
    )
  )

  val index: RunDataCell[List[CoNLLPath]] = RunData(
    train = "train",
    dev = "development",
    test = "test").flatMap(
    spec => fullIndex.get.map(_.filter(_.split == spec))
  ).toCell("OntoNotes Index")

  val rawDataset: RunDataCell[NonMergingMap[String, CoNLLSentence]] = RunData.strings.zip(index.data)
    .flatMap { case (split, filePaths) =>
      filePaths.infoBarFoldMapM(s"Reading PropBank files to construct instances ($split)") { path =>
        Log.trace(path.suffix) >> ontonotesService.getFile(path).map { file =>
          NonMergingMap(file.sentences.map(s => s.path.toString -> s).toMap)
        }
      }
    }.toCell("Raw PropBank dataset")

  val verbSenseAndArgs: CachedVerbFeats[(String, Map[ESpan, String])] = RunData.strings.zip(index.data)
    .flatMap { case (split, filePaths) =>
      filePaths.infoBarFoldMapM(s"Reading PropBank files to construct instances ($split)") { path =>
        Log.trace(path.suffix) >> ontonotesService.getFile(path).map { file =>
          file.sentences.foldMap { sentence =>
            val sentenceId = sentence.path.toString
            sentence.predicateArgumentStructures
              .filter(pas =>
                jjm.ling.en.PTBPosTags.verbs.contains(
                  sentence.tokens(pas.predicateIndex).pos
                )) // keep only verbal predicates
              .foldMap { pas =>
              val verbLemma = pas.predicate.lemma
              val verbSense = s"$verbLemma.${pas.predicate.sense}"
              val verbType = if(assumeGoldVerbSense) verbSense else verbLemma
              Map(
                verbType -> NonMergingMap(
                  VerbId(sentenceId, pas.predicateIndex) ->
                    (verbSense -> pas.arguments
                       .filter { case (label, span) => PropBankRoleLabel.isArgRelevant(pas.predicateIndex, pas.predicate, label, span) }
                       .map(_.swap).toMap)
                )
              )
            }
          }
        }
      }
    }.toCell("PropBank verb senses and args")

  // TODO: fill this in after adding sense information to `dataset`
  override def verbSenseLabels = ???

  // TODO eliminate redundant traversal of propbank
  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    index.data.flatMap(
      _.infoBarFoldMapM("Constructing sentence index") { path =>
        ontonotesService.getFile(path).map(
          _.sentences.foldMap(s => NonMergingMap(s.path.toString -> s.tokens.map(_.token).toVector))
        )
      }
    ).toCell("Sentence index")

  override val verbArgSets = verbSenseAndArgs.data.map(
    _.mapVals { verbs =>
      verbs.value.map { case (verbId, (_, labels)) =>
        verbId -> labels.keySet
      }
    }
  ).toCell("PropBank gold span instances")

  override val argSpans: CachedArgFeats[Map[ESpan, Double]] = verbSenseAndArgs.data.map(
    _.mapVals { verbs =>
      verbs.value.toList.foldMap { case (verbId, (framesetId, arguments)) =>
        NonMergingMap(
          arguments.map { case (span, _) =>
            ArgumentId(verbId, span) -> Map(span -> 1.0)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping")

  override def argSemanticHeadIndices: CachedArgFeats[Int] = ???

  override def argSyntacticFunctions: CachedArgFeats[String] = ???

  override def argSyntacticFunctionsConverted: CachedArgFeats[String] = ???

  override def argRoleLabels: CachedArgFeats[PropBankRoleLabel] = verbSenseAndArgs.data.map(
    _.mapVals { verbs =>
      verbs.value.toList.foldMap { case (verbId, (framesetId, arguments)) =>
        NonMergingMap(
          arguments.map { case (span, label) =>
            ArgumentId(verbId, span) -> PropBankRoleLabel(framesetId, label)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping")

  override val qgInputs: RunData[Stream[IO, PropBankQGInput]] = index.data.map { paths =>
    Stream.emits[IO, CoNLLPath](paths) >>= { path =>
      Stream.eval(ontonotesService.getFile(path)) >>= { file =>
        Stream.emits[IO, PropBankQGInput](
          file.sentences.map { sentence =>
            PropBankQGInput(
              sentence.path.toString,
              sentence.tokens.map(_.token).toVector,
              sentence.predicateArgumentStructures.map { pas =>
                pas.predicateIndex -> PropBankQGVerbInput(pas.predicateIndex, pas.arguments.map(_._2).toSet)
              }.toMap
            )
          }
        )
      }
    }
  }
}
