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

  import qfirst.ontonotes._

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

  val dataset: RunDataCell[Map[String, NonMergingMap[VerbId, (String, Map[ESpan, String])]]] = RunData.strings.zip(index.data)
    .flatMap { case (split, filePaths) =>
      filePaths.infoBarFoldMapM(s"Reading PropBank files to construct instances ($split)") { path =>
        Log.trace(path.suffix) >> ontonotesService.getFile(path).map { file =>
          file.sentences.foldMap { sentence =>
            val sentenceId = sentence.path.toString
            sentence.predicateArgumentStructures.foldMap { pas =>
              val verbLemma = pas.predicate.lemma
              val verbSense = s"$verbLemma.${pas.predicate.sense}"
              val verbType = if(assumeGoldVerbSense) verbSense else verbLemma
              Map(
                verbType -> NonMergingMap(
                  VerbId(sentenceId, pas.predicate.index) ->
                    (verbSense -> pas.arguments
                       .filter { case (label, span) => PropBankRoleLabel.isArgRelevant(pas.predicate, label, span) }
                       .map(_.swap).toMap)
                )
              )
            }
          }
        }
      }
    }.toCell("PropBank dataset")

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

  override val verbArgSets = dataset.data.map(
    _.mapVals { verbs =>
      verbs.value.map { case (verbId, (_, labels)) =>
        verbId -> labels.keySet
      }
    }
  ).toCell("PropBank gold span instances")

  override val argQuestionDists: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    RunData.strings.zip(verbIdToType.data).flatMap { case (split, vidToType) =>
      val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
      FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
        .map { case QGen.SentencePrediction(sid, _, verbs) =>
          verbs.foldMap { case QGen.VerbPrediction(vi, spans) =>
            val verbId = VerbId(sid, vi)
            val verbType = vidToType.value(verbId)
            Map(
              verbType -> NonMergingMap(
                spans.map { case QGen.SpanPrediction(span, _, questions) =>
                  ArgumentId(verbId, span) -> questions
                }.toMap
              )
            )
          }
        }
        .infoCompile(s"Reading QG Predictions ($split)")(_.foldMonoid)
    }.toCell("Question distributions for arguments")
  }

  override val argSpans: ArgFeats[Map[ESpan, Double]] = dataset.data.map(
    _.mapVals { verbs =>
      verbs.value.toList.foldMap { case (verbId, (framesetId, arguments)) =>
        NonMergingMap(
          arguments.map { case (span, _) =>
            ArgumentId(verbId, span) -> Map(span -> 1.0)
          }
        )
      }
    }
  ).toCell("PropBank span to role label mapping").data

  override def argSemanticHeadIndices: ArgFeats[Int] = ???

  override def argSyntacticFunctions: CachedArgFeats[String] = ???

  override def argSyntacticFunctionsConverted: CachedArgFeats[String] = ???

  override def argRoleLabels: CachedArgFeats[PropBankRoleLabel] = dataset.data.map(
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

  // TODO move this to main Features

  @JsonCodec case class PropBankQGInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbEntries: Map[Int, PropBankQGVerbInput]
  )
  @JsonCodec case class PropBankQGVerbInput(
    verbIndex: Int,
    argumentSpans: Set[ESpan]
  )

  val qgInputs = index.data.map { paths =>
    Stream.emits[IO, CoNLLPath](paths) >>= { path =>
      Stream.eval(ontonotesService.getFile(path)) >>= { file =>
        Stream.emits[IO, PropBankQGInput](
          file.sentences.map { sentence =>
            PropBankQGInput(
              sentence.path.toString,
              sentence.tokens.map(_.token).toVector,
              sentence.predicateArgumentStructures.map { pas =>
                pas.predicate.index -> PropBankQGVerbInput(pas.predicate.index, pas.arguments.map(_._2).toSet)
              }.toMap
            )
          }
        )
      }
    }
  }

  def getQGInputOutPath(split: String) = outDir
    .map(_.resolve(s"qg-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  val writeQGInputs = RunData.strings.zip(qgInputs).flatMap { case (split, inputStream) =>
    getQGInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"QG Inputs already found at $outPath."),
        Log.infoBranch(s"Logging QG inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  override def setup = super.setup >> writeQGInputs
}
