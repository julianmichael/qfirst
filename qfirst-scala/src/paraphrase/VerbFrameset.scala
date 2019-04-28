package qfirst.paraphrase
import qfirst.ClauseResolution

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import qasrl.bank.SentenceId
import qasrl.bank.JsonCodecs._

import qasrl._
import qasrl.labeling.SlotBasedLabel
import qfirst.ClauseResolution.ArgStructure

import io.circe.generic.JsonCodec

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm

import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}

import monocle.macros._

@JsonCodec sealed trait ParaphrasingFilter {
  def getParaphrases(
    frameset: VerbFrameset,
    frameDistribution: Vector[Double],
    structure: (ArgStructure, ArgumentSlot)
  ): Set[(ArgStructure, ArgumentSlot)] = {
    val scoredParaphrases = frameset.frames.zip(frameDistribution).maxBy(_._2)._1.getScoredParaphrases(structure)
    val adverbialParaphrases = structure._2 match {
      case adv @ Adv(_) => filterScoredParaphrases(
        scoredParaphrases.map { case ((clauseTemplate, _), (clauseProb, _)) => (clauseTemplate -> adv) -> (clauseProb -> 1.0) }
      )
      case _ => Set()
    }
    (filterScoredParaphrases(scoredParaphrases) ++ adverbialParaphrases).filter(_ != structure)
  }
  def filterScoredParaphrases(
    scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
  ): Set[(ArgStructure, ArgumentSlot)]
  def ignoreCoindexing: ParaphrasingFilter
}

object ParaphrasingFilter {
  @Lenses @JsonCodec case class TwoThreshold(
    clauseThreshold: Double, coindexingThreshold: Double
  ) extends ParaphrasingFilter {
    def filterScoredParaphrases(
      scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
    ): Set[(ArgStructure, ArgumentSlot)] = {
      scoredParaphrases.filter(p => p._2._1 >= clauseThreshold && p._2._2 >= coindexingThreshold).keySet
    }
    def ignoreCoindexing: TwoThreshold = this.copy(coindexingThreshold = 0.0)
  }
  object TwoThreshold

  @Lenses @JsonCodec case class OneThreshold(
    threshold: Double
  ) extends ParaphrasingFilter {
    def filterScoredParaphrases(
      scoredParaphrases: Map[(ArgStructure, ArgumentSlot), (Double, Double)]
    ): Set[(ArgStructure, ArgumentSlot)] =
      scoredParaphrases.filter(p => (p._2._1 * p._2._2) >= threshold).keySet
    def ignoreCoindexing: OneThreshold = this
  }
  object OneThreshold

  val oneThreshold = GenPrism[ParaphrasingFilter, OneThreshold]
  val twoThreshold = GenPrism[ParaphrasingFilter, TwoThreshold]
}

@Lenses @JsonCodec case class FrameClause(
  args: ArgStructure,
  probability: Double)
object FrameClause

@Lenses case class VerbFrame(
  clauseTemplates: List[FrameClause],
  coindexingScores: Map[((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double],
  probability: Double) {

  def getScoredParaphrases(
    structure: (ArgStructure, ArgumentSlot)
  ): Map[(ArgStructure, ArgumentSlot), (Double, Double)] = {
    clauseTemplates.flatMap { frameClause =>
      val clauseScore = frameClause.probability
      frameClause.args.args.keys.toList.map { otherSlot =>
        import scala.language.existentials
        val otherStructure = frameClause.args -> otherSlot
        val coindexingScore = coindexingScores.getOrElse(structure -> otherStructure, 0.0)
        otherStructure -> (clauseScore -> coindexingScore)
      }
    }.toMap
  }
}
object VerbFrame {
  import io.circe.{Encoder, Decoder}
  private[this] def fromListy(
    clauseTemplates: List[FrameClause],
    coindexingScores: List[(((ArgStructure, ArgumentSlot), (ArgStructure, ArgumentSlot)), Double)],
    probability: Double
  ) = VerbFrame(clauseTemplates, coindexingScores.toMap, probability)
  implicit val verbFrameDecoder: Decoder[VerbFrame] =
    Decoder.forProduct3("clauseTemplates", "coindexingScores", "probability")(fromListy)
  implicit val verbFrameEncoder: Encoder[VerbFrame] =
    Encoder.forProduct3("clauseTemplates", "coindexingScores", "probability")(x =>
      (x.clauseTemplates, x.coindexingScores.toList, x.probability)
    )
}

@Lenses @JsonCodec case class VerbId(
  sentenceId: String, verbIndex: Int
)
object VerbId {
  import io.circe._
  implicit val verbIdKeyEncoder = KeyEncoder.instance[VerbId](vid =>
    s"${vid.sentenceId}:${vid.verbIndex}"
  )
  implicit val verbIdKeyDecoder = KeyDecoder.instance[VerbId](s =>
    scala.util.Try(VerbId(s.reverse.dropWhile(_ != ':').tail.reverse, s.reverse.takeWhile(_ != ':').reverse.toInt)).toOption
  )
}

@Lenses @JsonCodec case class VerbFrameset(
  inflectedForms: InflectedForms,
  frames: List[VerbFrame],
  instances: Map[VerbId, Vector[Double]]
)
object VerbFrameset
