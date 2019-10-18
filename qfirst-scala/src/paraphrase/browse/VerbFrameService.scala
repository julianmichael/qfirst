package qfirst.paraphrase.browse
import qfirst.paraphrase._

import qasrl.labeling.SlotBasedLabel
import qasrl.data.VerbEntry

import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import io.circe.generic.JsonCodec

@JsonCodec case class ParaphrasingInfo(
  sentenceId: String,
  verbIndex: Int,
  verbEntry: VerbEntry,
  verbModel: VerbClusterModel,
  goldParaphrases: VerbParaphraseLabels
)

trait VerbFrameService[F[_]] { self =>
  def getVerbs: F[Map[InflectedForms, Int]]
  def getModel(verb: InflectedForms): F[VerbClusterModel]
  def getParaphrasingInfo(i: Int): F[ParaphrasingInfo]
  def saveParaphraseAnnotations(
    sentenceId: String, verbIndex: Int, paraphrases: VerbParaphraseLabels
  ): F[VerbParaphraseLabels]
}
object VerbFrameService {
  @JsonCodec sealed trait Request { type Out }
  object Request {
    case object GetVerbs extends Request { type Out = Map[InflectedForms, Int] }
    @JsonCodec case class GetModel(verb: InflectedForms) extends Request { type Out = VerbClusterModel }
    @JsonCodec case class GetParaphrasingInfo(i: Int) extends Request { type Out = ParaphrasingInfo }
    @JsonCodec case class SaveParaphraseAnnotations(
      sentenceId: String,
      verbIndex: Int,
      paraphrases: VerbParaphraseLabels
    ) extends Request { type Out = VerbParaphraseLabels }
  }
}

// case class VerbFrameServiceIO(
//   inflectionCounts: Map[InflectedForms, Int],
//   verbModels: Map[InflectedForms, VerbClusterModel],
//   dataset: Dataset,
//   getEvaluationItem: Int => (InflectedForms, String, Int), // sentence ID, verb index
//   paraphraseStoreRef: Ref[IO, ParaphraseAnnotations],
//   saveParaphrases: ParaphraseAnnotations => IO[Unit]
// ) extends VerbFrameService[IO] {
// 
//   def getVerbs: IO[Map[InflectedForms, Int]] =
//     IO.pure(inflectionCounts)
// 
//   def getModel(verb: InflectedForms): IO[VerbClusterModel] =
//     IO(verbModels(verb))
// 
//   def getParaphrasingInfo(i: Int): IO[ParaphrasingInfo] = {
//     val (verbInflectedForms, sentenceId, verbIndex) = getEvaluationItem(i)
//     paraphraseStoreRef.get.map(paraphrases =>
//       ParaphrasingInfo(
//         sentenceId, verbIndex,
//         dataset.sentences(sentenceId).verbEntries(verbIndex),
//         verbModels(verbInflectedForms),
//         paraphrases.get(sentenceId).flatMap(_.get(verbIndex)).getOrElse(VerbParaphraseLabels.empty)
//       )
//     )
//   }
// 
//   def saveParaphraseAnnotations(
//     sentenceId: String, verbIndex: Int, paraphrases: VerbParaphraseLabels
//   ): IO[VerbParaphraseLabels] = {
//     def updateFn(p: ParaphraseAnnotations) = {
//       val newSentenceLabels = p.getOrElse(sentenceId, Map()) + (verbIndex -> paraphrases)
//       p + (sentenceId -> newSentenceLabels)
//     }
//     for {
//       _ <- paraphraseStoreRef.update(updateFn)
//       store <- paraphraseStoreRef.get
//       _ <- saveParaphrases(store)
//     } yield store.getOrElse(sentenceId, Map()).getOrElse(verbIndex, VerbParaphraseLabels.empty)
//   }
// }
