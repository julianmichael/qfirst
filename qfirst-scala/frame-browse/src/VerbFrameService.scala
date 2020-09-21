package qfirst.frame.browse
import qfirst.frame._

import cats.effect.IO
import cats.effect.concurrent.Ref

import qasrl.labeling.SlotBasedLabel
import qasrl.data.Dataset
import qasrl.data.VerbEntry

import jjm.DotKleisli
import jjm.DotFunctionK
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

@JsonCodec case class ParaphrasingInfo(
  sentenceId: String,
  verbIndex: Int,
  verbEntry: VerbEntry,
  verbModel: VerbClusterModel[InflectedForms, ClausalQuestion]
  // goldParaphrases: VerbParaphraseLabels
)

case class VerbFrameService[F[_]](
  f: DotKleisli[F, VerbFrameService.Request]) { self =>
  import VerbFrameService._
  def getVerbs: F[Map[InflectedForms, Int]] = f(GetVerbs)
  def getModel(verb: InflectedForms): F[VerbClusterModel[InflectedForms, ClausalQuestion]] = f(GetModel(verb))
  // def getParaphrasingInfo(i: Int): F[ParaphrasingInfo] = f(GetParaphrasingInfo(i))
  // def saveParaphraseAnnotations(
  //   sentenceId: String, verbIndex: Int, paraphrases: VerbParaphraseLabels
  // ): F[VerbParaphraseLabels] = f(SaveParaphraseAnnotations(sentenceId, verbIndex, paraphrases))
}
object VerbFrameService {
  @JsonCodec sealed trait Request { type Out }
  case object GetVerbs extends Request { type Out = Map[InflectedForms, Int] }
  @JsonCodec case class GetModel(verb: InflectedForms) extends Request { type Out = VerbClusterModel[InflectedForms, ClausalQuestion] }
    // @JsonCodec case class GetParaphrasingInfo(i: Int) extends Request { type Out = ParaphrasingInfo }
    // @JsonCodec case class SaveParaphraseAnnotations(
    //   sentenceId: String,
    //   verbIndex: Int,
    //   paraphrases: VerbParaphraseLabels
    // ) extends Request { type Out = VerbParaphraseLabels }

  object Request {
    implicit val verbFrameServiceRequestDotEncoder = new DotKleisli[Encoder, Request] {
      def apply(req: Request): Encoder[req.Out] = req match {
        case GetVerbs => implicitly[Encoder[List[(InflectedForms, Int)]]]
            .contramap[Map[InflectedForms, Int]](_.toList).asInstanceOf[Encoder[req.Out]]
        case GetModel(_) => implicitly[Encoder[VerbClusterModel[InflectedForms, ClausalQuestion]]].asInstanceOf[Encoder[req.Out]]
        // case GetParaphrasingInfo(_) => implicitly[Encoder[ParaphrasingInfo]].asInstanceOf[Encoder[req.Out]]
        // case SaveParaphraseAnnotations(_, _, _) => implicitly[Encoder[VerbParaphraseLabels]].asInstanceOf[Encoder[req.Out]]
      }
    }
    implicit val verbFrameServiceRequestDotDecoder = new DotKleisli[Decoder, Request] {
      def apply(req: Request): Decoder[req.Out] = req match {
        case GetVerbs => implicitly[Decoder[List[(InflectedForms, Int)]]]
            .map(_.toMap).asInstanceOf[Decoder[req.Out]]
        case GetModel(_) => implicitly[Decoder[VerbClusterModel[InflectedForms, ClausalQuestion]]].asInstanceOf[Decoder[req.Out]]
        // case GetParaphrasingInfo(_) => implicitly[Decoder[ParaphrasingInfo]].asInstanceOf[Decoder[req.Out]]
        // case SaveParaphraseAnnotations(_, _, _) => implicitly[Decoder[VerbParaphraseLabels]].asInstanceOf[Decoder[req.Out]]
      }
    }
  }

  def basicIOService(
    inflectionCounts: Map[InflectedForms, Int],
    verbModels: Map[InflectedForms, VerbClusterModel[InflectedForms, ClausalQuestion]],
    dataset: Dataset
    // getEvaluationItem: Int => (InflectedForms, String, Int) // sentence ID, verb index
    // paraphraseStoreRef: Ref[IO, ParaphraseAnnotations],
    // saveParaphrases: ParaphraseAnnotations => IO[Unit]
  ): DotKleisli[IO, Request]  = DotKleisli.fromFunctionK(
    new DotFunctionK[IO, Request] {
      def apply[A](req: Request { type Out = A }): IO[A] = {
        val res = req match {
          case GetModel(verb) => IO(verbModels(verb)): IO[VerbClusterModel[InflectedForms, ClausalQuestion]]
          case GetVerbs => IO.pure(inflectionCounts): IO[Map[InflectedForms, Int]]
          // case GetParaphrasingInfo(i) => {
          //   val (verbInflectedForms, sentenceId, verbIndex) = getEvaluationItem(i)
          //   // paraphraseStoreRef.get.map(paraphrases =>
          //   IO.pure(
          //     ParaphrasingInfo(
          //       sentenceId, verbIndex,
          //       dataset.sentences(sentenceId).verbEntries(verbIndex),
          //       verbModels(verbInflectedForms)
          //       // paraphrases.get(sentenceId).flatMap(_.get(verbIndex)).getOrElse(VerbParaphraseLabels.empty)
          //     )
          //   )
          //   // ): IO[ParaphrasingInfo]

          // }
          // case SaveParaphraseAnnotations(sentenceId, verbIndex, paraphrases) => {
          //   def updateFn(p: ParaphraseAnnotations) = {
          //     val newSentenceLabels = p.getOrElse(sentenceId, Map()) + (verbIndex -> paraphrases)
          //     p + (sentenceId -> newSentenceLabels)
          //   }
          //   for {
          //     _ <- paraphraseStoreRef.update(updateFn)
          //     store <- paraphraseStoreRef.get
          //     _ <- saveParaphrases(store)
          //   } yield store.getOrElse(sentenceId, Map()).getOrElse(verbIndex, VerbParaphraseLabels.empty)
          // }: IO[VerbParaphraseLabels]
        }
        res.asInstanceOf[IO[A]]
      }
    }
  )
}
