// package qfirst.frames.verbal
// 
// import qfirst.frames.ArgumentSlot
// import qfirst.frames.Frame
// 
// import qasrl.bank.DataIndex
// import qasrl.bank.Document
// import qasrl.bank.DocumentId
// import qasrl.bank.Domain
// import qasrl.bank.SentenceId
// 
// import qasrl.data.Dataset
// 
// import nlpdata.util.LowerCaseStrings._
// import nlpdata.datasets.wiktionary.InflectedForms
// 
// import cats.implicits._
// import cats.effect._
// import cats.effect.concurrent.Ref
// 
// import org.http4s._
// import org.http4s.circe._
// import org.http4s.implicits._
// 
// import scala.concurrent.ExecutionContext.Implicits.global
// 
// import io.circe.generic.JsonCodec
// 
// import monocle.macros._
// import monocle.function.{all => Optics}
// 
// import qasrl.bank.JsonCodecs._
// import qasrl.bank.service.JsonCodecs._
// import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
// 
// import io.circe.syntax._
// 
// object VerbAnnotationHttpService {
// 
//   def make(service: VerbAnnotationServiceIO) = {
// 
//     import io.circe.Encoder
// 
//     implicit val verbCountsEncoder =
//       implicitly[Encoder[List[(InflectedForms, Int)]]]
//         .contramap[Map[InflectedForms, Int]](_.toList)
// 
//     implicit val inflectedFormsEntityDecoder = jsonOf[IO, InflectedForms]
//     implicit val verbCountsEntityEncoder = jsonEncoderOf[IO, Map[InflectedForms, Int]]
//     implicit val verbFrameEntityEncoder = jsonEncoderOf[IO, VerbFrame]
//     implicit val verbFrameEntityDecoder = jsonOf[IO, VerbFrame]
// 
//     import org.http4s.dsl.io._
// 
//     HttpRoutes.of[IO] {
//       case POST -> Root / "getVerbs" =>
//         service.getVerbs.flatMap(Ok(_))
//       case req @ POST -> Root / "getFrame" =>
//         req.as[InflectedForms].flatMap(service.getFrame).flatMap(Ok(_))
//       case req @ POST -> Root / "saveFrame" =>
//         req.as[VerbFrame].flatMap(service.saveFrame).flatMap(Ok(_))
//     }
//   }
// }
