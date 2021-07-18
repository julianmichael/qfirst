// package qfirst.frames.verbal
// 
// import scala.concurrent.Future
// 
// import jjm.ling.en.InflectedForms
// 
// import io.circe.parser.decode
// import io.circe.syntax._
// 
// case class VerbAnnotationClient(apiUrl: String) extends VerbAnnotationService[Future] {
// 
//   import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
//   import scala.concurrent.ExecutionContext.Implicits.global
//   val printer = io.circe.Printer.noSpaces
// 
//   def getVerbs: Future[Map[InflectedForms, Int]] = {
//     import io.circe.generic.auto._
//     org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/getVerbs", data = printer.pretty(().asJson)).map(_.responseText).flatMap { jsonStr =>
//       decode[List[(InflectedForms, Int)]](jsonStr) match {
//         case Left(err)  => Future.failed[Map[InflectedForms, Int]](new RuntimeException(err))
//         case Right(res) => Future.successful(res.toMap)
//       }
//     }
//   }
// 
//   def getFrame(inflectedForms: InflectedForms): Future[VerbFrame] = {
//     org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/getFrame", data = printer.pretty(inflectedForms.asJson)).map(_.responseText).flatMap { jsonStr =>
//       decode[VerbFrame](jsonStr) match {
//         case Left(err)  => Future.failed[VerbFrame](new RuntimeException(err))
//         case Right(res) => Future.successful(res)
//       }
//     }
//   }
// 
//   def saveFrame(frame: VerbFrame): Future[VerbFrame] = {
//     org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/saveFrame", data = printer.pretty(frame.asJson)).map(_.responseText).flatMap { jsonStr =>
//       decode[VerbFrame](jsonStr) match {
//         case Left(err)  => Future.failed[VerbFrame](new RuntimeException(err))
//         case Right(res) => Future.successful(res)
//       }
//     }
//   }
// }
