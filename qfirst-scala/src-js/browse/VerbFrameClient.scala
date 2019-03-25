package qfirst.browse

import scala.concurrent.Future

import nlpdata.datasets.wiktionary.InflectedForms

import io.circe.parser.decode
import io.circe.syntax._

case class VerbFrameClient(apiUrl: String) extends VerbFrameService[Future] {

  import qasrl.data.JsonCodecs.{inflectedFormsEncoder, inflectedFormsDecoder}
  import scala.concurrent.ExecutionContext.Implicits.global
  val printer = io.circe.Printer.noSpaces

  def getVerbs: Future[Map[InflectedForms, Int]] = {
    import io.circe.generic.auto._
    org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/getVerbs", data = printer.pretty(().asJson)).map(_.responseText).flatMap { jsonStr =>
      decode[List[(InflectedForms, Int)]](jsonStr) match {
        case Left(err)  => Future.failed[Map[InflectedForms, Int]](new RuntimeException(err))
        case Right(res) => Future.successful(res.toMap)
      }
    }
  }

  def getFrame(inflectedForms: InflectedForms): Future[VerbFrameset] = {
    org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/getFrame", data = printer.pretty(inflectedForms.asJson)).map(_.responseText).flatMap { jsonStr =>
      decode[VerbFrameset](jsonStr) match {
        case Left(err)  => Future.failed[VerbFrameset](new RuntimeException(err))
        case Right(res) => Future.successful(res)
      }
    }
  }

}
