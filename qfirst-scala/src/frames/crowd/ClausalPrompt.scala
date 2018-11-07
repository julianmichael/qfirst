package qfirst.frames.crowd

import qfirst.frames._

import qasrl.crowd.QASRLValidationWorkerInfoSummary

import spacro.tasks.ResponseRW

case class ClausalPrompt[SID](sentenceId: SID)
object ClausalPrompt {
  import upickle.default._
  implicit def reader[SID: Reader] = macroR[ClausalPrompt[SID]]
  implicit def writer[SID: Writer] = macroW[ClausalPrompt[SID]]
}

case class ClausalAjaxResponse(
  workerInfoOpt: Option[QASRLValidationWorkerInfoSummary],
  predictions: ClausalSentencePrediction
)
object ClausalAjaxResponse {
  import upickle.default._

  implicit val predictionsReader: Reader[ClausalSentencePrediction] = Reader {
    case x => io.circe.parser.decode[ClausalSentencePrediction](x.str.toString).right.get
  }
  implicit val predictionsWriter: Writer[ClausalSentencePrediction] = Writer(
    (x: ClausalSentencePrediction) => {
      import io.circe.syntax._
      upickle.Js.Str(io.circe.Printer.noSpaces.pretty(x.asJson))
    }
  )

  implicit val reader: Reader[ClausalAjaxResponse] = macroR[ClausalAjaxResponse]
  implicit val writer: Writer[ClausalAjaxResponse] = macroW[ClausalAjaxResponse]
}

case class ClausalAjaxRequest[SID](workerIdOpt: Option[String], id: SID) {
  type Response = ClausalAjaxResponse
}
object ClausalAjaxRequest {
  import upickle.default._
  implicit def responseRW[SID] = new ResponseRW[ClausalAjaxRequest[SID]] {
    override def getReader(request: ClausalAjaxRequest[SID]) =
      ClausalAjaxResponse.reader
    override def getWriter(request: ClausalAjaxRequest[SID]) =
      ClausalAjaxResponse.writer
  }
  implicit def reader[SID: Reader] = macroR[ClausalAjaxRequest[SID]]
  implicit def writer[SID: Writer] = macroW[ClausalAjaxRequest[SID]]
}
