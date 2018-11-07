package qfirst.frames

import qasrl.bank.SentenceId

import qasrl.crowd.QASRLEvaluationSettings

package object crowd {
  import upickle.default._
  implicit val sentenceIdReader: Reader[SentenceId] = Reader {
    case x => SentenceId.fromString(x.str.toString)
  }
  implicit val sentenceIdWriter: Writer[SentenceId] = Writer {
    case x => upickle.Js.Str(SentenceId.toString(x))
  }

  implicit val settings = new QASRLEvaluationSettings {}
}
