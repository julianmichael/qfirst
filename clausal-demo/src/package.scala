package qfirst.frames
import qfirst._
import qfirst.protocols._

import qasrl.crowd.QASRLEvaluationSettings


package object crowd {

  type ClausalSentencePrediction = SentencePrediction[
    FactoringProtocol.Beam[QfirstBeamItem[Map[String, String]]]
  ]

  def dollarsToCents(d: Double): Int = math.round(100 * d).toInt

  implicit val settings = new QASRLEvaluationSettings {}
}
