package qfirst.frames

import qasrl.data.VerbEntry

import simulacrum._
import scala.language.implicitConversions

@typeclass trait FramePredictionModel[A] {
  def predictFramesWithAnswers(model: A, verb: VerbEntry): Map[String, (Frame, ArgumentSlot)]
}
object FramePredictionModel {
  def make[A](predictFramesWithAnswersFn: (A, VerbEntry) => Map[String, (Frame, ArgumentSlot)]): FramePredictionModel[A] =
    new FramePredictionModel[A] {
      def predictFramesWithAnswers(model: A, verb: VerbEntry) = predictFramesWithAnswersFn(model, verb)
    }
}
