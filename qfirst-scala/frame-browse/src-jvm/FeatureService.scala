package qfirst.frame.browse

import qfirst.frame.features.Features

import jjm.DotKleisli

import cats.effect.IO

trait FeatureServiceCompanionPlatformExtensions {
  def baseService[VerbType, Arg](
    features: Features[VerbType, Arg]
  ) = new DotKleisli[IO, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): IO[req.Out] = req.key match {
      case FeatureKey.QuestionDist() => features.argQuestionDists.get
          .map(_.apply(req.verbType).value)
          .asInstanceOf[IO[req.Out]]
    }
  }
}
