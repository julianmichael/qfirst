package qfirst.frame.browse

import qfirst.frame.features.Features

import jjm.DotKleisli

import cats.effect.IO

trait FeatureServiceCompanionPlatformExtensions {
  def baseService[VerbType, Arg](
    features: Features[VerbType, Arg]
  ) = new DotKleisli[IO, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): IO[req.Out] = req match {
      case FeatureReq.Sentence(sid) => features.sentenceInfos.get
          .map(_.apply(sid))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.Sentences(vt) => features.sentencesByVerbType.get
          .map(_.apply(vt))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.QuestionDists(vt) => features.argQuestionDists.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgSpans(vt) => features.argSpans.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case _ => ??? // TODO rest of cases
    }
  }
}
