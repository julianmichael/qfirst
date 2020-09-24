package qfirst.frame

package object browse {

  import jjm.DotKleisli

  type FeatureService[F[_], VerbType, Arg] = DotKleisli[F, FeatureReq[VerbType, Arg]]
  object FeatureService extends FeatureServiceCompanionPlatformExtensions

  type ParaphraseAnnotations = Map[
    // sentence
    String, Map[
      // verb index
      Int, VerbParaphraseLabels
    ]
  ]
}
