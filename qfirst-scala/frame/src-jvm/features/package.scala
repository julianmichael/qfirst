package qfirst.frame

import qfirst.frame.util.NonMergingMap

package object features {

  type VerbFeatures[VerbType, A] = Map[
    VerbType, NonMergingMap[VerbId, A]
  ]
  type ArgFeatures[VerbType, A, B] = Map[
    VerbType, NonMergingMap[ArgumentId[A], B]
  ]

}
