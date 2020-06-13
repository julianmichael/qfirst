package qfirst.frame

import qfirst.frame.util.NonMergingMap

package object features {

  type VerbFeatures[VerbType, A] = Map[
    VerbType, NonMergingMap[VerbId, A]
  ]
  type ArgFeatures[VerbType, A, B] = Map[
    VerbType, NonMergingMap[ArgumentId[A], B]
  ]

  // TODO delete these
  // type Instances[VerbType, A] = Map[
  //   VerbType, Map[String, NonMergingMap[Int, A]]
  // ]
  // object Instances {
  //   type Qasrl = Instances[InflectedForms, QAPairs]
  //   type PropBank = Instances[String, QAPairs]
  // }
  // type ParaphraseAnnotations = Map[
  //   // sentence
  //   String, Map[
  //     // verb index
  //     Int, VerbParaphraseLabels
  //   ]
  // ]
}
