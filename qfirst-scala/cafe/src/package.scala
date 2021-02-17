package qfirst

package object cafe {
  sealed trait Person
  object Person {
    case object First extends Person
    case object Second extends Person
    case object Third extends Person
  }

  sealed trait Number
  object Number {
    case object Singular extends Number
    case object Plural extends Number
  }
}
