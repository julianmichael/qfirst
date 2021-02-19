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

  sealed trait Case
  object Case {
    case object Nominative extends Case
    case object Accusative extends Case
    case object Genitive extends Case
  }
}
