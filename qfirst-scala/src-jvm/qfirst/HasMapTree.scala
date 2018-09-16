package qfirst

import simulacrum._
import scala.language.implicitConversions

@typeclass trait HasMapTree[V, A] {
  def getMapTree(a: A): MapTree[String, V]
}
object {
  implicit def anyHasSelfTree[A]: HasMapTree[A, A] =
    new HasMapTree[A, A] {
      def getMapTree(a: A): MapTree[String, A] = MapTree.Leaf[String](a)
    }
}
