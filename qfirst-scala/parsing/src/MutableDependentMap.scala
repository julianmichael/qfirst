package qfirst.parsing

import scala.collection.mutable

final class MutableDependentMap[F[_], G[_]] private () {
  private[this] val map: mutable.Map[F[_], G[_]] = mutable.Map.empty[F[_], G[_]]
  def get[A](key: F[A]): Option[G[A]] = map.get(key).asInstanceOf[Option[G[A]]]
  def put[A](key: F[A], value: G[A]): Unit = map.put(key, value)
}
object MutableDependentMap {
  def empty[F[_], G[_]] = new MutableDependentMap[F, G]
}
