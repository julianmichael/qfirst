package qfirst.frames

import qasrl.util.DependentMap

import monocle.Lens
import monocle.function.At
import monocle.function.Index

object implicits {
  implicit def dependentMapAt[F[_], G[_], I]: At[DependentMap[F, G], F[I], Option[G[I]]] =
    At[DependentMap[F, G], F[I], Option[G[I]]](
      i => map => map.get(i))(
      i => optV => map => optV.fold(map.remove(i))(v => map.put(i, v))
    )
  implicit def dependentMapIndex[F[_], G[_], I]: Index[DependentMap[F, G], F[I], G[I]] = Index.fromAt
}
