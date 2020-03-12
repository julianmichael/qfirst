package qfirst

import shapeless.{::, HNil}

package object ontonotes {
  import jjm.ling._
  type CoNLLToken = Index :: Pos :: Token :: HNil
  object CoNLLToken {
    def apply(index: Int, pos: String, token: String): CoNLLToken = {
      Index.field(index) :: Pos.field(pos) :: Token.field(token) :: HNil
    }
  }

  // import jjm._
  // def memoizeDotKleisliIO[F[_], A <: Dot](
  //   dotKleisli: DotKleisli[IO, A],
  //   shouldCache: A => Boolean
  // ): DotKleisli[F, A] = {
  //   var cache = DotMap.empty[F, A]
  //   new DotKleisli[F, A] {
  //     def apply(a: A): F[a.Out] = {
  //       if(shouldCache(a)) {
  //         cache.get(a).map(IO.pure).getOrElse {
  //           dotKleisli(a).flatTap(res =>
  //             IO(cache = cache.put(a)(res))
  //           )
  //         }
  //       } else dotKleisli(a)
  //     }
  //   }
  // }
}
