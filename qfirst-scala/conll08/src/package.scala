package qfirst

import shapeless.{::, HNil}

package object conll08 {
  import jjm.ling._
  type CoNLL08Token = Index :: Pos :: Token :: HNil
  object CoNLL08Token {
    def apply(index: Int, pos: String, token: String): CoNLL08Token = {
      Index.field(index) :: Pos.field(pos) :: Token.field(token) :: HNil
    }
  }
}
