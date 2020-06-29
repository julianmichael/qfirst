package qfirst.datasets

import shapeless.{HList, ::, HNil}
import shapeless.labelled.FieldType
import shapeless.ops.record.Selector
import shapeless.record._
import shapeless.syntax.singleton._

import simulacrum._
import scala.language.implicitConversions

/** Utilities for working with the CoNLL 2008 Shared Task data, found here:
  * https://catalog.ldc.upenn.edu/LDC2009T12
  */
package object conll08 {

  import jjm.ling._

  type Lemma = FieldType[Lemma.type, String]
  object Lemma {
    def apply(x: String) = Lemma ->> x :: HNil
    def field(x: String) = Lemma ->> x
  }

  @typeclass trait HasLemma[T] {
    def lemma(t: T): String
  }
  object HasLemma {
    implicit def recordHasLemma[T <: HList : Selector.Aux[?, Lemma.type, String]] = new HasLemma[T] {
      def lemma(t: T): String = t(Lemma)
    }
    implicit val lemmaHasLemma = new HasLemma[Lemma] {
      def lemma(t: Lemma): String = t
    }
  }

  type CoNLL08Token = Index :: Pos :: Token :: Lemma :: HNil
  object CoNLL08Token {
    def apply(index: Int, pos: String, token: String, lemma: String): CoNLL08Token = {
      Index.field(index) :: Pos.field(pos) :: Token.field(token) :: Lemma.field(lemma) :: HNil
    }
  }
}
