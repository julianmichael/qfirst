package qfirst.frames

import scala.collection.immutable.SortedSet

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

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

  import io.circe.{Encoder, Decoder}
  import io.circe.{KeyEncoder, KeyDecoder}
  import io.circe.HCursor
  import io.circe.Json

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringEncoder: Encoder[LowerCaseString] =
    Encoder.instance[LowerCaseString](l => Json.fromString(l.toString))
  implicit val lowerCaseStringDecoder: Decoder[LowerCaseString] =
    Decoder.instance[LowerCaseString](c => c.as[String].map(_.lowerCase))

  import qasrl.Tense

  implicit val tenseEncoder: Encoder[Tense] = {
    io.circe.generic.semiauto.deriveEncoder[Tense]
  }
  implicit val tenseDecoder: Decoder[Tense] = {
    io.circe.generic.semiauto.deriveDecoder[Tense]
  }

  implicit def nonEmptySetDecoder[A: Decoder : Order]: Decoder[NonEmptySet[A]] = {
    import io.circe.generic.auto._
    implicitly[Decoder[List[A]]].map(l => NonEmptySet.fromSetUnsafe[A](SortedSet(l: _*)))
  }
  implicit def nonEmptySetEncoder[A: Encoder]: Encoder[NonEmptySet[A]] = {
    import io.circe.generic.auto._
    implicitly[Encoder[List[A]]].contramap[NonEmptySet[A]](_.toList)
  }

  trait DependentEncoder[F[_], G[_]] {
    def getEncoder[A](fa: F[A]): Encoder[G[A]]
  }

  trait DependentDecoder[F[_], G[_]] {
    def getDecoder[A](fa: F[A]): Decoder[G[A]]
  }

  implicit def dependentMapEncoder[F[_], G[_]](
    implicit keyEncoder: KeyEncoder[F[_]],
    dependentEncoder: DependentEncoder[F, G]
  ): Encoder[DependentMap[F, G]] = new Encoder[DependentMap[F, G]] {
    final def apply(m: DependentMap[F, G]) = Json.obj(
      m.iterator.map(pair =>
        keyEncoder(pair.fst) -> dependentEncoder.getEncoder(pair.fst)(pair.snd)
      ).toSeq: _*
    )
  }

  // TODO REALLY NEED TO FIX THIS UP!!!

  private case class Foo[F[_], A](fa: F[A]) {
    type Arg = A
  }

  implicit def dependentMapDecoder[F[_], G[_]](
    implicit keyDecoder: KeyDecoder[F[_]],
    dependentDecoder: DependentDecoder[F, G]
  ): Decoder[DependentMap[F, G]] = new Decoder[DependentMap[F, G]] {
    final def apply(c: HCursor): Decoder.Result[DependentMap[F, G]] = {
      // TODO aah replace the get
      c.keys.get.toList.foldM[Decoder.Result, DependentMap[F, G]](DependentMap.empty[F, G]) { (m, keyStr) =>
        import scala.language.existentials
        val key = keyDecoder(keyStr).get // TODO aah replace the get
        val value = dependentDecoder.getDecoder(key).tryDecode(c.downField(keyStr))
        val foo = Foo(key)
        type Arg = foo.Arg
        value.map(v => m.put[Arg](key.asInstanceOf[F[Arg]], v.asInstanceOf[G[Arg]]))
      }
    }
  }
}
