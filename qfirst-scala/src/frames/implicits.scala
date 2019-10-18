package qfirst.frames

import scala.collection.immutable.SortedSet

import cats.Foldable
import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import jjm.DependentMap

import io.circe.{Encoder, Decoder}

object implicits {

  // implicit def nonEmptySetDecoder[A: Decoder : Order]: Decoder[NonEmptySet[A]] = {
  //   import io.circe.generic.auto._
  //   implicitly[Decoder[List[A]]].map(l => NonEmptySet.fromSetUnsafe[A](SortedSet(l: _*)))
  // }
  // implicit def nonEmptySetEncoder[A: Encoder]: Encoder[NonEmptySet[A]] = {
  //   import io.circe.generic.auto._
  //   implicitly[Encoder[List[A]]].contramap[NonEmptySet[A]](_.toList)
  // }

  // implicit class RichFoldable[F[_]: Foldable, A](val fa: F[A]) {
  //   def maxima(implicit o: Order[A]): List[A] =
  //     fa.foldLeft(List.empty[A]) { (maxes, a) =>
  //       maxes.headOption.fold(a :: Nil) { max =>
  //         if(a > max) a :: Nil
  //         else if(a.eqv(max)) a :: maxes
  //         else maxes
  //       }
  //     }

  //   def maximaBy[B: Order](f: A => B): List[A] =
  //     fa.foldLeft(List.empty[(A, B)]) { (maxes, a) =>
  //       val b = f(a)
  //       maxes.headOption.fold((a, b) :: Nil) { case (_, maxKey) =>
  //         if(b > maxKey) (a, b) :: Nil
  //         else if(b.eqv(maxKey)) (a, b) :: maxes
  //         else maxes
  //       }
  //     }.map(_._1)
  // }
}
