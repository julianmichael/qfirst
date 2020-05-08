package qfirst.frame

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import freelog.TreeLogger

// TODO remove logger dependency
// circumvent side-effect of ref creation
class Cell[A](name: String, create: IO[A])(implicit Log: TreeLogger[IO, String]) {
  private[this] val value: Ref[IO, Option[A]] =
    Ref[IO].of[Option[A]](None).unsafeRunSync
  val get: IO[A] = value.get.flatMap(innerValue =>
    innerValue.map(a =>
      Log.debug(s"Retrieving in-memory cached value: $name").as(a)
    ).getOrElse(
      Log.debugBranch(s"Computing and caching value: $name")(
        create.flatTap(a => value.set(Some(a)))
      )
    )
  )
  val isPresent = value.get.map(_.nonEmpty)
}
object Cell {
  def apply[A](
    name: String)(
    create: IO[A])(
    implicit Log: TreeLogger[IO, String]
  ) = new Cell(name, create)
}
