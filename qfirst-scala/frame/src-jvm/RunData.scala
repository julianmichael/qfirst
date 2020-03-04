package qfirst.frame

import cats.Monoid
import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import freelog.TreeLogger

case class RunData[A](
  train: IO[A],
  dev: IO[A],
  test: IO[A]) {

  def map[B](f: A => B) = RunData(
    train map f,
    dev map f,
    test map f
  )

  def flatMap[B](f: A => IO[B]) = RunData(
    train >>= f,
    dev >>= f,
    test >>= f
  )

  def zip[B](that: RunData[B]) = RunData(
    this.train product that.train,
    this.dev product that.dev,
    this.test product that.test
  )

  def toCell(name: String)(
    implicit mode: RunMode,
    monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = new RunDataCell(
    name,
    new Cell(s"$name (train)", train),
    new Cell(s"$name (dev)", dev),
    new Cell(s"$name (test)", test),
  )

  def toFileCachedCell(
    name: String, getCachePath: String => Path)(
    read: Path => IO[A], write: (Path, A) => IO[Unit])(
    implicit mode: RunMode,
    monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = {
    def doCache(runName: String, a: IO[A]) = {
      new Cell(
        s"$name ($runName)",
        fileCached[A](s"$name ($runName)")(
          path = getCachePath(runName),
          read = read, write = write)(
          a
        )
      )
    }
    new RunDataCell(
      name,
      doCache("train", train),
      doCache("dev", dev),
      doCache("test", test)
    )
  }
}
object RunData {
  def strings = RunData(
    IO.pure("train"), IO.pure("dev"), IO.pure("test")
  )
  def apply[A](train: A, dev: A, test: A): RunData[A] = RunData(
    IO.pure(train), IO.pure(dev), IO.pure(test)
  )
}

class RunDataCell[A](
  name: String,
  train: Cell[A],
  dev: Cell[A],
  test: Cell[A])(
  implicit mode: RunMode, monoid: Monoid[A], Log: TreeLogger[IO, String]
) {
  def input = if(mode.isSanity) dev else train
  def eval = if(mode.isTest) test else dev
  val full = new Cell(
    s"$name (full)",
    for(i <- input.get; e <- eval.get) yield i |+| e
  )
  val all = new Cell(
    s"$name (all)",
    for(tr <- train.get; de <- dev.get; te <- test.get) yield tr |+| de |+| te
  )

  def get = RunData(train.get, dev.get, test.get)
}
