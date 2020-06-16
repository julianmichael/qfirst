package qfirst.frame.features

import qfirst.frame.ArgumentId
import qfirst.frame.RunMode
import qfirst.frame.VerbId

import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached

import cats.Applicative
import cats.Monad
import cats.Monoid
import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import freelog.TreeLogger

case class RunData[+A](
  train: IO[A],
  dev: IO[A],
  test: IO[A])(
  implicit mode: RunMode
) {

  def get = mode.get(this)

  def all = List(train, dev, test).sequence

  def apply(split: RunData.Split): IO[A] = split match {
    case RunData.Train => train
    case RunData.Dev => dev
    case RunData.Test => test
  }

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
  // def >>=[B](f: A => IO[B]) = flatMap(f)

  def zip[B](that: RunData[B]) = RunData(
    this.train product that.train,
    this.dev product that.dev,
    this.test product that.test
  )

  def toCell(name: String)(
    implicit mode: RunMode,
    // monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = new RunDataCell(
    name,
    new Cell(s"$name (train)", train),
    new Cell(s"$name (dev)", dev),
    new Cell(s"$name (test)", test),
  )

  def toFileCachedCell[B >: A](
    name: String, getCachePath: String => IO[Path])(
    read: Path => IO[B], write: (Path, B) => IO[Unit])(
    implicit mode: RunMode,
    // monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = {
    def doCache(runName: String, a: IO[B]) = {
      new Cell(
        s"$name ($runName)",
        getCachePath(runName) >>= (path =>
          FileCached.get[B](s"$name ($runName)")(
            path = path,
            read = read,
            write = write)(
            a
          )
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
  def strings(implicit mode: RunMode) = RunData(
    IO.pure("train"), IO.pure("dev"), IO.pure("test")
  )
  def apply[A](train: A, dev: A, test: A)(implicit mode: RunMode): RunData[A] = RunData(
    IO.pure(train), IO.pure(dev), IO.pure(test)
  )
  def splits(implicit mode: RunMode) = RunData[Split](
    IO.pure(Train), IO.pure(Dev), IO.pure(Test)
  )

  sealed trait Split
  case object Train extends Split
  case object Dev extends Split
  case object Test extends Split

  implicit def runDataApplicative(
    implicit mode: RunMode
  ): Applicative[RunData] = new Applicative[RunData] {

    def pure[A](a: A): RunData[A] = {
      val fa = IO.pure(a)
      RunData(fa, fa, fa)
    }

    def ap[A, B](ff: RunData[A => B])(fa: RunData[A]): RunData[B] = RunData(
      train = ff.train <*> fa.train,
      dev = ff.dev <*> fa.dev,
      test = ff.test <*> fa.test
    )

    override def map[A, B](fa: RunData[A])(f: A => B): RunData[B] = fa.map(f)

    override def product[A, B](fa: RunData[A], fb: RunData[B]): RunData[(A, B)] = fa.zip(fb)
  }
}

class RunDataCell[+A](
  name: String,
  train: Cell[A],
  dev: Cell[A],
  test: Cell[A])(
  implicit mode: RunMode, Log: TreeLogger[IO, String]
) {

  def apply(split: RunData.Split): IO[A] = split match {
    case RunData.Train => train.get
    case RunData.Dev => dev.get
    case RunData.Test => test.get
  }

  def data = RunData(train.get, dev.get, test.get)

  def all: IO[List[A]] = data.all

  def get = data.get
}
object RunDataCell
