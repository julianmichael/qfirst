package qfirst.frame

import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached

import cats.Monoid
import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import freelog.TreeLogger

case class RunData[A](
  train: IO[A],
  dev: IO[A],
  test: IO[A]) {

  def run: IO[Unit] = train >> dev >> test >> IO.unit

  def apply(split: RunData.BaseSplit): IO[A] = split match {
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
    name: String, getCachePath: String => IO[Path])(
    read: Path => IO[A], write: (Path, A) => IO[Unit])(
    implicit mode: RunMode,
    monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = {
    def doCache(runName: String, a: IO[A]) = {
      new Cell(
        s"$name ($runName)",
        getCachePath(runName) >>= (path =>
          FileCached.get[A](s"$name ($runName)")(
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
  def strings = RunData(
    IO.pure("train"), IO.pure("dev"), IO.pure("test")
  )
  def apply[A](train: A, dev: A, test: A): RunData[A] = RunData(
    IO.pure(train), IO.pure(dev), IO.pure(test)
  )
  def splits = RunData[BaseSplit](
    IO.pure(Train), IO.pure(Dev), IO.pure(Test)
  )

  sealed trait Split
  case object Input extends Split
  case object Eval extends Split
  case object Full extends Split
  case object All extends Split
  sealed trait BaseSplit extends Split
  case object Train extends BaseSplit
  case object Dev extends BaseSplit
  case object Test extends BaseSplit
}

class RunDataCell[A](
  name: String,
  train: Cell[A],
  dev: Cell[A],
  test: Cell[A])(
  implicit mode: RunMode, monoid: Monoid[A], Log: TreeLogger[IO, String]
) {

  def apply(split: RunData.Split): IO[A] = split match {
    case RunData.Train => train.get
    case RunData.Dev => dev.get
    case RunData.Test => test.get
    case RunData.Input => input.get
    case RunData.Eval => eval.get
    case RunData.Full => full.get
    case RunData.All => all.get
  }

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
