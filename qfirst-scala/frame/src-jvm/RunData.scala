package qfirst.frame

import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached

import cats.Monad
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

  // def input(implicit mode: RunMode) = if(mode.isSanity) dev else train
  // def eval(implicit mode: RunMode) = if(mode.isTest) test else dev
  // def full(implicit mode: RunMode, m: Monoid[A]) = {
  //   if(mode.isSanity) dev
  //   else input |+| eval
  // }
  // def all(implicit mode: RunMode, m: Monoid[A]) = {
  //   train |+| dev |+| test
  // }

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

  val input = new Cell(
    s"$name (input)",
    RunDataCell.constructSplit(mode, RunData.Input, train.get, dev.get, test.get)
  )
  val eval = new Cell(
    s"$name (eval)",
    RunDataCell.constructSplit(mode, RunData.Eval, train.get, dev.get, test.get)
  )
  val full = new Cell(
    s"$name (full)",
    RunDataCell.constructSplit(mode, RunData.Full, train.get, dev.get, test.get)
  )
  val all = new Cell(
    s"$name (all)",
    RunDataCell.constructSplit(mode, RunData.All, train.get, dev.get, test.get)
  )

  def get = RunData(train.get, dev.get, test.get)
}
object RunDataCell {
  def constructSplit[F[_]: Monad, A: Monoid](
    mode: RunMode, split: RunData.Split,
    train: F[A], dev: F[A], test: F[A]
  ): F[A] = {
    def input = if(mode.isSanity) dev else train
    def eval = if(mode.isTest) test else dev
    split match {
      case RunData.Train => train
      case RunData.Dev => dev
      case RunData.Test => test
      case RunData.Input => input
      case RunData.Eval => eval
      case RunData.Full => for(i <- input; e <- eval) yield i |+| e
      case RunData.All => for(tr <- train; de <- dev; te <- test) yield tr |+| de |+| te
    }
  }

  import cats.Id

  private[this] val getIndex = (s: String) => s match {
    case "train" => 0
    case "dev" => 1
    case "test" => 2
  }

  class Labels(mode: RunMode) {

    def apply(split: RunData.Split) = constructSplit[Id, Set[String]](
      mode, split, Set("train"), Set("dev"), Set("test")
    ).toList.sortBy(getIndex).mkString("+")

    def train = apply(RunData.Train)
    def dev = apply(RunData.Dev)
    def test = apply(RunData.Test)
    def input = apply(RunData.Input)
    def eval = apply(RunData.Eval)
    def full = apply(RunData.Full)
    def all  = apply(RunData.All)
  }
}
