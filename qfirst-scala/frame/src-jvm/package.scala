package qfirst.frame

import cats.Id
import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import qasrl.ArgumentSlot
import qasrl.data.Dataset

import freelog.TreeLogger

import qfirst.model.eval.questionLabelIsValidNonDense
import qfirst.clause.ArgStructure
import qfirst.metrics.HasMetrics.ops._

trait PackagePlatformExtensions {
  def filterDatasetNonDense(dataset: Dataset) = {
    dataset.filterQuestionLabels(questionLabelIsValidNonDense)
      .cullQuestionlessVerbs
      .cullQuestionlessSentences
      .cullVerblessSentences
  }

  def readDataset(path: Path): IO[Dataset] = IO.fromTry(
    qasrl.bank.Data.readDataset(path)
  )

  import java.nio.file._

  // TODO deprecate this
  def fileCached[A](
    name: String)(
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A])(
    implicit Log: TreeLogger[IO, String]
  ): IO[A] = FileCached(name)(path, read, write)(compute).get

  // def optionFromFile[A](
  //   path: Path,
  //   read: Path => IO[A]
  // ): IO[Option[A]] = fileCached[Option[A]](
  //   path,
  //   read = path => read(path).map(Some(_)),
  //   write = (_, _) => IO.unit
  // )(compute = None)

  // val ioToId: (IO ~> Id) = Lambda(_.unsafeRunSync())
  // val idToIO: (Id ~> IO) = Lambda(IO(_))

  // implicit def idLoggerFromIOLogger(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): EphemeralTreeLogger[Id, String] = new EphemeralTreeLogger[Id, String] {
  //   def emit(msg: Msg, level: LogLevel): Unit = Log.emit(msg, level).unsafeRun

  //   def emitBranch[A](
  //     msg: Msg, logLevel: LogLevel)(
  //     body: F[A])(
  //     implicit ambientLevel: LogLevel
  //   ): F[A]
    
  //   /** Create a rewind block, wherein calls to `rewind` will rewind to the current state */
  //   def block[A](fa: F[A]): F[A]
  //   /** Rewind to the state at the last containing `block`; Effectful changes to the log may be done lazily */
  //   def rewind: F[Unit]
  //   /** Flush the buffer to effect the last call to `rewind` */
  //   def flush: F[Unit]
  // }

  import qfirst.metrics._

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc,
      "mean" :: inc
    )
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)
}

