package qfirst.frame

import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import qasrl.ArgumentSlot
import qasrl.data.Dataset

import qfirst.model.eval.questionLabelIsValidNonDense
import qfirst.clause.ArgStructure
import qfirst.metrics.HasMetrics.ops._

trait PackagePlatformExtensions {
  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))

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

  def fileCached[A](
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A]
  ): IO[A] = {
    IO(Files.exists(path)).ifM(
      logOp(s"Reading data from $path", read(path)),
      compute.flatTap(write(path, _))
    )
  }

  // def optionFromFile[A](
  //   path: Path,
  //   read: Path => IO[A]
  // ): IO[Option[A]] = fileCached[Option[A]](
  //   path,
  //   read = path => read(path).map(Some(_)),
  //   write = (_, _) => IO.unit
  // )(compute = None)

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

