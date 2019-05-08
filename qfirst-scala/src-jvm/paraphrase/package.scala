package qfirst

import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import qasrl.data.Dataset

import qfirst.ClauseResolution.ArgStructure
import qasrl.ArgumentSlot

package object paraphrase {
  def logOp[A](msg: String, op: IO[A]): IO[A] =
    IO(print(s"$msg...")) >> op >>= (a => IO(println(" Done.")).as(a))

  def logOp[A](msg: String, op: => A): IO[A] = logOp(msg, IO(op))

  def filterDatasetNonDense(dataset: Dataset) = {
    dataset.filterQuestionLabels(questionLabelIsValidNonDense)
      .cullQuestionlessVerbs
      .cullQuestionlessSentences
      .cullVerblessSentences
  }

  def readDataset(path: Path): IO[Dataset] = IO(
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

  def getArgumentSlotsForClauseTemplate(clauseTemplate: ArgStructure): Set[ArgumentSlot] = {
    (clauseTemplate.args.keys.toList: List[ArgumentSlot]).filter {
      case qasrl.Obj2 => clauseTemplate.args.get(qasrl.Obj2) match {
        case Some(qasrl.Prep(_, None)) => false
        case _ => true
      }
      case _ => true
    }.toSet
  }
}

