package qfirst

import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import qasrl.data.Dataset

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
}

