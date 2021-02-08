package qfirst.frame.util

import java.nio.file.Path
import java.nio.file.Files

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import freelog.TreeLogger
import qfirst.frame.logLevel

class FileCached[A](
  name: String)(
  path: Path,
  _read: Path => IO[A],
  write: (Path, A) => IO[Unit])(
  val compute: IO[A]
) {
  def read(implicit Log: TreeLogger[IO, String]): IO[Option[A]] =
    IO(Files.exists(path)).ifM(
      Log.infoBranch(s"$name: reading cached data")(
        Log.info(s"Path: $path") >> _read(path).map(Some(_))
      ),
      Log.warn(s"$name: no cached data found at $path.").as(None)
    )

  def get(implicit Log: TreeLogger[IO, String]) =
    IO(Files.exists(path)).ifM(
      Log.infoBranch(s"$name: reading cached data")(
        Log.info(s"Path: $path") >> _read(path)
      ),
      Log.infoBranch(s"$name: no cached data found. Computing now.")(
        Log.info(s"Path: $path") >> compute.flatTap(x =>
          Log.infoBranch(s"$name: writing to cache")(
            write(path, x)
          )
        )
      )
    )
}
object FileCached {
  def apply[A](
    name: String)(
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A])(
    implicit Log: TreeLogger[IO, String]
  ): FileCached[A] = new FileCached(name)(path, read, write)(compute)

  def get[A](
    name: String)(
    path: Path,
    read: Path => IO[A],
    write: (Path, A) => IO[Unit])(
    compute: IO[A])(
    implicit Log: TreeLogger[IO, String]
  ): IO[A] = new FileCached(name)(path, read, write)(compute).get
}
