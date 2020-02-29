package qfirst.frame

import java.nio.file.Path
import java.nio.file.Files

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import freelog.TreeLogger

class FileCached[A](
  name: String)(
  path: Path,
  _read: Path => IO[A],
  write: (Path, A) => IO[Unit])(
  compute: IO[A]
) {
  def read(implicit Log: TreeLogger[IO, String]) =
    IO(Files.exists(path)).ifM(
      Log.infoBranch(s"$name: reading cached data from $path")(_read(path)),
      Log.warn(s"$name: no cached data found at $path.").as(None)
    )

  def get(implicit Log: TreeLogger[IO, String]) =
    IO(Files.exists(path)).ifM(
      Log.infoBranch(s"$name: reading cached data from $path")(_read(path)),
      Log.infoBranch(s"$name: no cached data found at $path. Computing now.")(
        compute.flatTap(x =>
          Log.infoBranch(s"$name: writing to cache at $path")(
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
}
