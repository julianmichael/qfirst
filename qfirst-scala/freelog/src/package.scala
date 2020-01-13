import cats.Monad
import cats.Traverse
import cats.data.StateT
import cats.implicits._

package object freelog {

  implicit class RichTraverse[F[_]: Traverse, A](val fa: F[A]) {
    def traverseWithIndexAndSizeM[G[_], B](f: (A, Int) => G[B])(implicit G: Monad[G]): G[(F[B], Int)] =
      fa.traverse(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b))))
        .run(0).map(_.swap)

    // bar if arg present
    final def logTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg],
      ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, logLevel, sizeHint)(f)

    final def debugTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Debug, sizeHint)(f)

    final def traceTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Trace, sizeHint)(f)

    final def infoTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Info, sizeHint)(f)

    final def warnTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Warn, sizeHint)(f)

    final def errorTraverse[G[_]: Monad, Msg: ProgressSpec, B](
      prefix: Msg, sizeHint: Option[Long])(
      f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Error, sizeHint)(f)

    // never bar
    final def logTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, logLevel, None)(f)
    final def debugTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Debug, None)(f)
    final def traceTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Trace, None)(f)
    final def infoTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Info, None)(f)
    final def warnTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Warn, None)(f)
    final def errorTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Error, None)(f)

    // always bar
    final def logBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, logLevel, Some(size))(f)
    final def debugBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Debug, Some(size))(f)
    final def traceBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Trace, Some(size))(f)
    final def infoBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Info, Some(size))(f)
    final def warnBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Warn, Some(size))(f)
    final def errorBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Error, Some(size))(f)

    // always bar
    final def logBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, logLevel, Some(fa.size))(f)
    final def debugBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Debug, Some(fa.size))(f)
    final def traceBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Trace, Some(fa.size))(f)
    final def infoBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Info, Some(fa.size))(f)
    final def warnBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Warn, Some(fa.size))(f)
    final def errorBarTraverse[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverse(fa, prefix, LogLevel.Error, Some(fa.size))(f)

    // sometimes bar
    def logTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, logLevel, sizeHint)(f)
    def debugTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Debug, sizeHint)(f)
    def traceTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Trace, sizeHint)(f)
    def infoTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Info, sizeHint)(f)
    def warnTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Warn, sizeHint)(f)
    def errorTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, sizeHint: Option[Long])(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Error, sizeHint)(f)

    // never bar
    final def logTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, logLevel, None)(f)
    final def debugTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Debug, None)(f)
    final def traceTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Trace, None)(f)
    final def infoTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Info, None)(f)
    final def warnTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Warn, None)(f)
    final def errorTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger:EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Error, None)(f)

    // always bar
    final def logBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, logLevel, Some(size))(f)
    final def debugBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Debug, Some(size))(f)
    final def traceBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Trace, Some(size))(f)
    final def infoBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Info, Some(size))(f)
    final def warnBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Warn, Some(size))(f)
    final def errorBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, size: Long)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Error, Some(size))(f)

    // always bar
    final def logBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg, logLevel: LogLevel)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, logLevel, Some(fa.size))(f)
    final def debugBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Debug, Some(fa.size))(f)
    final def traceBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Trace, Some(fa.size))(f)
    final def infoBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Info, Some(fa.size))(f)
    final def warnBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Warn, Some(fa.size))(f)
    final def errorBarTraverseWithIndexM[G[_]: Monad, Msg: ProgressSpec, B](prefix: Msg)(f: (A, Int) => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logger.logTraverseWithIndexM(fa, prefix, LogLevel.Error, Some(fa.size))(f)
  }
}

