package freelog
package syntax

import _root_.cats.Applicative
import _root_.cats.Foldable
import _root_.cats.Monad
import _root_.cats.Monoid
import _root_.cats.Traverse
import _root_.cats.data.StateT
import _root_.cats.implicits._

object cats extends CatsSyntax

trait CatsSyntax {
  implicit class FreelogCatsFoldableOps[F[_]: Foldable, A](val fa: F[A]) {
    def foldMapWithIndexAndSizeM[G[_], B](
      f: (A, Int) => G[B])(
      implicit G: Monad[G], M: Monoid[B]
    ): G[(B, Int)] =
      fa.foldM(M.empty -> 0) {
        case ((b, i), a) =>  f(a, i).map(b2 => (b |+| b2) -> (i + 1))
      }

    def foldMapWithIndexM[G[_], B](
      f: (A, Int) => G[B])(
      implicit G: Monad[G], M: Monoid[B]
    ): G[B] = foldMapWithIndexAndSizeM(f).map(_._1)

    def logFoldMapM[G[_]: Monad, Msg, B: Monoid](logLevel: LogLevel, prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = {
      val sizeHintOpt = Option(sizeHint).filter(_ >= 0)

      logger.wrapProgressOuter(prefix, logLevel)(
        fa.foldMapWithIndexAndSizeM((a, i) =>
          logger.wrapProgressInner(prefix, logLevel, sizeHintOpt, i)(f(a))
        ).flatMap { case (b, i) =>
            logger.progressEnd(prefix, logLevel, sizeHintOpt, i).as(b)
        }
      )
    }
    def debugFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Debug, prefix, sizeHint)(f)
    def traceFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Trace, prefix, sizeHint)(f)
    def infoFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Info, prefix, sizeHint)(f)
    def warnFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Warn, prefix, sizeHint)(f)
    def errorFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Error, prefix, sizeHint)(f)

    def debugBarFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Debug, prefix, fa.size)(f)
    def traceBarFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Trace, prefix, fa.size)(f)
    def infoBarFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Info, prefix, fa.size)(f)
    def warnBarFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Warn, prefix, fa.size)(f)
    def errorBarFoldMapM[G[_]: Monad, Msg, B: Monoid](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[B] = logFoldMapM(LogLevel.Error, prefix, fa.size)(f)
  }

  implicit class FreelogCatsTraverseOps[F[_]: Traverse, A](val fa: F[A]) {
    def traverseWithIndexAndSizeM[G[_], B](f: (A, Int) => G[B])(implicit G: Monad[G]): G[(F[B], Int)] =
      fa.traverse(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b))))
        .run(0).map(_.swap)

    def blockTraverse[G[_]: Applicative, Msg, B](f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg]
    ) = logger.block(fa.traverse(a => f(a) <* logger.rewind))

    // TODO add  blockTraverseWithIndexM

    // def blockTraverseWithIndexM[G[_]: Traverse, A, B](fa: G[A])(f: (A, Int) => F[B])(
    //   implicit ap: Monad[F]
    // ) =
    //   block(fa.traverseWithIndexM((a, i) => f(a, i) <* rewind))

    def logTraverse[G[_]: Monad, Msg, B](logLevel: LogLevel, prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = {
      val sizeHintOpt = Option(sizeHint).filter(_ >= 0)
      logger.wrapProgressOuter(prefix, logLevel)(
        fa.traverseWithIndexAndSizeM((a, i) =>
          logger.wrapProgressInner(prefix, logLevel, sizeHintOpt, i)(f(a))
        ).flatMap { case (b, i) =>
            logger.progressEnd(prefix, logLevel, sizeHintOpt, i).as(b)
        }
      )
    }
    def debugTraverse[G[_]: Monad, Msg, B](logLevel: LogLevel, prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Debug, prefix, sizeHint)(f)
    def traceTraverse[G[_]: Monad, Msg, B](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Trace, prefix, sizeHint)(f)
    def infoTraverse[G[_]: Monad, Msg, B](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Info, prefix, sizeHint)(f)
    def warnTraverse[G[_]: Monad, Msg, B](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Warn, prefix, sizeHint)(f)
    def errorTraverse[G[_]: Monad, Msg, B](prefix: Msg, sizeHint: Long = -1)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Error, prefix, sizeHint)(f)

    def logBarTraverse[G[_]: Monad, Msg, B](logLevel: LogLevel, prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(logLevel, prefix, fa.size)(f)
    def debugBarTraverse[G[_]: Monad, Msg, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Debug, prefix, fa.size)(f)
    def traceBarTraverse[G[_]: Monad, Msg, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Trace, prefix, fa.size)(f)
    def infoBarTraverse[G[_]: Monad, Msg, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Info, prefix, fa.size)(f)
    def warnBarTraverse[G[_]: Monad, Msg, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Warn, prefix, fa.size)(f)
    def errorBarTraverse[G[_]: Monad, Msg, B](prefix: Msg)(f: A => G[B])(
      implicit logger: EphemeralLogger[G, Msg], ambientLevel: LogLevel
    ): G[F[B]] = logTraverse(LogLevel.Error, prefix, fa.size)(f)

    // TODO withIndex variants
  }
}
