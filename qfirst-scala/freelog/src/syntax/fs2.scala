package freelog
package syntax

import _root_.cats.Monoid
import _root_.cats.effect.Sync
import _root_.cats.effect.concurrent.Ref
import _root_.cats.implicits._

import _root_.fs2.Stream
import _root_.fs2.Stream.CompileOps

object fs2 extends Fs2Syntax

trait Fs2Syntax {
  implicit class FreelogFs2StreamOps[F[_], O](val stream: Stream[F, O]) {
    // This doesn't use exactly the normal API for logging progress,
    // because we don't have explicit control over the evaluation order of effects in the stream.
    // In particular, if logging events happen during stream processing, we have no way of
    // capturing those events inside a branch. So instead we have to
    // 1) rewind _before_ logging the progress indicator so we don't insta-rewind our progress bar
    // 2) explicitly log the progress bar instead of using the logger's native inner-wrapper function
    //    since there's no effectful value to wrap.
    def logCompile[Msg, A](
      logLevel: LogLevel, prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = {
      val sizeHintOpt = Option(sizeHint).filter(_ >= 0)
      val innerPrefix = Option(prefix).filter(_ => logger.wrapProgressInnerUsesPrefix)
      logger.getLoggableLineLength.flatMap { lineLength =>
        logger.wrapProgressOuter(prefix, logLevel)(
          Ref[F].of(0) >>= { index =>
            val compiledStream = stream.evalTap { a =>
              index.get.flatMap { i =>
                logger.rewind >>
                  logger.logProgress(innerPrefix, sizeHintOpt, logLevel, i) >>
                  index.update(_ + 1)
              }
            }.compile

            run(compiledStream).flatTap { _ =>
              index.get >>= { i =>
                logger.rewind >> logger.progressEnd(prefix, logLevel, sizeHintOpt, i)
              }
            }
          }
        )
      }
    }
    def debugCompile[Msg, A](
      prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = logCompile(LogLevel.Debug, prefix, sizeHint)(run)
    def traceCompile[Msg, A](
      prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = logCompile(LogLevel.Trace, prefix, sizeHint)(run)
    def infoCompile[Msg, A](
      prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = logCompile(LogLevel.Info, prefix, sizeHint)(run)
    def warnCompile[Msg, A](
      prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = logCompile(LogLevel.Warn, prefix, sizeHint)(run)
    def errorCompile[Msg, A](
      prefix: Msg, sizeHint: Long = -1)(
      run: CompileOps[F, F, O] => F[A])(
      implicit logger: EphemeralLogger[F, Msg], F: Sync[F], ambientLevel: LogLevel
    ): F[A] = logCompile(LogLevel.Error, prefix, sizeHint)(run)
  }
}
