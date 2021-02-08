package freelog
package loggers

// import scala.concurrent.duFiniteDuration
import scala.concurrent.duration._

import cats.Monad
import cats.implicits._
import cats.data.NonEmptyList

import cats.effect.concurrent.Ref
import cats.effect.Concurrent
import cats.effect.Timer
import cats.effect.implicits._
import cats.Applicative

object Debounced {
  // TODO this probably could be generalized and given some more subtypes
  // to use as a general logging interface (ie LogCommand[Msg] => F[Unit]).
  sealed trait LogCommand[+Msg]
  case class EmitProgress[Msg](
    prefix: Option[Msg],
    sizeHint: Option[Long],
    logLevel: LogLevel,
    current: Long,
    progressSpec: ProgressSpec[Msg]
  ) extends LogCommand[Msg]
  case class Emit[Msg](msg: Msg, logLevel: LogLevel) extends LogCommand[Msg]
  case object BeginBlock extends LogCommand[Nothing]
  case object EndBlock extends LogCommand[Nothing]
  case class BeginBranch[Msg](msg: Msg, logLevel: LogLevel) extends LogCommand[Msg]
  case class EndBranch(logLevel: LogLevel) extends LogCommand[Nothing]
  case object Rewind extends LogCommand[Nothing]
  case object Flush extends LogCommand[Nothing]
}

// messageQueue itself being Nil indicates that we're not waiting.
case class Debounced[F[_]: Concurrent : Timer, Msg](
  messageQueue: Ref[F, List[List[Debounced.LogCommand[Msg]]]],
  innerLogger: SequentialEphemeralTreeLogger[F, Msg],
  debounceTime: FiniteDuration
) extends SequentialEphemeralTreeLogger[F, Msg] {

  // TODO: maybe use a semaphore to ensure single-user access to stuff? meh

  val F: Monad[F] = implicitly[Monad[F]]

  import Debounced._

  // TODO make this a parameter to support things like remote loggers
  private[this] def shouldDebounce(command: LogCommand[Msg]): Boolean = command match {
    case Rewind => true
    case _ => false
  }

  private[this] def hardRun(command: LogCommand[Msg]): F[Unit] = command match {
    case Emit(msg, logLevel) => innerLogger.emit(msg, logLevel)
    case EmitProgress(prefix, sizeHint, logLevel, current, progressSpec) =>
      innerLogger.emitProgress(prefix, sizeHint, logLevel, current)(progressSpec)
    case Rewind => innerLogger.rewind
    case Flush => innerLogger.flush
    case BeginBlock => innerLogger.beginBlock
    case EndBlock => innerLogger.endBlock
    case BeginBranch(msg, logLevel) => innerLogger.beginBranch(msg, logLevel)
    case EndBranch(logLevel) => innerLogger.endBranch(logLevel)
  }

  // TODO could perhaps improve efficiency by detecting and removing redundant rewinds.
  // probably only worth doing once there's a performance benchmark.
  private[this] def isPreservedInRewind(command: LogCommand[Msg]) = command match {
    case Rewind => true
    case EndBlock => true
    case BeginBranch(_, _) => true
    case EndBranch(_) => true
    case _ => false
  }

  // given that we're in a debounce, this defines how we modify the queue
  private[this] def enqueue(
    command: LogCommand[Msg],
    curQueue: NonEmptyList[List[LogCommand[Msg]]]
  ): List[List[LogCommand[Msg]]] = command match {
    case Rewind => curQueue.tail match {
      case Nil => // we're queued in the outermost block
        // clear all pending non-structural commands and queue a rewind for any that were already sent.
        // This filtering step is where we get the jitter & performance improvements from debouncing.
        (Rewind :: curQueue.head.filter(isPreservedInRewind)) :: Nil
      case outerBlocks => // we're not in the outermost block
        Nil :: outerBlocks // in this case we can safely delete everything in the current block
    }
    case BeginBlock => Nil :: curQueue.toList // increase nesting level
    case EndBlock => curQueue.tail match {
      case Nil => // we're in the outermost block
        (EndBlock :: curQueue.head) :: Nil  // we need to relay the EndBlock message via the queue since we can't end it ourselves
      case containingBlock :: outerBlocks => // there's a queued begin-block boundary
        (curQueue.head ++ containingBlock) :: outerBlocks
    }
    // naively enqueue the rest: Emit, Flush, BeginBranch, EndBranch
    case msg => (msg :: curQueue.head) :: curQueue.tail
  }

  // run a command, either dispatching directly to the logger (triggering a debounce if necessary)
  // or queueing the operation (if we're currently in a debounce).
  private[this] def run(command: LogCommand[Msg]): F[Unit] = messageQueue.modify {
    case Nil => Nil -> true
    case head :: tail => enqueue(command, NonEmptyList(head, tail)) -> false
  }.ifM(
    (if(shouldDebounce(command)) debounce else F.unit) >> hardRun(command),
    F.unit
  )

  def isDebouncing = messageQueue.get.map(_.nonEmpty)

  // don't layer debounces.
  def debounce: F[Unit] = isDebouncing.ifM(F.unit, hardDebounce.start.void)

  private[this] def hardDebounce: F[Unit] = messageQueue
    .update(q => if(q.isEmpty) List(Nil) else q) >> (
      Timer[F].sleep(debounceTime) *> flushQueue
    )

  // sends all queued commands to the logger / potentially ends a debounce.
  private[this] def flushQueue: F[Unit] =
    messageQueue.getAndSet(List(Nil)) >>= { messages =>
      // use the nesting structure of the message queue to insert begin-blocks at the correct locations.
      val orderedCommands = messages.reverse match {
        case Nil => Nil
        case outermostBlock :: innerBlocks =>
          outermostBlock.reverse ++ innerBlocks.map(BeginBlock :: _.reverse).flatten
      }
      if(orderedCommands.nonEmpty) {
        orderedCommands.traverse(this.hardRun) >> hardDebounce // we just delivered messages, so keep debouncing
      } else messageQueue.tryModify { interlopingMessages =>
        if(interlopingMessages == List(Nil)) Nil -> true // no new messages arrived since the flush; we're done debouncing
        else interlopingMessages -> false // new messages have arrived since flush; we're not done debouncing
      }.map(_.exists(identity)).ifM( // if the tryUpdate above succeeded AND returned true...
        F.unit, // then we're done debouncing.
        hardDebounce  // otherwise, keep debouncing.
      )
    }

  def emit(msg: Msg, logLevel: LogLevel) = run(Emit(msg, logLevel))

  def emitProgress(
    prefix: Option[Msg],
    sizeHint: Option[Long],
    logLevel: LogLevel,
    current: Long)(
    implicit progress: ProgressSpec[Msg]
  ): F[Unit] = run(EmitProgress(prefix, sizeHint, logLevel, current, progress))

  def rewind: F[Unit] = run(Rewind)

  def flush: F[Unit] = run(Flush)

  def beginBlock: F[Unit] =  run(BeginBlock)

  def endBlock: F[Unit] = run(EndBlock)

  def beginBranch(msg: Msg, logLevel: LogLevel): F[Unit] = run(BeginBranch(msg, logLevel))

  def endBranch(logLevel: LogLevel): F[Unit] = run(EndBranch(logLevel))
}
