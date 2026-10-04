package okay2.persist

import okay2.{!, pure}
import okay2.async.{Async, Scheduler}
import okay2.codec.{Codecs, Schema}

/**
 * A SAGA OVER THE LOG (okay-persist's Saga.scala; Garcia-Molina and
 * Salem, "Sagas", SIGMOD 1987): steps with compensations, every move
 * journalled BEFORE it is made (`Intent`) and after it is done, so a
 * process that dies anywhere recovers to the same decision. A failed
 * step undoes the done ones backwards; a failed COMPENSATION is `Stuck`
 * — a human's matter, said loudly. `recover` finishes forward or
 * backward by the declared `Policy` when it finds an intent with no
 * outcome. One saga = one key = one partition.
 */
final class Saga[S](topic: Topic, val id: String, steps: Vector[Saga.Step[S]], policy: Saga.Policy)
                   (implicit ss: Schema[S], sch: Scheduler) {
  import Saga._

  require(steps.nonEmpty, "a saga has at least one step")

  private val typed = Typed[Rec](topic, version = 1, upcasts = Map.empty)
  private val codec = Codecs.cbor(ss)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  private def put(r: Rec): Unit = {
    val _ = typed.append(partition, key, r, Ack.Durable)
  }
  private def bytes(s: S): Array[Byte] = codec.encode(s)
  private def stateOf(b: Array[Byte]): S =
    codec.decode(b).fold(e => throw new IllegalStateException(s"saga $id: a journaled state does not decode: $e"), identity)

  /** start from `init` and run forward */
  def run(init: S): Outcome[S] ! Async = {
    put(Rec.Started(bytes(init)))
    forward(0, init)
  }

  /** carry on from the journal after a crash */
  def recover(): Outcome[S] ! Async = {
    val f = fold()
    f.ended match {
      case Some(o) => pure[Async, Outcome[S]](o)
      case None =>
        if (f.started.isEmpty) throw new IllegalStateException(s"saga $id: no such saga")
        f.failed match {
          case Some((i, err)) => backward(f.undoFrom.getOrElse(i - 1), f.stateNow, i, err)
          case None => policy match {
            case Policy.Forward => forward(f.pendingIntent.getOrElse(f.doneUpTo + 1), f.stateNow)
            case Policy.Backward => backward(f.pendingIntent.getOrElse(f.doneUpTo), f.stateNow, -1, "recovered backward by policy")
          }
        }
    }
  }

  /** where it stands, from the journal alone */
  def status: Status = {
    val f = fold()
    val phase = f.ended match {
      case Some(_: Outcome.Finished[_]) => "finished"
      case Some(_: Outcome.Aborted[_]) => "aborted"
      case None if f.started.isEmpty => "unknown"
      case None if f.failed.isDefined => "compensating"
      case None => "running"
    }
    Status(id, phase, f.done.map(steps(_).name), f.undone.map(steps(_).name),
      f.pendingIntent.map(steps(_).name), f.failed.map(_._2))
  }

  // trampolined: each next step is deferred into the program's flatMap
  private def forward(i: Int, s: S): Outcome[S] ! Async =
    if (i >= steps.length) {
      put(Rec.Finished(bytes(s)))
      pure[Async, Outcome[S]](Outcome.Finished(s))
    } else {
      put(Rec.Intent(i))
      Async.attempt(steps(i).forward(s)).flatMap {
        case Right(s2) =>
          put(Rec.Done(i, bytes(s2)))
          forward(i + 1, s2)
        case Left(h: Halt) => throw h   // the process dies here: the intent stands, nothing else is written
        case Left(e) =>
          val err = Option(e.getMessage).getOrElse(e.getClass.getName)
          put(Rec.Failed(i, err))
          backward(i - 1, s, i, err)
      }
    }

  private def backward(j: Int, s: S, failedStep: Int, err: String): Outcome[S] ! Async =
    if (j < 0) {
      put(Rec.Aborted(bytes(s), failedStep, err))
      pure[Async, Outcome[S]](Outcome.Aborted(s, if (failedStep >= 0) Some(steps(failedStep).name) else None, err))
    } else {
      put(Rec.Undoing(j))
      Async.attempt(steps(j).compensate(s)).flatMap {
        case Right(s2) =>
          put(Rec.Undone(j, bytes(s2)))
          backward(j - 1, s2, failedStep, err)
        case Left(h: Halt) => throw h
        case Left(e) => throw new Stuck(id, steps(j).name, Option(e.getMessage).getOrElse(e.getClass.getName), e)
      }
    }

  private final class Fold {
    var started: Option[S] = None
    // set by Started before any read; the fold never reads it unless
    // `started` is defined
    var state: Option[S] = None
    var doneUpTo: Int = -1
    var done: Vector[Int] = Vector.empty
    var undone: Vector[Int] = Vector.empty
    var pendingIntent: Option[Int] = None
    var undoFrom: Option[Int] = None      // the compensation to (re)do next
    var failed: Option[(Int, String)] = None
    var ended: Option[Outcome[S]] = None
    def stateNow: S = state.getOrElse(throw new IllegalStateException(s"saga $id: no state before Started"))
  }

  private def fold(): Fold = {
    val f = new Fold
    var from = topic.begin(partition)
    var going = true
    while (going) {
      typed.read(partition, from, 256) match {
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else
            for (d <- rs if going) d match {
              case Typed.Decoded.Ok(off, _, k, rec) =>
                if (k.sameElements(key)) applyRec(f, rec)
                from = off + 1
              case Typed.Decoded.Bad(off, _) =>
                going = false
                from = off
            }
      }
    }
    f
  }

  private def applyRec(f: Fold, rec: Rec): Unit = rec match {
    case Rec.Started(b) =>
      val s = stateOf(b); f.started = Some(s); f.state = Some(s)
    case Rec.Intent(i) => f.pendingIntent = Some(i)
    case Rec.Done(i, b) =>
      f.pendingIntent = None; f.doneUpTo = i; f.done :+= i; f.state = Some(stateOf(b))
    case Rec.Failed(i, err) =>
      f.pendingIntent = None; f.failed = Some((i, err)); f.undoFrom = Some(i - 1)
    case Rec.Undoing(j) => f.undoFrom = Some(j)
    case Rec.Undone(j, b) =>
      f.undone :+= j; f.undoFrom = Some(j - 1); f.state = Some(stateOf(b))
    case Rec.Finished(b) => f.ended = Some(Outcome.Finished(stateOf(b)))
    case Rec.Aborted(b, i, err) =>
      f.ended = Some(Outcome.Aborted(stateOf(b), if (i >= 0) Some(steps(i).name) else None, err))
  }
}

object Saga {

  sealed trait Policy
  object Policy {
    /** an intent with no outcome is re-run */
    case object Forward extends Policy
    /** an intent with no outcome is compensated */
    case object Backward extends Policy
  }

  final case class Step[S](name: String, forward: S => S ! Async, compensate: S => S ! Async)

  sealed trait Outcome[S]
  object Outcome {
    final case class Finished[S](state: S) extends Outcome[S]
    final case class Aborted[S](state: S, failedStep: Option[String], error: String) extends Outcome[S]
  }

  final case class Status(id: String, phase: String, done: Vector[String], undone: Vector[String],
                          pending: Option[String], error: Option[String])
  implicit lazy val statusSchema: Schema[Status] = Schema.derived

  sealed trait Rec
  object Rec {
    final case class Started(state: Array[Byte]) extends Rec
    final case class Intent(step: Int) extends Rec
    final case class Done(step: Int, state: Array[Byte]) extends Rec
    final case class Failed(step: Int, error: String) extends Rec
    final case class Undoing(step: Int) extends Rec
    final case class Undone(step: Int, state: Array[Byte]) extends Rec
    final case class Finished(state: Array[Byte]) extends Rec
    final case class Aborted(state: Array[Byte], step: Int, error: String) extends Rec
    implicit lazy val schema: Schema[Rec] = Schema.derived
  }

  /** a test's crash: thrown from a step, the process "dies" with the
   * intent written and nothing else */
  final class Halt extends RuntimeException("halt") with scala.util.control.NoStackTrace

  /** a compensation failed: the saga cannot undo itself */
  final class Stuck(val saga: String, val step: String, val error: String, cause: Throwable)
    extends RuntimeException(s"saga $saga: compensation of '$step' failed: $error", cause)

  def apply[S](topic: Topic, id: String, policy: Policy = Policy.Forward)(steps: Step[S]*)
              (implicit ss: Schema[S], sch: Scheduler): Saga[S] =
    new Saga[S](topic, id, steps.toVector, policy)
}
