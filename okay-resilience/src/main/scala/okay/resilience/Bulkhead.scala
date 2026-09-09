package okay.resilience

import okay.*
import okay.codec.Schema
import java.util.concurrent.atomic.AtomicReference

object Bulkhead:
  final case class Stats(permits: Int, inFlight: Int, waiting: Int,
                         rejected: Long) derives Schema

  /** a parked caller: its status moves by CAS so a permit handed to a
    * caller that was cancelled in the meantime goes back, not lost */
  private enum Status:
    case Queued, Granted, Started, Released
  private final class Waiter(val wake: Waiter => Unit):
    val status = AtomicReference(Status.Queued)

  private final case class St(inFlight: Int, waiting: Vector[Waiter], rejected: Long)

/**
 * The bulkhead: at most `permits` programs in flight, at most `queue`
 * parked for one, the rest refused at once with `Refused.BulkheadFull`.
 * A permit is released when the program completes, fails, or is
 * cancelled while waiting; a cancel that lands between a grant and
 * the first step of the program hands the permit back too.
 */
final class Bulkhead(val name: String, permits: Int, queue: Int = 0)
  extends Reporting[Bulkhead.Stats]:
  import Bulkhead.*

  require(permits >= 1, "a bulkhead needs at least one permit")

  private val cell = TRef(St(0, Vector.empty, 0L))

  def stats: Stats =
    val s = cell.get
    Stats(permits, s.inFlight, s.waiting.size, s.rejected)

  /** run with a permit, park for one, or refuse */
  def limit[A](prog: => A ! Async): A ! Async =
    acquire.flatMap(_ => Attempt(prog)).map { r =>
      release()
      r.fold(t => throw t, identity)
    }

  /** the same permit over a ROW, held for the whole streaming call */
  def limitIn[A, F[+_]](prog: => A ! (F + Async))(using okay.TypeableK[Async]): A ! (F + Async) =
    !.widen[Unit, Async, F](acquire).flatMap(_ => Attempt.in[A, F](prog)).map { r =>
      release()
      r.fold(t => throw t, identity)
    }

  /** the answer is the waiter when there was one, so the first step
    * after the park can mark it Started — from then on the permit's
    * return is `limit`'s own */
  private def acquire: Unit ! Async =
    Async.await[Option[Waiter]] { k =>
      val outcome = cell.modify { s =>
        if s.inFlight < permits then (s.copy(inFlight = s.inFlight + 1), Right(()))
        else if s.waiting.size < queue then
          val made = Waiter(w => k(Right(Some(w))))
          (s.copy(waiting = s.waiting :+ made), Left(Some(made)))
        else (s.copy(rejected = s.rejected + 1), Left(None))
      }
      outcome match
        case Right(()) => k(Right(None)); () => ()
        case Left(None) => k(Left(Refused.BulkheadFull(name))); () => ()
        case Left(Some(w)) => () => cancelWaiter(w)
    }.flatMap { w =>
      okay.async(w.foreach(_.status.compareAndSet(Status.Granted, Status.Started)))
    }

  /** a cancel while parked: leave the queue; a cancel after the
    * grant but before the program's first step: give the permit back */
  private def cancelWaiter(w: Waiter): Unit =
    val removed = cell.modify { s =>
      val i = s.waiting.indexOf(w)
      if i >= 0 then (s.copy(waiting = s.waiting.patch(i, Nil, 1)), true) else (s, false)
    }
    if removed then w.status.set(Status.Released)
    else if w.status.compareAndSet(Status.Granted, Status.Released) then release()

  /** the permit moves to the first waiter, or goes back to the pool */
  private def release(): Unit =
    val next = cell.modify { s =>
      s.waiting match
        case w +: rest => (s.copy(waiting = rest), Some(w))
        case _ => (s.copy(inFlight = s.inFlight - 1), None)
    }
    next.foreach { w =>
      if w.status.compareAndSet(Status.Queued, Status.Granted) then w.wake(w)
      else release()   // cancelled between the dequeue and the grant: pass it on
    }
