package okay2.sql

import java.util.concurrent.atomic.AtomicBoolean
import okay2.{!, +, Resource, TRef, pure}
import okay2.async.{Async, Scheduler, Timer, asyncFailing}

/**
 * A bounded connection pool (okay-sql's Pool.scala): at most `size`
 * connections open, a borrower past that WAITS (parked in `Async`, not a
 * thread) and fails with `Exhausted` after `acquireTimeoutMillis`, and a
 * returned connection gets the brake (`cancel`) first, so a borrower's
 * open transaction never leaks into the next one. The state is one
 * `TRef`, every move a single `modify`.
 */
final class Pool[C <: Sql] private (open: () => C ! Async, dispose: C => Unit,
                                    val size: Int, val acquireTimeoutMillis: Long)
                                   (implicit S: Scheduler, T: Timer) {
  import Pool._

  private val state = TRef(St[C](Nil, 0, Vector.empty, closed = false, 0L))

  /** a connection for `use`, returned (with the brake) however it ends */
  def borrow[A](use: C => A ! (Resource + Async)): A ! Async =
    acquire.flatMap { c =>
      Async.attempt(Resource.run[A, Async](use(c))).flatMap { r =>
        release(c)
        r.fold(t => throw t, a => pure[Async, A](a))
      }
    }

  /** a connection held for the enclosing Resource scope */
  def pinned: C ! (Resource + Async) =
    acquire.flatMap(c => Resource.acquire(c)(release))

  def stats: Stats = {
    val st = state.get
    Stats(size, st.idle.length, st.busy, st.waiters.length, st.created, st.closed)
  }

  /** dispose the idle connections and refuse every waiter */
  def close(): Unit = {
    val (idle, waiters) = state.modify(st => (st.copy(idle = Nil, waiters = Vector.empty, closed = true), (st.idle, st.waiters)))
    idle.foreach(dispose)
    waiters.foreach(w => if (w.taken.compareAndSet(false, true)) w.answer(Left(Closed())))
  }

  private def acquire: C ! Async = {
    val w = new Waiter[C]
    Async.timeout(acquireTimeoutMillis)(grab(w)).map {
      case None =>
        withdraw(w)
        throw Exhausted(size, acquireTimeoutMillis)
      case Some(Left(e)) => throw e
      case Some(Right(c)) => c
    }
  }

  private def withdraw(w: Waiter[C]): Unit =
    if (w.taken.compareAndSet(false, true))
      state.modify(st => (st.copy(waiters = st.waiters.filterNot(_ eq w)), ()))

  private def grab(w: Waiter[C]): Either[Throwable, C] ! Async =
    Async.await[Either[Throwable, C]] { k0 =>
      val k: Either[Throwable, C] => Unit = r => k0(Right(r))
      w.answer = k
      val act: Act[C] = state.modify { st =>
        if (st.closed) (st, Refuse)
        else st.idle match {
          case c :: rest => (st.copy(idle = rest, busy = st.busy + 1), Give[C](c))
          case Nil if st.busy < size => (st.copy(busy = st.busy + 1, created = st.created + 1), OpenOne)
          case Nil => (st.copy(waiters = st.waiters :+ w), Wait)
        }
      }
      act match {
        case Give(c) => k(Right(c))
        case Refuse => k(Left(Closed()))
        case Wait => ()
        case OpenOne =>
          w.taken.set(true) // not queued: nothing to withdraw
          Async.spawn(open()).onComplete {
            case Right(c) => k(Right(c))
            case Left(e) =>
              state.modify(st => (st.copy(busy = st.busy - 1), ()))
              k(Left(e))
          }
      }
      () => withdraw(w)
    }

  private def release(c: C): Unit = {
    c.cancel()
    val (next, disposeIt) = state.modify { st =>
      if (st.closed) (st.copy(busy = st.busy - 1), (None, true))
      else st.waiters match {
        case w +: rest => (st.copy(waiters = rest), (Some(w), false))
        case _ => (st.copy(idle = c :: st.idle, busy = st.busy - 1), (None, false))
      }
    }
    next match {
      case Some(w) => if (w.taken.compareAndSet(false, true)) w.answer(Right(c)) else release(c)
      case None => if (disposeIt) dispose(c)
    }
  }
}

object Pool {

  // the pool's own state and moves, here rather than inside the class:
  // a case class nested in a class carries an outer reference its
  // pattern cannot check (a warning, an error under -Werror)
  private final class Waiter[C] {
    val taken = new AtomicBoolean(false)
    @volatile var answer: Either[Throwable, C] => Unit = _ => ()
  }

  private final case class St[C](idle: List[C], busy: Int, waiters: Vector[Waiter[C]], closed: Boolean, created: Long)

  private sealed trait Act[+C]
  private final case class Give[C](c: C) extends Act[C]
  private case object OpenOne extends Act[Nothing]
  private case object Wait extends Act[Nothing]
  private case object Refuse extends Act[Nothing]

  def apply[C <: Sql](size: Int, acquireTimeoutMillis: Long = 5000L)
                     (open: () => C ! Async)(close: C => Unit)
                     (implicit S: Scheduler, T: Timer): Pool[C] = {
    require(size >= 1, "a pool holds at least one connection")
    new Pool[C](open, close, size, acquireTimeoutMillis)
  }

  final case class Stats(size: Int, idle: Int, busy: Int, waiting: Int, created: Long, closed: Boolean)

  final case class Exhausted(size: Int, waitedMillis: Long)
    extends RuntimeException(s"pool exhausted: $size connections busy for $waitedMillis ms")

  final case class Closed() extends RuntimeException("pool closed")
}
