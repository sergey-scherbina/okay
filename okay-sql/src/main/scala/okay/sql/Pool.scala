package okay.sql

import okay.{!, +, Async, Resource, Scheduler, TRef, Timer, pure}
import java.util.concurrent.atomic.AtomicBoolean

/**
 * A fixed-size pool of connections (specs/sql.md, "The pool"): one
 * `Sql` is one connection and one thread of control, so a service
 * with more than one request in flight borrows from here. Driver-
 * neutral — the pg wire has no pool of its own, JDBC's Hikari is one
 * `open` away (`() => JdbcSql(dataSource.getConnection)` and a close
 * that hands the connection back) — and every platform: the state is
 * one `TRef` cell modified atomically, waiters are callbacks, and the
 * hand-off is cancel-safe (a waiter whose timeout fired never keeps
 * the connection it was just given; it goes to the next waiter).
 *
 * A returned connection goes through the BRAKE (`Sql.cancel`) first:
 * an open transaction — a raw `begin` the borrower never closed —
 * rolls back, and a no-op costs nothing. That is the health probe
 * this stack already has, not a new one.
 *
 * Failure to open counts against nobody: the slot frees, the opener
 * sees the error, queued waiters keep waiting for a return within
 * their own timeout.
 */
final class Pool[C <: Sql] private (open: () => C ! Async, dispose: C => Unit,
                                    val size: Int, val acquireTimeoutMillis: Long)
                                   (using Scheduler, Timer):
  import Pool.*

  private final class Waiter:
    val taken = AtomicBoolean(false)
    @volatile var answer: Either[Throwable, C] => Unit = _ => ()

  private final case class St(idle: List[C], busy: Int, waiters: Vector[Waiter],
                              closed: Boolean, created: Long)

  private val state = TRef(St(Nil, 0, Vector.empty, false, 0L))

  /** a program on one connection, returned to the pool after it —
   * value or failure — through the brake */
  def borrow[A](use: C => A ! (Resource + Async)): A ! Async =
    acquire.flatMap { c =>
      Async.attempt(Resource.run[A, Async](use(c))).flatMap { r =>
        release(c)
        r.fold(t => throw t, pure)
      }
    }

  /** a connection pinned for the enclosing Resource scope; the
   * scope's end returns it */
  def pinned: C ! (Resource + Async) =
    !.widen[C, Async, Resource](acquire).flatMap(c =>
      !.widen[C, Resource, Async](Resource.acquire(c)(release)))

  def stats: Stats =
    val st = state.get
    Stats(size, st.idle.length, st.busy, st.waiters.length, st.created, st.closed)

  /** closes the idle connections now and every busy one as it
   * returns; waiters fail with `Closed`, later borrows refuse */
  def close(): Unit =
    val (idle, waiters) = state.modify(st => (st.copy(idle = Nil, waiters = Vector.empty, closed = true), (st.idle, st.waiters)))
    idle.foreach(dispose)
    waiters.foreach(w => if w.taken.compareAndSet(false, true) then w.answer(Left(Closed())))

  /** the grab's own failure (`Closed`, a failed open) is a VALUE so
   * the timeout race sees it as an answer — a failing contender never
   * wins a race, and a Closed pool must not report Exhausted */
  private def acquire: C ! Async =
    val w = Waiter()
    Async.timeout(acquireTimeoutMillis)(grab(w)).map {
      case None =>
        // the timeout: withdraw here as well as in the canceller — both
        // idempotent on the one CAS, so whichever runs first settles it
        withdraw(w)
        throw Exhausted(size, acquireTimeoutMillis)
      case Some(Left(e)) => throw e
      case Some(Right(c)) => c
    }

  private def withdraw(w: Waiter): Unit =
    if w.taken.compareAndSet(false, true) then
      state.modify(st => (st.copy(waiters = st.waiters.filterNot(_ eq w)), ()))

  private enum Act:
    case Give(c: C)
    case Open
    case Wait
    case Refuse

  private def grab(w: Waiter): Either[Throwable, C] ! Async =
    Async.await[Either[Throwable, C]] { k0 =>
      val k: Either[Throwable, C] => Unit = r => k0(Right(r))
      w.answer = k
      val act = state.modify { st =>
        if st.closed then (st, Act.Refuse)
        else st.idle match
          case c :: rest => (st.copy(idle = rest, busy = st.busy + 1), Act.Give(c))
          case Nil if st.busy < size => (st.copy(busy = st.busy + 1, created = st.created + 1), Act.Open)
          case Nil => (st.copy(waiters = st.waiters :+ w), Act.Wait)
      }
      act match
        case Act.Give(c) => k(Right(c))
        case Act.Refuse => k(Left(Closed()))
        case Act.Wait => ()
        case Act.Open =>
          w.taken.set(true)   // not queued: nothing to withdraw
          Async.spawn(open()).onComplete {
            case Right(c) => k(Right(c))
            case Left(e) =>
              state.modify(st => (st.copy(busy = st.busy - 1), ()))
              k(Left(e))
          }
      // the canceller (a timeout fired): leave the queue; a grant that
      // races this loses the CAS and passes the connection on
      () => withdraw(w)
    }

  private def release(c: C): Unit =
    c.cancel()
    val (next, disposeIt) = state.modify { st =>
      if st.closed then (st.copy(busy = st.busy - 1), (None, true))
      else st.waiters match
        case w +: rest => (st.copy(waiters = rest), (Some(w), false))
        case _ => (st.copy(idle = c :: st.idle, busy = st.busy - 1), (None, false))
    }
    next match
      case Some(w) => if w.taken.compareAndSet(false, true) then w.answer(Right(c)) else release(c)
      case None => if disposeIt then dispose(c)

object Pool:

  /** `size` connections at most, opened on demand; a borrow waits up
   * to `acquireTimeoutMillis` for one and then fails with `Exhausted` */
  def apply[C <: Sql](size: Int, acquireTimeoutMillis: Long = 5000L)
                     (open: () => C ! Async)(close: C => Unit)
                     (using Scheduler, Timer): Pool[C] =
    require(size >= 1, "a pool holds at least one connection")
    new Pool[C](open, close, size, acquireTimeoutMillis)

  final case class Stats(size: Int, idle: Int, busy: Int, waiting: Int, created: Long, closed: Boolean)

  final case class Exhausted(size: Int, waitedMillis: Long)
    extends RuntimeException(s"pool exhausted: $size connections busy for $waitedMillis ms")

  final case class Closed() extends RuntimeException("pool closed")
