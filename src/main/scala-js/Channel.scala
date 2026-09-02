package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.{tailrec, unused}
import scala.collection.mutable

/**
 * The Await-based channel (specs/cross-platform-async.md): the same
 * surface as the JVM one with no parking anywhere — a receive that
 * finds the buffer empty leaves a callback, send hands the element
 * straight to the first waiter. On the single JS thread nothing
 * races. Capacity is advisory: a JS sender cannot park, so it never
 * waits — bound producers by demand (the pull side), not by buffer
 * pressure.
 */
final class Channel[A](@unused capacity: Int = Int.MaxValue) {

  private val q = mutable.Queue[A]()
  private val waiters = mutable.Queue[Either[Throwable, Option[A]] => Unit]()
  private var open = true
  private var failure: Throwable | Null = null

  /** enqueue, or hand straight to a waiting receiver. False once
   * closed: the element is dropped, nothing thrown — the JVM
   * channel's contract, trivially exact on one thread */
  def send(a: A): Boolean =
    if !open then false
    else
      if waiters.nonEmpty then waiters.dequeue()(Right(Some(a)))
      else q.enqueue(a)
      true

  /** end the stream: the buffered elements still drain */
  def close(): Unit =
    open = false
    val end: Either[Throwable, Option[A]] =
      if failure != null then Left(failure.nn) else Right(None)
    while waiters.nonEmpty && q.isEmpty do waiters.dequeue()(end)

  /**
   * Record that a producer broke. It does NOT close — in a merge the
   * other source is still feeding — and `close` then ends the stream
   * with this error rather than cleanly. See the JVM channel for the
   * whole argument.
   */
  def fail(e: Throwable): Unit =
    if failure == null then failure = e

  /** what ended the stream, if anything did badly */
  def failed: Option[Throwable] = Option(failure)

  /** the callback form of receive: now if an element (or the end) is
   * ready, later when one arrives */
  def receiveAsync(k: Either[Throwable, Option[A]] => Unit): Unit =
    if q.nonEmpty then k(Right(Some(q.dequeue())))
    else if !open then
      // buffered elements drain first; the failure IS the end
      if failure != null then k(Left(failure.nn)) else k(Right(None))
    else waiters.enqueue(k)
}

/** a channel is an async stream of what it receives (linear): the
 * uncons is an Await, served by the event loop */
given Stream[Channel, Async] with
  def uncons[A](c: Channel[A]): Option[(A, Channel[A])] ! Async =
    // the Left is the row's error channel, which is exactly where a
    // producer's failure belongs
    Async.await(k => { c.receiveAsync(r => k(r.map(_.map((_, c))))); () => () })

object Channel {

  /** merge two streams into one channel; the sources feed on their
   * own (event-loop) fibers, the channel closes when both end */
  def merge[A, S[_], F[+_], T[_], G[+_]](s: S[A], t: T[A], capacity: Int = Int.MaxValue)
                                        (using Stream[S, F], Handler[F], Stream[T, G], Handler[G])
                                        (using sch: Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    val alive = AtomicInteger(2)
    inline def feed[U[_], H[+_]](u: U[A])(using St: Stream[U, H], HH: Handler[H]): Unit =
      try
        @tailrec def go(x: U[A]): Unit = St.uncons(x).runWith match
          case Some((a, r)) => c.send(a): Unit; go(r)
          case None => ()
        go(u)
      catch case e: Throwable => c.fail(e)
      finally if alive.decrementAndGet() == 0 then c.close()
    sch.fork(() => async(feed(s))): Unit
    sch.fork(() => async(feed(t))): Unit
    c

  /** unfold the stream into the channel on its own fiber */
  def buffer[A, S[_], F[+_]](capacity: Int)(s: S[A])
                            (using Stream[S, F], Handler[F], Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    val _ = summon[Scheduler].fork: () =>
      async:
        try s.toLazyList.foreach(c.send)
        catch case e: Throwable => c.fail(e)
        finally c.close()
    c
}
