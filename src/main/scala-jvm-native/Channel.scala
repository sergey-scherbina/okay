package okay

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.annotation.tailrec

/**
 * A channel: a queue between fibers, the missing primitive of
 * CONCURRENT streams — everything the pull-based observation cannot
 * say (readiness, pacing) lives here. send parks the producer when
 * the buffer is full, receive parks the consumer when it is empty (on
 * Loom parking is free), close ends the stream after the buffered
 * elements drain. merge and buffer below are derived.
 *
 * A channel is itself a Stream (in Async), but a LINEAR one: the
 * observation consumes — a repeated uncons reads the NEXT element,
 * not the same one. Bridge to LazyList (toLazyList memoizes) for a
 * re-observable view.
 */
final class Channel[A](capacity: Int = Int.MaxValue) {

  private val q = LinkedBlockingQueue[A](capacity)
  @volatile private var open = true
  @volatile private var failure: Throwable | Null = null

  /** park until there is room, then enqueue; do not send after close */
  def send(a: A): Unit = q.put(a)

  /** end the stream: the buffered elements still drain */
  def close(): Unit = open = false

  /**
   * Record that a producer broke. It does NOT close: in a merge the
   * other source is still feeding, and one side failing is no reason
   * to cut the side that is fine. `close` still ends the stream, and
   * the error is what the END then is — so a consumer receives
   * everything that was actually produced and only then hears that
   * something went wrong.
   *
   * Without this a producer that failed halfway was indistinguishable
   * from one that finished: the exception died on the producer's own
   * fiber, `finally` closed the channel, and the consumer read a
   * perfectly ordinary end of stream. A merge quietly returning half
   * its elements looks exactly like a merge that legitimately had
   * that many, which is the worst way for a stream to be wrong.
   */
  def fail(e: Throwable): Unit =
    if failure == null then failure = e

  /** what ended the stream, if anything did badly */
  def failed: Option[Throwable] = Option(failure)

  /** park until an element arrives, or None — closed and drained.
   * The closed check comes BEFORE the timed poll, so the end of a
   * drained channel is immediate; the timeout only bounds how long a
   * concurrent close can go unnoticed while the queue idles. */
  @tailrec def receive(): Option[A] =
    val a = q.poll()
    if a != null then Some(a)
    else if !open then
      val b = q.poll()
      // the buffered elements drain first; the failure is what the
      // END of the stream is, so it surfaces only once they are gone
      if b != null then Some(b)
      else if failure != null then throw failure.nn
      else None
    else
      val b = q.poll(10, TimeUnit.MILLISECONDS)
      if b != null then Some(b) else receive()
}

/** a channel is an async stream of what it receives (linear: see above) */
given Stream[Channel, Async] with
  def uncons[A](c: Channel[A]): Option[(A, Channel[A])] ! Async =
    async(c.receive().map((_, c)))

object Channel {

  /**
   * Merge two streams by READINESS, not by turns: a fiber per source
   * feeds one channel, the loser of every race simply arrives later.
   * This is the concurrency zip and ++ cannot express — they are
   * strictly sequential. The channel closes when both sources end.
   */
  def merge[A, S[_], F[+_], T[_], G[+_]](s: S[A], t: T[A], capacity: Int = Int.MaxValue)
                                        (using Stream[S, F], Handler[F], Stream[T, G], Handler[G])
                                        (using sch: Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    val alive = AtomicInteger(2)
    inline def feed[U[_], H[+_]](u: U[A])(using St: Stream[U, H], HH: Handler[H]): Unit =
      try
        @tailrec def go(x: U[A]): Unit = St.uncons(x).runWith match
          case Some((a, t)) => c.send(a); go(t)
          case None => ()
        go(u)
      catch case e: Throwable => c.fail(e)
      finally if alive.decrementAndGet() == 0 then c.close()
    sch.fork(() => async(feed(s))): Unit
    sch.fork(() => async(feed(t))): Unit
    c

  /**
   * Run the producer ahead of the consumer, at most capacity elements
   * ahead: a fiber unfolds the stream into a bounded channel — send
   * parks the producer when the consumer lags by capacity.
   */
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

/** merge two chunked streams by readiness: the existing Channel.merge,
 * one queue operation per chunk (type args spelled out — inference
 * abstracts the wrong slot through the nested alias) */
def mergeChunks[A](s: Chunks[A], t: Chunks[A], capacity: Int = Int.MaxValue)
                  (using Scheduler): Channel[Chunk[A]] =
  Channel.merge[Chunk[A], Producer, Pure, Producer, Pure](s, t, capacity)
